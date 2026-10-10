# coder_run.R -------------------------------------------------------------------------
# Execution: protocols meet data. The entry points build an experiments tibble
# (config + messages, one row per unit x protocol) and hand it to a runner, by
# default LLMR::call_llm_par(), so tests can inject a fake runner and the whole
# layer stays offline-testable. The shared execution helper runs one table per
# replicate and reduces each protocol-unit group to its modal label.

# Internal: experiments tibble for a set of protocols over texts.
.build_experiments <- function(protocols, texts) {
  rows <- list()
  for (p in seq_along(protocols)) {
    pr <- protocols[[p]]
    for (i in seq_along(texts)) {
      rows[[length(rows) + 1L]] <- tibble::tibble(
        protocol_id = p,
        unit_id = i,
        # The raw unit text rides along as a metadata column so an injected
        # runner (a demo responder, say) can key on the unit rather than on
        # the full rendered prompt; LLMR::call_llm_par() passes it through.
        text = as.character(texts[[i]]),
        config = list(pr$config),
        messages = list(c(user = .render_prompt(pr, texts[[i]])))
      )
    }
  }
  do.call(rbind, rows)
}

# Internal: default runner. Returns the experiments tibble + at least
# `response_text` and `success` columns (call_llm_par's contract).
.default_runner <- function(experiments, ...) {
  LLMR::call_llm_par(experiments, ...)
}

# Internal: code each protocol-unit group with the protocol's replicate rule and
# return one row carrying its modal label and replicate details.
.run_protocols <- function(protocols, texts, .runner = NULL, ...) {
  runner <- .runner %||% .default_runner
  exps <- .build_experiments(protocols, texts)
  k <- vapply(protocols, `[[`, integer(1), "replicates")
  res <- lapply(seq_len(max(k)), function(r) {
    pass <- exps[exps$protocol_id %in% which(k >= r), , drop = FALSE]
    out <- runner(pass, ...)
    required <- c(names(pass), "response_text")
    if (!is.data.frame(out) || !all(required %in% names(out)) ||
        nrow(out) != nrow(pass)) {
      abort("`.runner` must return the experiment rows with a `response_text` column.")
    }
    expected_key <- paste(pass$protocol_id, pass$unit_id, sep = "\r")
    result_key <- paste(out$protocol_id, out$unit_id, sep = "\r")
    if (anyNA(out$protocol_id) || anyNA(out$unit_id) ||
        anyDuplicated(result_key) || !setequal(result_key, expected_key)) {
      abort("`.runner` must preserve experiment row identity.")
    }
    out <- out[match(expected_key, result_key), , drop = FALSE]
    if ("success" %in% names(out) && any(!out$success %in% TRUE)) {
      n_failed <- sum(!out$success %in% TRUE)
      abort(sprintf("`.runner` reported failure for %d experiment row(s).",
                    n_failed))
    }
    if (anyNA(out$response_text)) {
      abort(sprintf("`.runner` returned a missing response for %d experiment row(s).",
                    sum(is.na(out$response_text))))
    }
    out$replicate <- r
    out
  })
  res <- .bind_call_rows(res)
  labels_of <- function(p) codebook_labels(protocols[[p]]$codebook)
  res$label <- vapply(seq_len(nrow(res)), function(i) {
    pr <- protocols[[res$protocol_id[i]]]
    .parse_protocol_label(pr$parser, res$response_text[i] %||% NA_character_,
                          labels_of(res$protocol_id[i]))
  }, character(1))

  res$.call_tokens <- .call_tokens(res)
  rows <- list()
  for (p in seq_along(protocols)) {
    for (i in seq_along(texts)) {
      ri <- res[res$protocol_id == p & res$unit_id == i, , drop = FALSE]
      ri <- ri[order(ri$replicate), , drop = FALSE]
      labels <- ri$label
      counts <- table(labels[!is.na(labels)])
      # The modal label, under an explicit measurement rule: a tie among
      # replicates is disagreement, and disagreement is NA, never an
      # arbitrary pick by table ordering. label_share counts agreement over
      # ALL replicates, so parse failures lower it; label_share_parsed is
      # the share among parsed replies alone.
      top <- if (length(counts)) max(counts) else 0L
      tied <- sum(counts == top) > 1L
      modal <- if (!top || tied) NA_character_ else names(counts)[counts == top]
      row <- tibble::tibble(
        protocol_id = p,
        unit_id = i,
        label = modal,
        label_share = as.numeric(top) / length(labels),
        label_share_parsed = if (sum(counts)) as.numeric(top) / sum(counts)
                             else NA_real_,
        tied = tied,
        parse_failures = sum(is.na(labels)),
        replicate_labels = list(labels)
      )
      tok <- .token_sum(ri$.call_tokens)
      row$.calls <- tok$calls
      row$.tokens <- tok$tokens
      row$.tokens_unknown <- tok$unknown
      rows[[length(rows) + 1L]] <- row
    }
  }
  do.call(rbind, rows)
}

#' Compare coding protocols on a development split
#'
#' Applies candidate protocols to a gold-set split and compares their labels
#' with the gold labels. Each protocol uses its configured replicate count
#' and modal-label rule.
#'
#' @details
#' The gold set's declared holdout split cannot be used for tuning.
#'
#' A call's tokens are the runner's `total_tokens`, or `sent_tokens` plus
#' `rec_tokens` when both are reported. `tokens` sums the calls that reported
#' usage and is `NA` when none did; counts that are not nonnegative whole
#' numbers are treated as unreported, with a warning.
#'
#' @param protocols A list of [protocol()] objects, or one protocol.
#' @param gold A [gold_set()].
#' @param split Name of the split to evaluate. The default is `"dev"`.
#'   The gold set's declared holdout split is refused.
#' @param .runner A function accepting `(experiments, ...)`. It must return
#'   the experiment rows with their identifying columns and a nonmissing
#'   `response_text` column. If it returns `success`, every value must be
#'   `TRUE`. The default runner is [LLMR::call_llm_par()].
#' @param ... Arguments passed to the runner.
#' @return A list of class `protocol_tuning` with the following elements.
#'   \describe{
#'     \item{table}{A tibble with one row per protocol, ordered by decreasing
#'       accuracy. The columns have the following meanings.
#'       \itemize{
#'         \item `protocol`, the protocol label, made unique when needed.
#'         \item `n`, the number of scored units.
#'         \item `accuracy`, the proportion of correct labels.
#'         \item `acc_lo` and `acc_hi`, the limits of the exact 95 percent
#'           binomial confidence interval.
#'         \item `macro_f1`, the mean category F1 score, with undefined
#'           category scores counted as zero.
#'         \item `parse_failures`, the number of missing modal labels.
#'         \item `tokens`, the subtotal of known token totals.
#'       }}
#'     \item{per_category}{Category-level scores, named by the protocol
#'       labels used in `table`.}
#'     \item{split}{The evaluated split name.}
#'     \item{usage}{A tibble in the same protocol order as `table`, with
#'       columns `protocol`, `calls` (model calls, replicates included), and
#'       `tokens_unknown` (calls that reported no usage).}
#'   }
#' @examples
#' \dontrun{
#' cb <- codebook("tone", "one sentence",
#'   list(cb_category("positive", "Approving."),
#'        cb_category("negative", "Critical.")))
#' gold_data <- data.frame(
#'   text  = c(paste("clear benefit", 1:10), paste("serious harm", 1:10)),
#'   label = rep(c("positive", "negative"), each = 10))
#' set.seed(110)
#' g  <- gold_set(gold_data, text = "text", label = "label",
#'                split = c(dev = 0.5, test = 0.5))
#' cfg <- LLMR::llm_config("groq", "openai/gpt-oss-20b", temperature = 0)
#' tune_protocol(list(protocol(cb, cfg, label = "baseline")), g)
#' }
#' @export
tune_protocol <- function(protocols, gold, split = "dev",
                          .runner = NULL, ...) {
  if (inherits(protocols, "coding_protocol")) protocols <- list(protocols)
  stopifnot(is.list(protocols), length(protocols) >= 1L,
            inherits(gold, "gold_set"))
  for (p in protocols) {
    if (!inherits(p, "coding_protocol")) {
      abort("`protocols` must be coding_protocol objects.")
    }
  }
  if (identical(split, .gold_holdout(gold))) {
    abort(sprintf(paste(
      "The holdout split ('%s') is sealed for tuning.",
      "Lock one protocol and run validate_protocol() once."),
      .gold_holdout(gold)))
  }
  g <- gold_split(gold, split)
  texts <- g[[gold$text]]
  truth <- as.character(g[[gold$label]])

  res <- .run_protocols(protocols, texts, .runner = .runner, ...)
  # Distinct labels keep the comparison table and the per_category field
  # one-to-one with the protocols; duplicate labels (the default when tuning
  # prompt variants on one model) would otherwise overwrite each other.
  plabels <- make.unique(vapply(protocols, `[[`, character(1), "label"))
  per_cat <- list()
  usage <- list()
  rows <- lapply(seq_along(protocols), function(p) {
    ri <- res[res$protocol_id == p, ]
    ri <- ri[order(ri$unit_id), ]
    sc <- .score_labels(ri$label, truth, codebook_labels(protocols[[p]]$codebook))
    ci <- .acc_ci(ri$label, truth)
    per_cat[[plabels[[p]]]] <<- sc$per_category
    u <- .tokens_of(ri)
    usage[[p]] <<- tibble::tibble(protocol = plabels[[p]], calls = u$calls,
                                  tokens_unknown = u$unknown)
    tibble::tibble(
      protocol = plabels[[p]],
      n = sc$n, accuracy = sc$accuracy,
      acc_lo = ci[1], acc_hi = ci[2],
      macro_f1 = sc$macro_f1, parse_failures = sc$parse_failures,
      tokens = u$tokens)
  })
  table <- do.call(rbind, rows)
  ord <- order(-table$accuracy)
  table <- table[ord, ]
  usage <- do.call(rbind, usage)[ord, ]
  structure(
    list(table = table, per_category = per_cat, split = split, usage = usage),
    class = "protocol_tuning"
  )
}

#' @export
print.protocol_tuning <- function(x, ...) {
  cat(sprintf("<protocol_tuning | split '%s' | protocols = %d>\n",
              x$split, nrow(x$table)))
  print(x$table, ...)
  if (!is.null(x$usage)) {
    line <- .usage_unknown_line(sum(x$usage$tokens_unknown), sum(x$usage$calls))
    if (!is.null(line)) cat("  ", line, "\n", sep = "")
  }
  invisible(x)
}

#' Coerce protocol tuning results to a tibble
#'
#' @param x A `protocol_tuning` object.
#' @param ... Passed to [tibble::as_tibble()].
#' @return A tibble with one row per protocol.
#' @exportS3Method tibble::as_tibble
as_tibble.protocol_tuning <- function(x, ...) {
  tibble::as_tibble(x$table, ...)
}

# Runner batches may omit token columns on passes with no reported usage.
.bind_call_rows <- function(batches) {
  columns <- unique(unlist(lapply(batches, names), use.names = FALSE))
  batches <- lapply(batches, function(batch) {
    for (column in setdiff(columns, names(batch))) {
      batch[[column]] <- rep(NA, nrow(batch))
    }
    batch[columns]
  })
  do.call(rbind, batches)
}

# Each row carries its total and its contribution to call and missing counts.
# Keep these together when protocol-unit rows are reduced across replicates.
.call_tokens <- function(res) {
  n <- nrow(res)
  column <- function(name) {
    v <- res[[name]]
    if (is.null(v) || (is.logical(v) && all(is.na(v)))) return(rep(NA_real_, n))
    if (!is.numeric(v)) {
      cli::cli_warn(paste0(
        "The runner's {.field {name}} column is {.cls {class(v)[[1]]}}, ",
        "not numeric; its counts are treated as unknown."))
      return(rep(NA_real_, n))
    }
    invalid <- is.nan(v) | (!is.na(v) & (!is.finite(v) | v < 0 | v != trunc(v)))
    if (any(invalid)) {
      cli::cli_warn("The runner's {.field {name}} column contains invalid counts; they are treated as unknown.")
      v[invalid] <- NA_real_
    }
    as.double(v)
  }
  sent <- column("sent_tokens")
  rec <- column("rec_tokens")
  total <- column("total_tokens")
  total <- ifelse(is.na(total) & !is.na(sent) & !is.na(rec), sent + rec, total)
  follow_on <- rep(FALSE, n)
  pack <- res[["rowpack_row"]]
  pack_id <- res[["rowpack_id"]]
  if (is.numeric(pack) && !is.null(pack_id)) {
    first <- pack_id[!is.na(pack) & pack == 1 & !is.na(pack_id)]
    follow_on <- !is.na(pack) & pack > 1 & !is.na(pack_id) & pack_id %in% first
  }
  data.frame(tokens = as.double(total), calls = .call_count_rows(res),
             unknown = as.integer(is.na(total) & !follow_on))
}

# Attribute each distinct identifier to its first row. This preserves call
# counts when several unit rows came from the same request.
.call_count_rows <- function(res) {
  n <- nrow(res)
  complete <- function(id) {
    if (is.list(id)) return(length(id) > 0L && all(vapply(id, complete, logical(1))))
    id <- as.character(id)
    length(id) > 0L && all(!is.na(id) & nzchar(id))
  }
  failed <- res[["success"]] %in% FALSE
  if (!length(failed)) failed <- rep(FALSE, n)
  for (column in intersect(c("call_id", "request_id", "response_id"), names(res))) {
    ids <- lapply(res[[column]], function(id) as.character(unlist(id, use.names = FALSE)))
    has <- vapply(res[[column]], complete, logical(1))
    if (any(has) && all(has | failed)) {
      ids[!has] <- rep(list(character()), sum(!has))
      row <- rep(seq_len(n), lengths(ids))
      first <- !duplicated(unlist(ids, use.names = FALSE))
      return(as.integer(tabulate(row[first], nbins = n) + !has))
    }
  }
  # Each runner row is one request unless shared identifiers say otherwise.
  rep(1L, n)
}

# Sum known totals separately from the integer call and missing counts.
.token_sum <- function(tok) {
  list(tokens = if (nrow(tok) && all(is.na(tok$tokens))) NA_real_
                else as.double(sum(tok$tokens, na.rm = TRUE)),
       calls = as.integer(sum(tok$calls)),
       unknown = as.integer(sum(tok$unknown)))
}

# Sum token subtotals and usage counts over protocol-unit rows.
.tokens_of <- function(res) {
  list(tokens = if (nrow(res) && all(is.na(res$.tokens))) NA_real_
                else as.double(sum(res$.tokens, na.rm = TRUE)),
       calls = as.integer(sum(res$.calls)),
       unknown = as.integer(sum(res$.tokens_unknown)))
}

# Internal: the print line for calls whose usage is unknown, or NULL.
.usage_unknown_line <- function(unknown, calls) {
  if (!isTRUE(unknown > 0L)) return(NULL)
  if (unknown == calls) {
    return(sprintf("token usage unknown for all %d call(s)", calls))
  }
  sprintf("token usage unknown for %d of %d call(s); `tokens` sums the rest",
          unknown, calls)
}

#' Validate a coding protocol against gold labels
#'
#' Applies one protocol to a gold-set split and compares its modal labels
#' with the gold labels.
#'
#' @details
#' Evaluation on the declared holdout split requires a locked protocol.
#' A locked protocol must still match its stored hash. Evaluation on
#' another split may use an unlocked protocol.
#'
#' An evaluation on the holdout split is appended to the gold set's ledger
#' when that split is sealed.
#'
#' Tokens are counted as in [tune_protocol()]: `tokens` sums the calls that
#' reported usage and is `NA` when none did.
#'
#' @param protocol A [protocol()]. It must be locked for evaluation on the
#'   gold set's declared holdout split.
#' @param gold A [gold_set()].
#' @param split Name of the split to evaluate. `NULL` selects the gold set's
#'   declared holdout split.
#' @inheritParams tune_protocol
#' @return A list of class `protocol_validation` with the following elements.
#'   \itemize{
#'     \item `protocol`, the protocol label.
#'     \item `protocol_hash`, the stored protocol hash.
#'     \item `split`, the evaluated split name.
#'     \item `holdout`, the declared holdout split name.
#'     \item `n`, the number of scored units.
#'     \item `accuracy`, the proportion of correct labels.
#'     \item `acc_lo` and `acc_hi`, the exact 95 percent binomial confidence
#'       limits.
#'     \item `macro_f1`, the mean category F1 score, with undefined category
#'       scores counted as zero.
#'     \item `parse_failures`, the number of missing modal labels.
#'     \item `tokens`, the subtotal of known token totals.
#'     \item `calls`, the number of model calls, replicates included.
#'     \item `tokens_unknown`, the number of calls that reported no usage.
#'     \item `per_category`, the category-level score table.
#'     \item `confusion`, the table of gold and predicted labels.
#'     \item `ledger_entries`, the number of ledger entries after the call,
#'       including this evaluation when it was recorded.
#'   }
#' @examples
#' \dontrun{
#' cb <- codebook("tone", "one sentence",
#'   list(cb_category("positive", "Approving."),
#'        cb_category("negative", "Critical.")))
#' gold_data <- data.frame(
#'   text  = c(paste("clear benefit", 1:10), paste("serious harm", 1:10)),
#'   label = rep(c("positive", "negative"), each = 10))
#' set.seed(110)
#' g <- gold_set(gold_data, text = "text", label = "label",
#'               split = c(test = 1))
#' p <- protocol_lock(protocol(cb, LLMR::llm_config("groq", "openai/gpt-oss-20b")))
#' validate_protocol(p, g)
#' gold_ledger(g)   # the evaluation is on the record
#' }
#' @export
validate_protocol <- function(protocol, gold, split = NULL,
                              .runner = NULL, ...) {
  stopifnot(inherits(protocol, "coding_protocol"), inherits(gold, "gold_set"))
  holdout <- .gold_holdout(gold)
  split <- split %||% holdout
  is_holdout <- identical(split, holdout)
  if (is_holdout && !isTRUE(protocol$locked)) {
    abort(sprintf(paste(
      "Refusing to evaluate an unlocked protocol on the holdout split ('%s').",
      "Call protocol_lock() first; the hash ties the validation to",
      "exactly this instrument."), split))
  }
  if (isTRUE(protocol$locked) &&
      !identical(protocol$hash, .protocol_hash(protocol))) {
    abort("The locked protocol has changed since protocol_lock(); lock it again before validation.")
  }
  g <- gold_split(gold, split)
  texts <- g[[gold$text]]
  truth <- as.character(g[[gold$label]])
  res <- .run_protocols(list(protocol), texts, .runner = .runner, ...)
  res <- res[order(res$unit_id), ]
  sc <- .score_labels(res$label, truth, codebook_labels(protocol$codebook))
  ci <- .acc_ci(res$label, truth)
  if (is_holdout && isTRUE(gold$sealed)) {
    .gold_ledger_append(gold, split, protocol$hash, protocol$label,
                        sc$n, sc$accuracy)
  }
  u <- .tokens_of(res)
  structure(
    list(protocol = protocol$label, protocol_hash = protocol$hash,
         split = split, holdout = holdout, n = sc$n,
         accuracy = sc$accuracy, acc_lo = ci[1], acc_hi = ci[2],
         macro_f1 = sc$macro_f1, parse_failures = sc$parse_failures,
         tokens = u$tokens, calls = u$calls, tokens_unknown = u$unknown,
         per_category = sc$per_category, confusion = sc$confusion,
         ledger_entries = length(gold$ledger$rows)),
    class = "protocol_validation"
  )
}

#' @export
print.protocol_validation <- function(x, ...) {
  cat(sprintf("<protocol_validation | %s | split '%s' | n = %d>\n",
              x$protocol, x$split, x$n))
  cat(sprintf("  accuracy %.3f [%.3f, %.3f] | macro-F1 %.3f | parse failures %d\n",
              x$accuracy, x$acc_lo, x$acc_hi, x$macro_f1, x$parse_failures))
  line <- .usage_unknown_line(x[["tokens_unknown"]], x[["calls"]])
  if (!is.null(line)) cat("  ", line, "\n", sep = "")
  if (identical(x$split, x$holdout)) {
    cat(sprintf("  %s-split evaluations ledgered so far: %d\n",
                x$split, x$ledger_entries))
  }
  invisible(x)
}

#' Report a protocol validation through the LLMR generic
#'
#' `LLMR::report()` for a `protocol_validation` assembles the report from the
#' validation, gold set, and locked protocol. The validation object stores
#' scores but not the latter two objects, so calls must pass `gold =` and
#' `protocol =` through `...`.
#'
#' @param x A `protocol_validation` object.
#' @param ... Must include `gold =` and `protocol =`; additional arguments
#'   are forwarded to report construction.
#' @return A `coding_report` object.
#' @exportS3Method LLMR::report
report.protocol_validation <- function(x, ...) {
  args <- list(...)
  if (!"gold" %in% names(args) || is.null(args$gold)) {
    abort("`report()` for a protocol_validation requires `gold =`.")
  }
  if (!"protocol" %in% names(args) || is.null(args$protocol)) {
    abort("`report()` for a protocol_validation requires `protocol =`.")
  }
  gold <- args$gold
  protocol <- args$protocol
  args$gold <- NULL
  args$protocol <- NULL
  if (!is.null(x$protocol_hash) && inherits(protocol, "coding_protocol") &&
      !identical(protocol$hash, x$protocol_hash)) {
    abort(paste(
      "The supplied protocol does not match the one this validation scored",
      "(hash mismatch); pass the protocol that produced the validation."))
  }
  do.call(coding_report,
          c(list(validation = x, gold = gold, protocol = protocol), args))
}

#' Protocol-validation diagnostics
#'
#' Returns the machine-readable validation summary.
#'
#' @param x A `protocol_validation` object.
#' @param ... Ignored.
#' @return A one-row tibble with accuracy, interval, macro-F1, parse failures,
#'   evaluated units, and ledger entries.
#' @exportS3Method LLMR::diagnostics
diagnostics.protocol_validation <- function(x, ...) {
  tibble::tibble(
    accuracy = x$accuracy,
    acc_lo = x$acc_lo,
    acc_hi = x$acc_hi,
    macro_f1 = x$macro_f1,
    parse_failures = as.integer(x$parse_failures),
    n = as.integer(x$n),
    ledger_entries = as.integer(x$ledger_entries)
  )
}

#' Code a corpus with a locked protocol
#'
#' Applies the locked protocol to every text, with `protocol$replicates`
#' codings per unit. With replicates, the modal label and the share of
#' replicates agreeing with it are returned, which is the unit-level
#' stability diagnostic reviewers should ask for.
#'
#' @details Execution is live and parallel (`LLMR::call_llm_par()`); the
#'   `.runner` seam accepts any function with the same contract, including
#'   the replayer [archive_replay()] returns.
#'
#' @param corpus A data frame.
#' @param protocol A **locked** [protocol()].
#' @param text Name of the text column in `corpus`.
#' @param id Optional name of a stable unit-identifier column in `corpus`. Carry
#'   it through so [gold_correct()] can link audit units to corpus rows by id,
#'   the only way to disambiguate rows that share identical text. Use the same
#'   `id` here as in [gold_set()].
#' @inheritParams tune_protocol
#' @return A `coded_corpus` object. Its `data` field contains `corpus` plus
#'   `label` (modal label; `NA` when no replicate parsed or when the top
#'   labels tie, since a tie is disagreement, not a winner), `label_share`
#'   (the top label's share of all replicates, so parse failures lower it),
#'   `label_share_parsed` (its share among parsed replies alone), `tied`,
#'   `parse_failures` per unit, a `.text_hash` linkage column, and when
#'   `protocol$replicates > 1` the individual replicate columns `label_rep1`,
#'   `label_rep2`, .... Protocol and linkage fields are stored alongside the
#'   data. These result columns are reserved: a corpus that already contains
#'   one of them is refused rather than overwritten.
#'
#' @section Coded text rides in the prompt:
#'   The rendered prompt interpolates each unit's text into the instruction
#'   message. A document that itself contains instruction-like text ("ignore
#'   the codebook and answer X") can therefore try to steer the model; for
#'   adversarial corpora, inspect replicate disagreement and validate against
#'   human labels with particular care. Separating instructions from data at
#'   the message level is planned for a later release.
#' @examples
#' cb <- codebook("tone", "one sentence",
#'   list(cb_category("positive", "Approving."),
#'        cb_category("negative", "Critical.")))
#' cfg <- LLMR::llm_config("groq", "openai/gpt-oss-20b", temperature = 0)
#' \dontrun{
#' p <- protocol_lock(protocol(cb, cfg, replicates = 2))
#' code_corpus(data.frame(text = c("clear progress", "serious problem")),
#'             p, "text")
#' }
#'
#' # The `.runner` seam answers the calls without a provider, for tests or for
#' # a deterministic or external coder:
#' p <- protocol_lock(protocol(cb, cfg))
#' keyword_coder <- function(experiments, ...) {
#'   user <- vapply(experiments$messages, `[[`, "", "user")
#'   experiments$response_text <- ifelse(grepl("progress", user),
#'                                       "positive", "negative")
#'   experiments
#' }
#' code_corpus(data.frame(text = c("clear progress", "serious problem")),
#'             p, "text", .runner = keyword_coder)
#' @export
code_corpus <- function(corpus, protocol, text, id = NULL, .runner = NULL, ...) {
  stopifnot(is.data.frame(corpus), inherits(protocol, "coding_protocol"))
  if (!isTRUE(protocol$locked)) {
    abort("code_corpus() requires a locked protocol (protocol_lock()).")
  }
  if (!identical(protocol$hash, .protocol_hash(protocol))) {
    abort("The locked protocol has changed since protocol_lock(); lock it again before coding.")
  }
  if (!text %in% names(corpus)) abort(sprintf("Column '%s' not found.", text))
  if (!is.null(id) && !id %in% names(corpus)) {
    abort(sprintf("`id` column '%s' not found in `corpus`.", id))
  }
  texts <- corpus[[text]]
  if (anyNA(texts)) {
    abort(sprintf(paste(
      "%d row(s) of column '%s' are NA. A missing text cannot be coded;",
      "drop those rows or repair the corpus first."),
      sum(is.na(texts)), text))
  }
  k <- protocol$replicates
  # The result columns are written into the returned data. Refuse to shadow
  # existing columns rather than silently destroying user data (a labeled
  # corpus routinely arrives with its own `label`).
  reserved <- c("label", "label_share", "label_share_parsed", "tied",
                "parse_failures", ".text_hash",
                if (k > 1L) paste0("label_rep", seq_len(k)))
  collision <- intersect(names(corpus), reserved)
  if (length(collision)) {
    abort(sprintf(paste(
      "`corpus` already contains column(s) %s, which code_corpus() writes.",
      "Rename or drop them first."),
      paste(sprintf("'%s'", collision), collapse = ", ")))
  }
  if (!nrow(corpus)) {
    out <- tibble::as_tibble(corpus)
    out$label <- character(0)
    out$label_share <- numeric(0)
    out$label_share_parsed <- numeric(0)
    out$tied <- logical(0)
    out$parse_failures <- integer(0)
    if (k > 1L) {
      for (r in seq_len(k)) out[[paste0("label_rep", r)]] <- character(0)
    }
    out$.text_hash <- character(0)
    return(.new_coded_corpus(out, protocol, text, id))
  }
  res <- .run_protocols(list(protocol), texts, .runner = .runner, ...)
  res <- res[order(res$unit_id), , drop = FALSE]
  m <- do.call(rbind, res$replicate_labels)
  out <- tibble::as_tibble(corpus)
  out$label <- res$label
  out$label_share <- res$label_share
  out$label_share_parsed <- res$label_share_parsed
  out$tied <- res$tied
  out$parse_failures <- res$parse_failures
  if (k > 1L) {
    for (r in seq_len(k)) out[[paste0("label_rep", r)]] <- m[, r]
  }
  out$.text_hash <- .text_hash(texts)
  .new_coded_corpus(out, protocol, text, id)
}

# Internal constructor shared by populated and typed-empty results.
.new_coded_corpus <- function(data, protocol, text, id) {
  structure(
    list(
      data = data,
      protocol_hash = protocol$hash,
      protocol_label = protocol$label,
      text = text,
      label = "label",
      id = id,
      labels = codebook_labels(protocol$codebook)
    ),
    class = "coded_corpus"
  )
}

#' @export
print.coded_corpus <- function(x, ...) {
  linkage <- if (is.null(x$id)) "text hash" else sprintf("id '%s'", x$id)
  cat(sprintf("<coded_corpus | %d unit(s) | %d label(s) | %s | protocol %s>\n",
              nrow(x$data), length(x$labels), linkage,
              substr(x$protocol_hash, 1, 12)))
  invisible(x)
}

#' Coerce a coded corpus to a tibble
#'
#' @param x A [code_corpus()] result.
#' @param ... Passed to [tibble::as_tibble()].
#' @return The coded corpus's `data` tibble.
#' @exportS3Method tibble::as_tibble
as_tibble.coded_corpus <- function(x, ...) {
  tibble::as_tibble(x$data, ...)
}
