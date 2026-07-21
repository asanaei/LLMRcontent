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
    stopifnot(is.data.frame(out), "response_text" %in% names(out))
    out$replicate <- r
    out
  })
  res <- do.call(rbind, res)
  labels_of <- function(p) codebook_labels(protocols[[p]]$codebook)
  res$label <- vapply(seq_len(nrow(res)), function(i) {
    pr <- protocols[[res$protocol_id[i]]]
    out <- pr$parser(res$response_text[i] %||% NA_character_,
                     labels_of(res$protocol_id[i]))
    as.character(out %||% NA_character_)
  }, character(1))

  token_cols <- intersect(c("sent_tokens", "rec_tokens"), names(res))
  rows <- list()
  for (p in seq_along(protocols)) {
    for (i in seq_along(texts)) {
      ri <- res[res$protocol_id == p & res$unit_id == i, , drop = FALSE]
      ri <- ri[order(ri$replicate), , drop = FALSE]
      labels <- ri$label
      present <- labels[!is.na(labels)]
      modal <- if (!length(present)) NA_character_ else {
        names(sort(table(present), decreasing = TRUE))[1]
      }
      row <- tibble::tibble(
        protocol_id = p,
        unit_id = i,
        label = modal,
        label_share = if (!length(present)) NA_real_ else
          mean(labels == modal, na.rm = TRUE),
        parse_failures = sum(is.na(labels)),
        replicate_labels = list(labels)
      )
      for (col in token_cols) row[[col]] <- sum(ri[[col]], na.rm = TRUE)
      rows[[length(rows) + 1L]] <- row
    }
  }
  do.call(rbind, rows)
}

#' Tune candidate protocols on the development split
#'
#' Runs every protocol over the gold set's `split` rows (default `"dev"`)
#' and scores each protocol's modal label across its configured replicates
#' against the gold labels: accuracy with a bootstrap CI, macro-F1, and parse
#' failures. This is the tuning loop: iterate freely here; the holdout split
#' waits, sealed, for the one protocol you lock.
#'
#' @param protocols A list of [protocol()] objects (or a single one).
#' @param gold A [gold_set()].
#' @param split Which split to evaluate on; the gold set's holdout split
#'   (`"test"` unless [gold_set()] was given another `holdout` name) is
#'   refused here. That is [validate_protocol()]'s job, and it leaves a
#'   ledger entry.
#' @param .runner Internal seam for tests: a function `(experiments, ...)`
#'   returning the experiments with a `response_text` column. Default
#'   `LLMR::call_llm_par()`.
#' @param ... Passed to the runner (e.g. `tries`, `progress`).
#' @return A `protocol_tuning` result: tibble with one row per protocol
#'   (`protocol`, `n`, `accuracy`, `acc_lo`, `acc_hi`, `macro_f1`,
#'   `parse_failures`, `tokens` when the runner reports usage), plus
#'   per-protocol detail in `attr(x, "per_category")`.
#' @examples
#' \dontrun{
#' cb <- codebook("tone", "one sentence",
#'   list(cb_category("positive", "Approving."),
#'        cb_category("negative", "Critical.")))
#' gold_data <- data.frame(
#'   text  = c(paste("clear benefit", 1:10), paste("serious harm", 1:10)),
#'   label = rep(c("positive", "negative"), each = 10))
#' g  <- gold_set(gold_data, text = "text", labels = "label",
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
  truth <- as.character(g[[gold$labels]])

  res <- .run_protocols(protocols, texts, .runner = .runner, ...)
  # Distinct labels keep the comparison table and the per_category attribute
  # one-to-one with the protocols; duplicate labels (the default when tuning
  # prompt variants on one model) would otherwise overwrite each other.
  plabels <- make.unique(vapply(protocols, `[[`, character(1), "label"))
  per_cat <- list()
  rows <- lapply(seq_along(protocols), function(p) {
    ri <- res[res$protocol_id == p, ]
    ri <- ri[order(ri$unit_id), ]
    sc <- .score_labels(ri$label, truth, codebook_labels(protocols[[p]]$codebook))
    ci <- .acc_ci(ri$label, truth)
    per_cat[[plabels[[p]]]] <<- sc$per_category
    tibble::tibble(
      protocol = plabels[[p]],
      n = sc$n, accuracy = sc$accuracy,
      acc_lo = ci[1], acc_hi = ci[2],
      macro_f1 = sc$macro_f1, parse_failures = sc$parse_failures,
      tokens = .tokens_of(ri))
  })
  out <- do.call(rbind, rows)
  out <- out[order(-out$accuracy), ]
  attr(out, "per_category") <- per_cat
  attr(out, "split") <- split
  class(out) <- c("protocol_tuning", class(out))
  out
}

#' @export
print.protocol_tuning <- function(x, ...) {
  cat(sprintf("<protocol_tuning | split '%s' | protocols = %d>\n",
              attr(x, "split") %||% NA_character_, nrow(x)))
  NextMethod()
  invisible(x)
}

#' Coerce protocol tuning results to a tibble
#'
#' Strips the `protocol_tuning` class and returns the comparison table.
#'
#' @param x A `protocol_tuning` object.
#' @param ... Passed to [tibble::as_tibble()].
#' @return A tibble with one row per protocol.
#' @exportS3Method tibble::as_tibble
as_tibble.protocol_tuning <- function(x, ...) {
  out <- x
  class(out) <- setdiff(class(out), "protocol_tuning")
  tibble::as_tibble(out, ...)
}

# Internal: total tokens (sent + received) when the runner reported them.
# The cost story belongs in the comparison table, not in a footnote.
.tokens_of <- function(res) {
  cols <- intersect(c("sent_tokens", "rec_tokens"), names(res))
  if (!length(cols)) return(NA_integer_)
  as.integer(sum(unlist(res[cols]), na.rm = TRUE))
}

#' Validate a locked protocol on the sealed holdout split
#'
#' The one honest evaluation. Requires a locked protocol (so the validated
#' instrument is hash-identified), applies its configured replicate count and
#' modal-label rule over the holdout split, and when the gold set was built with
#' `seal_test = TRUE` appends the event to the ledger, so every holdout-split
#' evaluation that ever happened appears in [coding_report()].
#'
#' @param protocol A **locked** [protocol()].
#' @param gold A [gold_set()].
#' @param split Which split to evaluate on. Defaults to the gold set's
#'   holdout split (`"test"` unless [gold_set()] was given another
#'   `holdout` name).
#' @inheritParams tune_protocol
#' @return A `protocol_validation`: accuracy with bootstrap CI, macro-F1,
#'   parse failures, total tokens (when the runner reported them),
#'   per-category table, confusion matrix, the protocol hash, the gold
#'   set's holdout split name, and the ledger position of this evaluation.
#' @examples
#' \dontrun{
#' cb <- codebook("tone", "one sentence",
#'   list(cb_category("positive", "Approving."),
#'        cb_category("negative", "Critical.")))
#' gold_data <- data.frame(
#'   text  = c(paste("clear benefit", 1:10), paste("serious harm", 1:10)),
#'   label = rep(c("positive", "negative"), each = 10))
#' g <- gold_set(gold_data, text = "text", labels = "label",
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
  truth <- as.character(g[[gold$labels]])
  res <- .run_protocols(list(protocol), texts, .runner = .runner, ...)
  res <- res[order(res$unit_id), ]
  sc <- .score_labels(res$label, truth, codebook_labels(protocol$codebook))
  ci <- .acc_ci(res$label, truth)
  if (is_holdout && isTRUE(gold$sealed)) {
    .gold_ledger_append(gold, split, protocol$hash, protocol$label,
                        sc$n, sc$accuracy)
  }
  structure(
    list(protocol = protocol$label, protocol_hash = protocol$hash,
         split = split, holdout = holdout, n = sc$n,
         accuracy = sc$accuracy, acc_lo = ci[1], acc_hi = ci[2],
         macro_f1 = sc$macro_f1, parse_failures = sc$parse_failures,
         tokens = .tokens_of(res),
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
  if (identical(x$split, x$holdout %||% "test")) {
    cat(sprintf("  %s-split evaluations ledgered so far: %d\n",
                x$split, x$ledger_entries))
  }
  invisible(x)
}

#' Report a protocol validation through the LLMR generic
#'
#' `LLMR::report()` for a `protocol_validation` delegates to
#' [coding_report()]. The validation object stores scores but not the gold
#' set or the locked protocol, so calls must pass `gold =` and `protocol =`
#' through `...`.
#'
#' @param x A `protocol_validation` object.
#' @param ... Must include `gold =` and `protocol =`; additional arguments
#'   are forwarded to [coding_report()].
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
#' @return `corpus` plus `label` (modal label), `label_share` (share of
#'   replicates agreeing with it), `parse_failures` per unit, a `.text_hash`
#'   linkage column, and when `protocol$replicates > 1` the individual replicate
#'   columns `label_rep1`, `label_rep2`, ....
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
code_corpus <- function(corpus, protocol, text, .runner = NULL, id = NULL, ...) {
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
  k <- protocol$replicates
  res <- .run_protocols(list(protocol), texts, .runner = .runner, ...)
  res <- res[order(res$unit_id), , drop = FALSE]
  m <- do.call(rbind, res$replicate_labels)
  out <- tibble::as_tibble(corpus)
  out$label <- res$label
  out$label_share <- res$label_share
  out$parse_failures <- res$parse_failures
  if (k > 1L) {
    for (r in seq_len(k)) out[[paste0("label_rep", r)]] <- m[, r]
  }
  out$.text_hash <- .text_hash(texts)
  attr(out, "protocol_hash") <- protocol$hash
  attr(out, "protocol_label") <- protocol$label
  attr(out, "text") <- text
  attr(out, "id") <- id
  attr(out, "labels") <- codebook_labels(protocol$codebook)
  out
}
