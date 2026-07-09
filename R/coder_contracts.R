# coder_contracts.R ------------------------------------------------------------------
# Gold-set correction: the coded corpus is the population, the test split is
# the audit sample, and the difference estimator removes the measurement bias
# the audit reveals.

# Internal: choose the key that links audit (test-split) units to coded-corpus
# rows. Preference: a shared explicit id (the only key that survives identical
# text), then a content hash of the text. Returns the two key vectors and a
# human label for messages. The id path requires the same id column on both
# objects; a partial id (one side only) falls back to the text hash with a note.
.resolve_gold_link <- function(coded, gold, g, text_col) {
  coded_id <- attr(coded, "id")
  gold_id <- gold$id
  if (!is.null(coded_id) && !is.null(gold_id)) {
    if (!coded_id %in% names(coded)) {
      abort(sprintf("`coded` declares id column '%s' but it is absent; re-run code_corpus().", coded_id))
    }
    if (!gold_id %in% names(g)) {
      abort(sprintf("gold set declares id column '%s' but it is absent from its data.", gold_id))
    }
    return(list(by = "id",
                corpus_key = as.character(coded[[coded_id]]),
                gold_key = as.character(g[[gold_id]])))
  }
  if (xor(is.null(coded_id), is.null(gold_id))) {
    cli::cli_warn(paste(
      "Only one of `coded`/`gold` carries an id column; linking by text hash",
      "instead. Pass the same `id` to both gold_set() and code_corpus() to",
      "link by id."))
  }
  corpus_key <- if (".text_hash" %in% names(coded)) {
    as.character(coded[[".text_hash"]])
  } else {
    .text_hash(coded[[text_col]])
  }
  gold_key <- if (".text_hash" %in% names(g)) {
    as.character(g[[".text_hash"]])
  } else {
    .text_hash(g[[gold$text]])
  }
  list(by = "text hash", corpus_key = corpus_key, gold_key = gold_key)
}

#' Correct corpus prevalences with the gold-set audit
#'
#' LLM labels are error-prone measurements, and the error is rarely
#' symmetric, so the naive share of a category in the coded corpus is
#' biased. `gold_correct()` estimates corrected corpus-level category
#' prevalences by combining the full coded corpus with the gold set's test
#' split, and reports a standard error for each.
#'
#' @param coded A [code_corpus()] result.
#' @param gold A [gold_set()] whose test split is an audited subsample of
#'   the coded corpus (linked by a shared `id` when present, otherwise by a
#'   content hash of the text; see Details).
#' @param conf Confidence level for the normal-approximation intervals.
#' @return A `gold_correction` object: a list with `table` (`category`,
#'   `share_naive`, `share_corrected`, `se`, `ci_lo`, `ci_hi`), `n_corpus`,
#'   `n_parse_failures`, `n_audit`, `n_audit_parse_failures`,
#'   `accuracy_audit`, `protocol_hash`, `protocol_label`, `conf`, `link_by`
#'   (how audit units were linked to the corpus, `"id"` or `"text hash"`),
#'   and `sealed` (whether the gold set's test split was sealed), with a
#'   print method.
#'
#' @details
#' For category `c`, the estimator is the corpus mean of
#' `1(llm label = c)` plus the audit mean of
#' `1(gold label = c) - 1(llm label = c)` -- the survey-sampling
#' difference estimator, which is also what prediction-powered inference
#' reduces to for proportions. Its estimated variance is
#' `(1 - n/N) * S2_d / n`, where `d` is the audit difference, `n` the
#' number of audit pairs, and `N` the number of parsed corpus labels; the
#' corpus term carries no sampling error because the corpus itself is the
#' estimand's population.
#'
#' Only the test split is used. The dev split tuned the protocol, so dev
#' error rates are optimistic, and a correction built on them inherits the
#' optimism.
#'
#' The estimand conditions on parse success: corpus rows whose label is
#' `NA` are excluded from the shares and counted, as are matched audit
#' units whose corpus label is `NA`.
#'
#' Two assumptions do the inferential work: the audited units are a random
#' subsample of the corpus (or of the population the corpus represents),
#' and the corpus labels and the audit-pair labels come from the same
#' locked protocol -- which the linkage guarantees here, because the
#' audit pairs take their model labels from the coded corpus itself.
#'
#' Linkage uses a shared `id` column when both [gold_set()] and
#' [code_corpus()] were given one; this is the only key that can tell apart
#' units with identical text. Absent an id, units are linked by a content hash
#' of the (whitespace-normalized) text, and audited units whose text is
#' duplicated in the corpus are refused rather than matched to an arbitrary
#' row -- supply an `id` to handle genuine duplicates.
#'
#' Corrected shares are not clamped to `[0, 1]`; a value outside the unit
#' interval is a signal that the audit correction is noisy, and the print
#' method says so when it happens.
#'
#' Using test-split truth is a look at the test split, so when the gold
#' set is sealed the event is appended to the ledger and appears in
#' [coding_report()] like any other evaluation.
#'
#' @examples
#' \dontrun{
#' cb <- codebook("stance", "one text",
#'   list(cb_category("positive", "Approving."),
#'        cb_category("negative", "Critical.")))
#' gold_data <- data.frame(
#'   text  = c(paste("clear benefit", 1:10), paste("serious harm", 1:10)),
#'   label = rep(c("positive", "negative"), each = 10))
#' gold <- gold_set(gold_data, text = "text", labels = "label",
#'                  split = c(test = 1))
#' corpus <- data.frame(text = c(gold_data$text,
#'                               "a hopeful note", "an alarming figure"))
#' cfg <- LLMR::llm_config("groq", "openai/gpt-oss-20b", temperature = 0)
#' coded <- code_corpus(corpus, protocol_lock(protocol(cb, cfg)), "text")
#' gold_correct(coded, gold)
#' }
#'
#' @references
#' Angelopoulos, Bates, Fannjiang, Jordan, and Zrnic (2023).
#' "Prediction-Powered Inference." \emph{Science} 382(6671), 669-674.
#'
#' Egami, Hinck, Stewart, and Wei (2023). "Using Imperfect Surrogates for
#' Downstream Inference." \emph{Advances in Neural Information Processing
#' Systems} 36.
#'
#' Cochran (1977). \emph{Sampling Techniques}, 3rd edition, on the
#' difference estimator.
#'
#' @seealso [code_corpus()], [gold_set()], [validate_protocol()].
#' @export
gold_correct <- function(coded, gold, conf = 0.95) {
  stopifnot(is.data.frame(coded), inherits(gold, "gold_set"))
  if (!is.numeric(conf) || length(conf) != 1L || is.na(conf) ||
      conf <= 0 || conf >= 1) {
    abort("`conf` must be a number between 0 and 1.")
  }
  labels <- attr(coded, "labels")
  if (is.null(labels) || !length(labels)) {
    abort("`coded` must carry codebook labels in attr(coded, 'labels'); re-run code_corpus().")
  }
  labels <- as.character(labels)
  text_col <- attr(coded, "text")
  if (!is.character(text_col) || length(text_col) != 1L || is.na(text_col) ||
      !nzchar(text_col) || !text_col %in% names(coded)) {
    abort("`coded` must carry its text column in attr(coded, 'text'); re-run code_corpus().")
  }
  if (!"label" %in% names(coded)) {
    abort("`coded` must contain a `label` column.")
  }

  llm <- vapply(as.character(coded[["label"]]), .normalize_label,
                character(1), labels = labels, USE.NAMES = FALSE)
  n_parse_failures <- sum(is.na(llm))
  corpus_ok <- !is.na(llm)
  n_corpus <- sum(corpus_ok)
  if (!n_corpus) {
    abort("`coded` has no parsed labels; gold_correct() conditions on parse success.")
  }
  naive <- vapply(labels, function(l) mean(llm[corpus_ok] == l), numeric(1))

  g <- gold_split(gold, "test")

  # Resolve the linkage key. An explicit id, shared by gold_set() and
  # code_corpus(), is the only key that disambiguates units with identical
  # text; absent one, a content hash of the text links robustly but cannot
  # tell true duplicates apart, so duplicates among audited units are refused.
  link <- .resolve_gold_link(coded, gold, g, text_col)
  corpus_key <- link$corpus_key
  gold_key <- link$gold_key

  match_idx <- match(gold_key, corpus_key, incomparables = NA)
  n_unmatched <- sum(is.na(match_idx))
  if (n_unmatched) {
    cli::cli_warn("{n_unmatched} test-split gold unit(s) did not match the coded corpus by {link$by} and were excluded.")
  }
  matched <- !is.na(match_idx)
  if (!any(matched)) {
    abort(sprintf("No test-split gold units matched the coded corpus by %s; the audited units must be part of the coded corpus.",
                  link$by))
  }
  # A matched unit whose key occurs more than once in the corpus cannot be
  # assigned to a specific corpus row. match() would pick the first
  # arbitrarily; refuse instead, whichever key is in use.
  corpus_counts <- table(corpus_key)
  dup_keys <- names(corpus_counts)[corpus_counts > 1L]
  n_dup <- sum(gold_key[matched] %in% dup_keys)
  if (n_dup) {
    if (identical(link$by, "id")) {
      abort(paste0(
        n_dup, " matched audit unit(s) have an id that is duplicated in the coded corpus, ",
        "so they cannot be linked to a specific corpus row by id. ",
        "Make the corpus's `id` column unique before code_corpus()."))
    }
    abort(paste0(
      n_dup, " matched audit unit(s) have text that is duplicated in the corpus, ",
      "so they cannot be linked to a specific corpus row by text. ",
      "Add a stable `id` column to both gold_set() and code_corpus() to disambiguate."))
  }

  gold_lab <- vapply(as.character(g[[gold$labels]][matched]), .normalize_label,
                     character(1), labels = labels, USE.NAMES = FALSE)
  if (anyNA(gold_lab)) {
    abort(sprintf("%d matched gold label(s) are not among the codebook labels on `coded`.",
                  sum(is.na(gold_lab))))
  }
  llm_audit <- llm[match_idx[matched]]
  audit_ok <- !is.na(llm_audit)
  n_audit_parse_failures <- sum(!audit_ok)
  if (n_audit_parse_failures) {
    cli::cli_warn("{n_audit_parse_failures} matched audit unit(s) have an NA corpus label and were excluded from the audit pairs.")
  }
  gold_lab <- gold_lab[audit_ok]
  llm_audit <- llm_audit[audit_ok]
  n_audit <- length(llm_audit)
  if (!n_audit) {
    abort("No matched test-split audit units have parsed corpus labels.")
  }
  if (n_audit < 20L) {
    cli::cli_warn("Only {n_audit} audited unit(s); the correction's variance estimate is unstable.")
  }

  accuracy_audit <- mean(llm_audit == gold_lab)
  z <- stats::qnorm(1 - (1 - conf) / 2)
  rows <- lapply(labels, function(l) {
    d <- as.numeric(gold_lab == l) - as.numeric(llm_audit == l)
    corrected <- unname(naive[[l]] + mean(d))
    se <- if (n_audit == n_corpus) 0 else {
      s2 <- if (n_audit > 1L) stats::var(d) else NA_real_
      sqrt((1 - n_audit / n_corpus) * s2 / n_audit)
    }
    tibble::tibble(category = l,
                   share_naive = unname(naive[[l]]),
                   share_corrected = corrected,
                   se = se,
                   ci_lo = corrected - z * se,
                   ci_hi = corrected + z * se)
  })
  tab <- do.call(rbind, rows)

  protocol_hash <- attr(coded, "protocol_hash") %||% NA_character_
  protocol_label <- attr(coded, "protocol_label") %||% NA_character_
  if (isTRUE(gold$sealed)) {
    .gold_ledger_append(gold, "test", protocol_hash, protocol_label,
                        n_audit, accuracy_audit)
  }

  structure(
    list(table = tab,
         n_corpus = as.integer(n_corpus),
         n_parse_failures = as.integer(n_parse_failures),
         n_audit = as.integer(n_audit),
         n_audit_parse_failures = as.integer(n_audit_parse_failures),
         accuracy_audit = accuracy_audit,
         protocol_hash = protocol_hash,
         protocol_label = protocol_label,
         conf = conf,
         link_by = link$by,
         sealed = isTRUE(gold$sealed)),
    class = "gold_correction"
  )
}

# Internal: print-safe number formatting (NA stays "NA").
.gc_fmt <- function(x) ifelse(is.na(x), "NA", sprintf("%.3f", x))

#' @export
print.gold_correction <- function(x, ...) {
  cat("<gold_correction | naive vs corrected category prevalences>\n")
  for (i in seq_len(nrow(x$table))) {
    r <- x$table[i, ]
    cat(sprintf("  %s: naive %s | corrected %s | SE %s | %.0f%% CI [%s, %s]\n",
                r$category, .gc_fmt(r$share_naive), .gc_fmt(r$share_corrected),
                .gc_fmt(r$se), 100 * x$conf, .gc_fmt(r$ci_lo), .gc_fmt(r$ci_hi)))
  }
  cat(sprintf("  audit n = %d | audit accuracy %s | corpus parse failures = %d%s | protocol %s\n",
              x$n_audit, .gc_fmt(x$accuracy_audit), x$n_parse_failures,
              if (x$n_audit_parse_failures > 0L)
                sprintf(" | audit pairs excluded for NA labels = %d",
                        x$n_audit_parse_failures) else "",
              substr(x$protocol_hash, 1, 12)))
  cat(sprintf("  The audit uses the %stest split, linked into the corpus by %s.\n",
              if (isTRUE(x$sealed)) "sealed " else "", x$link_by %||% "text hash"))
  outside <- x$table$share_corrected < 0 | x$table$share_corrected > 1
  if (any(outside, na.rm = TRUE)) {
    cat("  Corrected shares outside [0, 1] signal an unstable audit correction.\n")
  }
  invisible(x)
}

#' Coerce a gold correction to a tibble
#'
#' Returns the category-level correction table.
#'
#' @param x A `gold_correction` object.
#' @param ... Passed to [tibble::as_tibble()].
#' @return A tibble with `category`, `share_naive`, `share_corrected`, `se`,
#'   `ci_lo`, and `ci_hi`.
#' @exportS3Method tibble::as_tibble
as_tibble.gold_correction <- function(x, ...) {
  tibble::as_tibble(x$table, ...)
}

#' Gold-correction diagnostics
#'
#' Returns the machine-readable correction table.
#'
#' @param x A `gold_correction` object.
#' @param ... Ignored.
#' @return A tibble with `category`, `share_naive`, `share_corrected`, `se`,
#'   `ci_lo`, and `ci_hi`.
#' @exportS3Method LLMR::diagnostics
diagnostics.gold_correction <- function(x, ...) {
  tibble::as_tibble(x$table)
}
