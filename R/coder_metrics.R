# coder_metrics.R ---------------------------------------------------------------------
# Validation metrics computed from parsed labels against gold. Parse failures
# (NA) count as errors, never as silently dropped rows: an instrument that
# does not answer is a failing instrument.

# Internal: accuracy, macro-F1, per-category precision/recall/F1, confusion.
.score_labels <- function(pred, gold, labels) {
  stopifnot(length(pred) == length(gold))
  n <- length(gold)
  per <- lapply(labels, function(l) {
    tp <- sum(pred == l & gold == l, na.rm = TRUE)
    fp <- sum(pred == l & gold != l, na.rm = TRUE)
    fn <- sum((pred != l | is.na(pred)) & gold == l, na.rm = TRUE)
    prec <- if (tp + fp == 0) NA_real_ else tp / (tp + fp)
    rec  <- if (tp + fn == 0) NA_real_ else tp / (tp + fn)
    f1   <- if (is.na(prec) || is.na(rec) || prec + rec == 0) NA_real_
            else 2 * prec * rec / (prec + rec)
    tibble::tibble(label = l, n_gold = sum(gold == l), precision = prec,
                   recall = rec, f1 = f1)
  })
  per <- do.call(rbind, per)
  confusion <- table(gold = factor(gold, levels = labels),
                     predicted = factor(pred, levels = c(labels, NA)),
                     useNA = "ifany")
  # macro-F1 scores an undefined per-category F1 (a category the model never
  # predicted, or that gold never contains) as 0, the standard convention --
  # dropping it would reward never predicting a hard category
  f1_zero <- ifelse(is.na(per$f1), 0, per$f1)
  list(
    n = n,
    accuracy = mean((pred == gold) %in% TRUE),
    parse_failures = sum(is.na(pred)),
    macro_f1 = mean(f1_zero),
    per_category = per,
    confusion = confusion
  )
}

# Internal: bootstrap CI for accuracy (percentile).
.acc_ci <- function(pred, gold, conf = 0.95, reps = 2000) {
  n <- length(gold)
  hit <- (pred == gold) %in% TRUE
  boots <- vapply(seq_len(reps), function(i) mean(hit[sample.int(n, n, TRUE)]),
                  numeric(1))
  stats::quantile(boots, c((1 - conf) / 2, 1 - (1 - conf) / 2), names = FALSE)
}

#' Agreement among coders or replicates
#'
#' A thin, shape-aware wrapper around `LLMR::llm_agreement()`. Pass either a
#' [gold_set()] whose `coders` columns hold individual human codings (the
#' human-human reliability a methods section must report), or any data frame
#' plus the label columns to compare (e.g. model replicates from
#' [code_corpus()], or one human column and one model column).
#'
#' @param x A [gold_set()], a [code_corpus()] result, or a data frame.
#' @param cols For data frames: character vector of label columns. Ignored
#'   for gold sets (their `coders` columns are used).
#' @return An `llmr_agreement` object (mean pairwise agreement,
#'   Krippendorff's alpha, per-unit majorities); see
#'   `LLMR::llm_agreement()`.
#' @export
coder_agreement <- function(x, cols = NULL) {
  if (inherits(x, "gold_set")) {
    if (is.null(x$coders) || length(x$coders) < 2L) {
      abort("This gold set has no `coders` columns; nothing to agree on.")
    }
    return(LLMR::llm_agreement(x$data, cols = x$coders))
  }
  if (inherits(x, "coded_corpus")) x <- x$data
  stopifnot(is.data.frame(x), is.character(cols), length(cols) >= 2L)
  LLMR::llm_agreement(x, cols = cols)
}
