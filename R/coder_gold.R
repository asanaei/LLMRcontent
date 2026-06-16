# coder_gold.R ------------------------------------------------------------------------
# Gold sets carry split provenance and a sealed test split. "Sealed" is
# honesty-by-visibility, not DRM: nothing stops an evaluation on the test
# split, but every one of them is appended to a ledger that lives inside the
# object (an environment, so appends survive without reassignment) and is
# printed in every report. One look is a validation; five looks are visible
# as five looks.

#' Build a gold-standard set with split provenance and a sealed test split
#'
#' @param data A data frame of human-labeled units. Gold labels must be
#'   complete: a missing label is not gold, and construction fails on `NA`s.
#' @param text Name of the text column (character scalar).
#' @param labels Name of the (adjudicated) label column.
#' @param split Named numeric vector of proportions, e.g.
#'   `c(dev = 0.6, test = 0.4)`. Must sum to 1. Sizes are allocated by
#'   largest remainder (exact, no rounding loss); assignment is random
#'   within the allocation -- set a seed beforehand for a repeatable draw,
#'   and keep the saved object either way, since the split is stored, not
#'   recomputed.
#' @param stratify If TRUE (default), the split is stratified by the label
#'   column, so dev and test carry the same class composition -- the methods
#'   default for evaluation splits.
#' @param seal_test If TRUE (default), the test split is *sealed*:
#'   every [validate_protocol()] run against it is recorded in the ledger
#'   and printed by [coding_report()]. The seal is visibility, not
#'   enforcement; save the gold set with the study and archive the LLM call
#'   log (see the archive workflow) when tamper evidence is needed.
#' @param coders Optional character vector naming columns holding individual
#'   coder labels (pre-adjudication), used by [coder_agreement()].
#' @param id Optional name of a column holding a stable unit identifier. When
#'   supplied, [gold_correct()] links audit units to the coded corpus by this
#'   id, which is the only way to disambiguate units that share identical text.
#'   The same column must be present in the `corpus` passed to [code_corpus()].
#'   When omitted, linkage falls back to a content hash of the text, and
#'   duplicate texts among audited units are refused rather than matched
#'   arbitrarily.
#' @return A `gold_set`: the data plus split assignment, seal status, and an
#'   evaluation ledger.
#' @examples
#' set.seed(110)   # the split assignment draws locally
#' g <- gold_set(
#'   data.frame(text  = paste0("unit", seq_len(40)),
#'              label = rep(c("x", "y"), each = 20)),
#'   text = "text", labels = "label", split = c(dev = 0.5, test = 0.5)
#' )
#' g
#' table(gold_split(g, "dev")$label)   # stratified: same class mix as test
#' gold_ledger(g)   # empty until something evaluates on the test split
#' @seealso [validate_protocol()], [gold_ledger()], [coder_agreement()]
#' @export
gold_set <- function(data, text, labels, split = c(dev = 0.6, test = 0.4),
                     stratify = TRUE, seal_test = TRUE, coders = NULL,
                     id = NULL) {
  stopifnot(is.data.frame(data), nrow(data) >= 2L)
  for (col in c(text, labels, coders, id)) {
    if (!col %in% names(data)) abort(sprintf("Column '%s' not found in `data`.", col))
  }
  data <- tibble::as_tibble(data)
  data$.text_hash <- .text_hash(data[[text]])
  if (!is.null(id)) {
    if (anyNA(data[[id]]) || anyDuplicated(data[[id]])) {
      abort(sprintf("`id` column '%s' must have no NA and no duplicate values.", id))
    }
  }
  if (is.null(names(split)) || abs(sum(split) - 1) > 1e-8) {
    abort("`split` must be a named vector of proportions summing to 1.")
  }
  if (anyNA(data[[labels]])) {
    abort("Gold labels contain NA; a missing label is not gold. Adjudicate or drop those units first.")
  }
  n <- nrow(data)
  assignment <- character(n)
  if (isTRUE(stratify)) {
    for (cls in unique(data[[labels]])) {
      idx <- which(data[[labels]] == cls)
      assignment[idx] <- sample(.alloc_split(length(idx), split))
    }
  } else {
    assignment <- sample(.alloc_split(n, split))
  }
  n_test <- sum(assignment == "test")
  if ("test" %in% names(split) && n_test < 20L) {
    cli::cli_warn(paste(
      "The test split has only {n_test} unit(s); accuracy intervals will be",
      "wide. gold_size() helps plan a defensible size."))
  }
  ledger <- new.env(parent = emptyenv())
  ledger$rows <- list()
  structure(
    list(data = data,
         text = text, labels = labels, coders = coders, id = id,
         split = assignment, sealed = isTRUE(seal_test),
         ledger = ledger,
         created = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")),
    class = "gold_set"
  )
}

# Internal: content hash of normalized text. Linkage that survives whitespace
# and trivial preprocessing drift, and is explicit about what was matched.
# Uses the ecosystem's one hash convention, vectorized over the input.
.text_hash <- function(x) {
  norm <- gsub("\\s+", " ", trimws(as.character(x)))
  vapply(norm, function(s) LLMR::llm_hash(s), character(1), USE.NAMES = FALSE)
}

# Internal: largest-remainder allocation of n units to named proportions --
# exact (sums to n), honors the proportions as closely as integers allow.
.alloc_split <- function(n, split) {
  raw <- split * n
  base <- floor(raw)
  short <- n - sum(base)
  if (short > 0) {
    extra <- order(raw - base, decreasing = TRUE)[seq_len(short)]
    base[extra] <- base[extra] + 1
  }
  rep(names(split), times = base)
}

#' @export
print.gold_set <- function(x, ...) {
  tab <- table(x$split)
  cat(sprintf("<gold_set | %d units | %s | test split %s | %d ledgered evaluation(s)>\n",
              nrow(x$data),
              paste(sprintf("%s=%d", names(tab), as.integer(tab)), collapse = ", "),
              if (x$sealed) "SEALED" else "unsealed",
              length(x$ledger$rows)))
  invisible(x)
}

#' Rows of a gold set belonging to one split
#' @param x A [gold_set()].
#' @param split `"dev"` or `"test"` (or any split name used at creation).
#' @return A tibble of that split's rows.
#' @export
gold_split <- function(x, split = "dev") {
  stopifnot(inherits(x, "gold_set"))
  if (!split %in% unique(x$split)) {
    abort(sprintf("No split named '%s' in this gold set.", split))
  }
  x$data[x$split == split, , drop = FALSE]
}

#' The evaluation ledger of a sealed gold set
#'
#' One row per evaluation that ever touched the sealed test split: when, by
#' which protocol (hash), and with what headline result. Entries are
#' appended automatically by [validate_protocol()] and printed in full by
#' [coding_report()]. The mechanism is visibility, not enforcement -- an
#' in-memory object cannot stop a determined user, and does not pretend to;
#' archive the study's LLM call log (see the archive workflow) when tamper
#' evidence is needed.
#'
#' @param x A [gold_set()].
#' @return A tibble: `ts`, `split`, `protocol_hash`, `protocol_label`, `n`,
#'   `accuracy`.
#' @export
gold_ledger <- function(x) {
  stopifnot(inherits(x, "gold_set"))
  rows <- x$ledger$rows
  if (!length(rows)) {
    return(tibble::tibble(ts = character(0), split = character(0),
                          protocol_hash = character(0),
                          protocol_label = character(0),
                          n = integer(0), accuracy = numeric(0)))
  }
  do.call(rbind, lapply(rows, tibble::as_tibble))
}

# Internal: append one evaluation event (by reference, via the env).
.gold_ledger_append <- function(gold, split, protocol_hash, protocol_label,
                                n, accuracy) {
  gold$ledger$rows[[length(gold$ledger$rows) + 1L]] <- list(
    ts = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    split = split,
    protocol_hash = protocol_hash %||% NA_character_,
    protocol_label = protocol_label %||% NA_character_,
    n = as.integer(n), accuracy = as.numeric(accuracy))
  invisible(NULL)
}

#' Plan the size of a gold set
#'
#' Answers, by simulation, the budgeting question every project starts with:
#' how many human-labeled units do I need so that the agreement estimate
#' (proportion of model labels matching gold) has a confidence interval no
#' wider than `ci_width`? The simulation draws from a binomial at the
#' anticipated agreement level, which is adequate for planning; report the
#' realized interval from [validate_protocol()] in the paper.
#'
#' @param expected_agreement Anticipated model-gold agreement (default 0.85).
#' @param ci_width Target total width of the 95% interval (default 0.10).
#' @param conf Confidence level (default 0.95).
#' @param n_grid Candidate sizes to evaluate.
#' @param sims Monte Carlo draws per candidate size.
#' @return The smallest n in `n_grid` meeting the target, with the simulated
#'   widths as an attribute.
#' @examples
#' set.seed(110)
#' gold_size(expected_agreement = 0.85, ci_width = 0.10)
#' @export
gold_size <- function(expected_agreement = 0.85, ci_width = 0.10,
                      conf = 0.95, n_grid = c(50, 100, 200, 300, 500, 800),
                      sims = 2000) {
  stopifnot(expected_agreement > 0, expected_agreement < 1)
  widths <- vapply(n_grid, function(n) {
    hits <- stats::rbinom(sims, n, expected_agreement)
    w <- vapply(hits, function(h) {
      ci <- stats::binom.test(h, n, conf.level = conf)$conf.int
      diff(ci)
    }, numeric(1))
    mean(w)
  }, numeric(1))
  ok <- which(widths <= ci_width)
  if (!length(ok)) {
    cli::cli_warn("No candidate size meets the target; largest n returned.")
    ok <- length(n_grid)
  }
  out <- n_grid[min(ok)]
  attr(out, "widths") <- stats::setNames(widths, n_grid)
  out
}
