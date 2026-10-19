# coder_gold.R ------------------------------------------------------------------------
# Gold sets carry split provenance and a sealed holdout split (named "test"
# unless the caller chooses otherwise). "Sealed" is honesty-by-visibility,
# not DRM: nothing stops an evaluation on the holdout split, but every one
# of them is appended to a ledger that lives inside the object (an
# environment, so appends survive without reassignment) and is printed in
# every report. One look is a validation; five looks are visible as five
# looks.

#' Build a gold-standard set with split provenance and a sealed holdout split
#'
#' @param data A data frame of human-labeled units. Gold labels must be
#'   complete: a missing label is not gold, and construction fails on `NA`s.
#' @param text Name of the text column (character scalar).
#' @param label Name of the (adjudicated) label column.
#' @param split Named numeric vector of proportions, e.g.
#'   `c(dev = 0.6, test = 0.4)`. The names must be unique and nonempty, and
#'   the proportions finite, nonnegative, and summing to 1. Sizes are allocated by
#'   largest remainder (exact, no rounding loss); assignment is random
#'   within the allocation -- set a seed beforehand for a repeatable draw,
#'   and keep the saved object either way, since the split is stored, not
#'   recomputed. Split names are yours to choose; name the held-out one in
#'   `holdout` when it is not `"test"`.
#' @param holdout Name of the held-out split (character scalar, default
#'   `"test"`). This is the split that [validate_protocol()] evaluates by
#'   default, that [gold_correct()] audits, that [tune_protocol()] refuses,
#'   and that the seal and the ledger act on. The name is stored on the
#'   object, so downstream functions follow it without repetition.
#' @param stratify If TRUE (default), the split is stratified by the label
#'   column, so every split carries the same class composition -- the methods
#'   default for evaluation splits.
#' @param seal_holdout If TRUE (default), the holdout split is *sealed*:
#'   every [validate_protocol()] or [gold_correct()] run against it is
#'   recorded in the ledger and printed by `LLMR::report()`. The seal is
#'   visibility, not enforcement; save the gold set with the study and
#'   archive the LLM call log (see the archive workflow) when tamper
#'   evidence is needed.
#' @param coders Optional character vector naming columns holding individual
#'   coder labels (pre-adjudication), used by [coder_agreement()].
#' @param id Optional name of a column holding a stable unit identifier. When
#'   supplied, [gold_correct()] links audit units to the coded corpus by this
#'   id, which is the only way to disambiguate units that share identical text.
#'   The same column must be present in the `corpus` passed to [code_corpus()].
#'   Its values must be unique and nonmissing. When omitted, linkage falls back to a content hash of the text, and
#'   duplicate texts among audited units are refused rather than matched
#'   arbitrarily.
#' @return A `gold_set`: the data plus split assignment, the stored holdout
#'   split name, seal status, and an evaluation ledger.
#' @examples
#' set.seed(110)   # the split assignment draws locally
#' g <- gold_set(
#'   data.frame(text  = paste0("unit", seq_len(40)),
#'              label = rep(c("x", "y"), each = 20)),
#'   text = "text", label = "label", split = c(dev = 0.5, test = 0.5)
#' )
#' g
#' table(gold_split(g, "dev")$label)   # stratified: same class mix as test
#' gold_ledger(g)   # empty until something evaluates on the holdout split
#' @seealso [validate_protocol()], [gold_ledger()], [coder_agreement()]
#' @export
gold_set <- function(data, text, label, split = c(dev = 0.6, test = 0.4),
                     holdout = "test", stratify = TRUE, seal_holdout = TRUE,
                     coders = NULL, id = NULL) {
  stopifnot(is.data.frame(data), nrow(data) >= 2L)
  for (col in c(text, label, coders, id)) {
    if (!col %in% names(data)) abort(sprintf("Column '%s' not found in `data`.", col))
  }
  data <- tibble::as_tibble(data)
  if (anyNA(data[[text]])) {
    abort(sprintf("%d row(s) of column '%s' are NA; a missing text cannot be gold.",
                  sum(is.na(data[[text]])), text))
  }
  data$.text_hash <- .text_hash(data[[text]])
  if (!is.null(id)) .check_unit_ids(data[[id]], id, "`data`")
  .check_split_spec(split)
  if (!is.character(holdout) || length(holdout) != 1L || is.na(holdout) ||
      !nzchar(holdout)) {
    abort("`holdout` must be a single, non-empty split name.")
  }
  # The seal, the ledger, validate_protocol(), and gold_correct() all address
  # the holdout split; a seal with no such split would be vacuous, so say so.
  if (isTRUE(seal_holdout) && !holdout %in% names(split)) {
    cli::cli_warn(paste0(
      "`seal_holdout = TRUE` but no split is named '", holdout, "', the declared ",
      "holdout; the seal, the ledger, validate_protocol(), and gold_correct() ",
      "all act on the holdout split, so nothing will be sealed or ledgered. ",
      "Name one split '", holdout, "', point `holdout` at an existing split, ",
      "or set seal_holdout = FALSE."))
  }
  if (anyNA(data[[label]])) {
    abort("Gold labels contain NA; a missing label is not gold. Adjudicate or drop those units first.")
  }
  n <- nrow(data)
  assignment <- character(n)
  if (isTRUE(stratify)) {
    for (cls in unique(data[[label]])) {
      idx <- which(data[[label]] == cls)
      assignment[idx] <- sample(.alloc_split(length(idx), split))
    }
  } else {
    assignment <- sample(.alloc_split(n, split))
  }
  n_holdout <- sum(assignment == holdout)
  if (holdout %in% names(split) && n_holdout < 20L) {
    cli::cli_warn(paste(
      "The holdout split ('{holdout}') has only {n_holdout} unit(s); accuracy",
      "intervals will be wide. gold_size() helps plan a defensible size."))
  }
  ledger <- new.env(parent = emptyenv())
  ledger$rows <- list()
  structure(
    list(data = data,
         text = text, label = label, coders = coders, id = id,
         split = assignment, holdout = holdout, sealed = isTRUE(seal_holdout),
         ledger = ledger,
         created = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")),
    class = "gold_set"
  )
}

# Internal: refuse missing or repeated unit identifiers, naming them.
.check_unit_ids <- function(values, id, where) {
  missing <- sum(is.na(values))
  repeated <- unique(values[!is.na(values) & duplicated(values)])
  if (!missing && !length(repeated)) return(invisible(NULL))
  problems <- c(
    if (missing) sprintf("%d row(s) have no id", missing),
    if (length(repeated)) {
      sprintf("%d id(s) repeat (%s)", length(repeated), .quote_some(repeated))
    })
  abort(sprintf("`id` column '%s' must have no NA and no duplicate values in %s: %s.",
                id, where, paste(problems, collapse = "; ")))
}

# Internal: refuse a split specification that cannot be allocated as stated.
.check_split_spec <- function(split) {
  labels <- names(split)
  if (!is.numeric(split) || !length(split) || is.null(labels)) {
    abort("`split` must be a named vector of proportions summing to 1.")
  }
  unnamed <- is.na(labels) | !nzchar(trimws(labels))
  if (any(unnamed)) {
    abort(sprintf("`split` entries %s have no name; name every split.",
                  paste(which(unnamed), collapse = ", ")))
  }
  repeated <- unique(labels[duplicated(labels)])
  if (length(repeated)) {
    abort(sprintf("`split` names must be unique; %s appear(s) more than once.",
                  .quote_some(repeated)))
  }
  bad <- !is.finite(split) | split < 0
  if (any(bad)) {
    abort(sprintf("`split` proportions must be finite and nonnegative: %s.",
                  paste(sprintf("%s = %s", labels[bad], format(split[bad])),
                        collapse = ", ")))
  }
  if (abs(sum(split) - 1) > 1e-8) {
    abort(sprintf("`split` must be a named vector of proportions summing to 1; these sum to %s.",
                  format(sum(split))))
  }
  invisible(NULL)
}

# Internal: quote the first few values for an error message.
.quote_some <- function(x, n = 5L) {
  shown <- paste0("'", utils::head(as.character(x), n), "'", collapse = ", ")
  if (length(x) > n) paste0(shown, ", ...") else shown
}

# Internal: the holdout split name stored on a gold set.
.gold_holdout <- function(x) x$holdout

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
  cat(sprintf("<gold_set | %d units | %s | holdout '%s' %s | %d ledgered evaluation(s)>\n",
              nrow(x$data),
              paste(sprintf("%s=%d", names(tab), as.integer(tab)), collapse = ", "),
              .gold_holdout(x),
              if (x$sealed) "SEALED" else "unsealed",
              length(x$ledger$rows)))
  invisible(x)
}

#' Rows of a gold set belonging to one split
#' @param x A [gold_set()].
#' @param split `"dev"` or `"test"` (or any split name used at creation).
#' @return A tibble of that split's rows.
#' @examples
#' set.seed(110)
#' g <- gold_set(
#'   data.frame(text = paste("unit", 1:40),
#'              label = rep(c("x", "y"), each = 20)),
#'   "text", "label", split = c(dev = 0.5, test = 0.5))
#' head(gold_split(g, "dev"))
#' @export
gold_split <- function(x, split = "dev") {
  stopifnot(inherits(x, "gold_set"))
  if (!split %in% unique(x$split)) {
    counts <- table(x$split, x$data[[x$label]])
    allocated <- vapply(rownames(counts), function(s) {
      sprintf("%s: %s", s, paste(sprintf("%s %d", colnames(counts), counts[s, ]),
                                 collapse = ", "))
    }, character(1))
    abort(c(sprintf("No split named '%s' in this gold set.", split),
            i = paste("Units by split and label:", paste(allocated, collapse = "; "))))
  }
  x$data[x$split == split, , drop = FALSE]
}

#' The evaluation ledger of a sealed gold set
#'
#' One row per evaluation that ever touched the sealed holdout split: when,
#' by which protocol (hash), and with what headline result. Entries are
#' appended automatically by [validate_protocol()] and [gold_correct()] and
#' printed in full by `LLMR::report()`. The mechanism is visibility, not
#' enforcement -- an in-memory object cannot stop a determined user, and
#' does not pretend to; archive the study's LLM call log (see the archive
#' workflow) when tamper evidence is needed.
#'
#' @param x A [gold_set()].
#' @return A tibble: `ts`, `split`, `protocol_hash`, `protocol_label`, `n`,
#'   `accuracy`.
#' @examples
#' set.seed(110)
#' g <- gold_set(
#'   data.frame(text = paste("unit", 1:40),
#'              label = rep(c("x", "y"), each = 20)),
#'   "text", "label", split = c(dev = 0.5, test = 0.5))
#' gold_ledger(g)
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
#' @return A `gold_size` object with `recommended_size` and a `candidates`
#'   tibble containing `n`, `mean_ci_width`, and `meets_target`.
#' @examples
#' set.seed(110)
#' gold_size(expected_agreement = 0.85, ci_width = 0.10)
#' @export
gold_size <- function(expected_agreement = 0.85, ci_width = 0.10,
                      conf = 0.95, n_grid = c(50, 100, 200, 300, 500, 800),
                      sims = 2000) {
  stopifnot(expected_agreement > 0, expected_agreement < 1,
            is.numeric(n_grid), length(n_grid) >= 1L,
            all(is.finite(n_grid)), all(n_grid >= 1),
            all(n_grid == floor(n_grid)))
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
    recommended <- max(n_grid)
  } else {
    recommended <- min(n_grid[ok])
  }
  structure(
    list(
      recommended_size = as.integer(recommended),
      candidates = tibble::tibble(
        n = as.integer(n_grid),
        mean_ci_width = as.numeric(widths),
        meets_target = as.logical(widths <= ci_width)
      )
    ),
    class = "gold_size"
  )
}

#' @export
print.gold_size <- function(x, ...) {
  cat(sprintf("<gold_size | recommended n = %d | %d candidate(s)>\n",
              x$recommended_size, nrow(x$candidates)))
  invisible(x)
}
