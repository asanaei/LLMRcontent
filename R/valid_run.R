# valid_run.R -------------------------------------------------------------------------
# Executing the grid and recomputing the estimand under every cell. One
# runner call for the whole grid (LLMR::call_llm_par by default; injectable
# for offline tests).

#' Run the audit grid
#'
#' Crosses prompts x models x label orders x temperatures, measures every
#' unit under every cell, recomputes the estimator per cell, and returns the
#' audit frame the stability functions consume. Cell 1 -- baseline prompt,
#' first model, `"as_given"`, first temperature -- is the reference for
#' [audit_fragility()].
#'
#' @param plan An [audit_plan()] with at least one model.
#' @param .runner Internal seam for tests: `function(experiments, ...)`
#'   returning the experiments with a `response_text` column. Default
#'   `LLMR::call_llm_par()`.
#' @param ... Passed to the runner (e.g. `tries`, `progress`).
#' @return An `audit`: a tibble with one row per cell -- `cell`,
#'   `prompt`, `model`, `label_order`, `temperature`, `estimate`,
#'   `parse_failures`, `tokens` (when the runner reports usage) -- with the
#'   plan and the unit-level table (see [audit_units()]) as attributes.
#'   Estimator errors inside a cell yield `estimate = NA` for that cell.
#' @examples
#' \dontrun{
#' speeches <- data.frame(
#'   text = c("cut taxes now", "deregulate markets",
#'            "fund the schools", "expand care"))
#' plan <- audit_plan(
#'   data = speeches, text = "text",
#'   estimator = function(d) mean(d$label == "conservative", na.rm = TRUE),
#'   labels = c("conservative", "progressive"),
#'   prompt = "Classify as one of: {labels}.\n\n{text}\n\nLabel:")
#' plan <- audit_add_models(plan,
#'   list(oss = LLMR::llm_config("groq", "openai/gpt-oss-20b", temperature = 0)))
#' audit <- audit_run(plan)
#' audit
#' audit_stability(audit)
#' audit_fragility(audit)
#' head(audit_units(audit))
#' }
#'
#' # The `.runner` seam answers the grid without a provider, for tests or for
#' # a deterministic or external coder. The same plan, scored offline:
#' speeches <- data.frame(
#'   text = c("cut taxes now", "deregulate markets",
#'            "fund the schools", "expand care"))
#' plan <- audit_plan(
#'   data = speeches, text = "text",
#'   estimator = function(d) mean(d$label == "conservative", na.rm = TRUE),
#'   labels = c("conservative", "progressive"),
#'   prompt = "Classify as one of: {labels}.\n\n{text}\n\nLabel:")
#' plan <- audit_add_models(plan,
#'   list(oss = LLMR::llm_config("groq", "openai/gpt-oss-20b", temperature = 0)))
#' keyword_coder <- function(experiments, ...) {
#'   msg <- vapply(experiments$messages, `[[`, "", "user")
#'   experiments$response_text <- ifelse(grepl("taxes|deregulate", msg),
#'                                       "conservative", "progressive")
#'   experiments
#' }
#' audit <- audit_run(plan, .runner = keyword_coder)
#' audit_stability(audit)
#' @export
audit_run <- function(plan, .runner = NULL, ...) {
  stopifnot(inherits(plan, "audit_plan"))
  if (!length(plan$models)) {
    abort("The plan has no models; audit_add_models() first.")
  }
  runner <- .runner %||% function(experiments, ...) {
    LLMR::call_llm_par(experiments, ...)
  }

  grid <- expand.grid(
    prompt = names(plan$prompts),
    model = names(plan$models),
    label_order = plan$label_orders,
    temperature = plan$temperatures,
    stringsAsFactors = FALSE
  )
  texts <- plan$data[[plan$text]]

  rows <- list()
  for (g in seq_len(nrow(grid))) {
    cfg <- plan$models[[grid$model[g]]]
    cfg$model_params$temperature <- grid$temperature[g]
    for (i in seq_along(texts)) {
      rows[[length(rows) + 1L]] <- tibble::tibble(
        cell = g, unit_id = i,
        # Raw unit text as a metadata column, so an injected runner can key on
        # the unit rather than the full rendered prompt (which carries the
        # labels); LLMR::call_llm_par() passes it through.
        text = as.character(texts[[i]]),
        config = list(cfg),
        messages = list(c(user = .render_audit_prompt(
          plan$prompts[[grid$prompt[g]]], texts[[i]],
          plan$labels, grid$label_order[g]))))
    }
  }
  exps <- do.call(rbind, rows)
  res <- runner(exps, ...)
  stopifnot(is.data.frame(res), "response_text" %in% names(res))
  res$label <- vapply(res$response_text, .normalize_label, character(1),
                      labels = plan$labels, USE.NAMES = FALSE)

  out <- do.call(rbind, lapply(seq_len(nrow(grid)), function(g) {
    ri <- res[res$cell == g, ]
    ri <- ri[order(ri$unit_id), ]
    d <- plan$data
    d$label <- ri$label
    est <- tryCatch(as.numeric(plan$estimator(d))[1], error = function(e) NA_real_)
    tok <- intersect(c("sent_tokens", "rec_tokens"), names(ri))
    tibble::tibble(
      cell = g, prompt = grid$prompt[g], model = grid$model[g],
      label_order = grid$label_order[g], temperature = grid$temperature[g],
      estimate = est, parse_failures = sum(is.na(ri$label)),
      tokens = if (length(tok)) as.integer(sum(unlist(ri[tok]), na.rm = TRUE))
               else NA_integer_)
  }))
  unit_cols <- intersect(c("cell", "unit_id", "label", "response_id"),
                         names(res))
  attr(out, "units") <- tibble::as_tibble(res[order(res$cell, res$unit_id),
                                              unit_cols])
  attr(out, "plan") <- plan
  class(out) <- c("audit", class(out))
  out
}

#' The unit-level trail behind an audit
#'
#' One row per unit per cell: which label each text received under each
#' specification (plus `response_id` when the runner reported one, which is
#' the join key into an archive (see the archive workflow)). Cell summaries answer "is the
#' estimate stable"; this table answers "*which units* moved when it was
#' not" -- and without it an audit is unfalsifiable.
#'
#' @param audit An [audit_run()] result.
#' @return A tibble: `cell`, `unit_id`, `label`, and `response_id` when
#'   available.
#' @export
audit_units <- function(audit) {
  stopifnot(inherits(audit, "audit"))
  attr(audit, "units")
}

#' Coerce an audit to a tibble
#'
#' @param x An [audit_run()] result.
#' @param ... Passed to [tibble::as_tibble()].
#' @return The per-cell tibble with the extra `audit` class and audit
#'   attributes stripped.
#' @exportS3Method tibble::as_tibble audit
as_tibble.audit <- function(x, ...) {
  class(x) <- setdiff(class(x), "audit")
  attr(x, "units") <- NULL
  attr(x, "plan") <- NULL
  tibble::as_tibble(x, ...)
}

#' Stability of the estimate across the grid
#'
#' @param audit An [audit_run()] result.
#' @param reference Cell number whose estimate anchors the sign comparison
#'   (default 1, the baseline cell).
#' @return A one-row tibble: `n_cells`, `reference_estimate`,
#'   `sign_agreement` (share of cells whose estimate has the reference's
#'   sign), `min`, `median`, `max`, `iqr`, `n_failed_cells` (estimator
#'   returned NA), and `status` (`"ok"`, or `"no_valid_estimates"` when no
#'   cell produced an estimate, or `"reference_failed"` when the reference
#'   cell's estimate is NA, in which case the summary columns are NA).
#' @export
audit_stability <- function(audit, reference = 1L) {
  stopifnot(inherits(audit, "audit"))
  e <- audit$estimate
  ref_rows <- which(audit$cell == reference)
  if (!length(ref_rows)) {
    abort(sprintf("Reference cell %s is not among the audit's cells.",
                  as.character(reference)))
  }
  ref <- e[ref_rows[1]]
  ok <- !is.na(e)
  # With no valid estimate (or a missing reference estimate), the stability
  # summary is undefined; return a structured no-estimates row rather than
  # Inf/NaN from min()/max()/mean() over empty input.
  if (!any(ok) || is.na(ref)) {
    return(tibble::tibble(
      n_cells = nrow(audit), reference_estimate = ref,
      sign_agreement = NA_real_, min = NA_real_, median = NA_real_,
      max = NA_real_, iqr = NA_real_, n_failed_cells = sum(!ok),
      status = if (!any(ok)) "no_valid_estimates" else "reference_failed"))
  }
  tibble::tibble(
    n_cells = nrow(audit),
    reference_estimate = ref,
    sign_agreement = mean(sign(e[ok]) == sign(ref)),
    min = min(e, na.rm = TRUE),
    median = stats::median(e, na.rm = TRUE),
    max = max(e, na.rm = TRUE),
    iqr = stats::IQR(e, na.rm = TRUE),
    n_failed_cells = sum(!ok),
    status = "ok"
  )
}

#' The specification curve
#'
#' Cells ordered by their estimate, with the grid coordinates alongside --
#' the table behind the classic specification-curve figure. With `ggplot2`
#' installed and `plot = TRUE`, the estimates are drawn against their rank, with
#' a reference line at zero. The companion specification panel (which cell sits
#' at each rank) is not drawn; the returned tibble carries the `rank` together
#' with every grid column, so you can lay out that panel yourself or read off
#' which specification produced any point.
#'
#' @aliases specification_curve
#' @param audit An [audit_run()] result.
#' @param plot Draw the figure (needs `ggplot2`); default is to draw only
#'   in interactive sessions.
#' @return The ordered tibble (the estimate ranking plus the grid coordinates),
#'   invisibly when plotted.
#' @export
audit_curve <- function(audit, plot = interactive()) {
  stopifnot(inherits(audit, "audit"))
  out <- audit[order(audit$estimate), ]
  out$rank <- seq_len(nrow(out))
  if (isTRUE(plot)) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      cli::cli_warn("Install ggplot2 to draw the curve; returning the data.")
      return(out)
    }
    p <- ggplot2::ggplot(out, ggplot2::aes(x = rank, y = estimate)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, linetype = 2) +
      ggplot2::labs(x = "specification (ranked)", y = "estimate",
                    title = "Measurement multiverse") +
      ggplot2::theme_minimal()
    print(p)
    return(invisible(out))
  }
  out
}

#' Conclusion-flip rules for the fragility index
#'
#' What counts as "the conclusion changed" is itself a research decision.
#' [sign_flip()] -- the default -- flags any estimate whose sign differs
#' from the reference's, which suits signed effects with a null at zero.
#' [threshold_flip()] flags estimates crossing a substantive threshold from
#' the reference's side, which suits one-sided claims and bounded
#' quantities, where a sign flip may be impossible or meaningless. Any
#' `function(estimate, reference) -> logical` works in
#' [audit_fragility()]'s `flip` argument.
#'
#' Edge worth knowing: a reference estimate of exactly zero has no sign, so
#' [sign_flip()] flags nothing; choose a threshold rule there.
#'
#' @param at For [threshold_flip()]: the substantive threshold.
#' @return A function `(estimate, reference) -> logical`.
#' @examples
#' rule <- sign_flip()
#' rule(c(-0.2, 0.1, NA), reference = 0.3)
#' rule2 <- threshold_flip(at = 0.5)
#' rule2(c(0.4, 0.6), reference = 0.7)
#' @name flip_rules
NULL

#' @rdname flip_rules
#' @export
sign_flip <- function() {
  function(estimate, reference) {
    if (is.na(reference) || sign(reference) == 0) {
      return(rep(FALSE, length(estimate)))
    }
    !is.na(estimate) & sign(estimate) != sign(reference)
  }
}

#' @rdname flip_rules
#' @export
threshold_flip <- function(at) {
  stopifnot(is.numeric(at), length(at) == 1L)
  function(estimate, reference) {
    !is.na(estimate) & ((reference >= at & estimate < at) |
                          (reference < at & estimate >= at))
  }
}

#' How small a measurement change flips the conclusion?
#'
#' The fragility index is the smallest number of grid dimensions (prompt,
#' model, label order, temperature) that must change from the reference
#' cell before the conclusion -- as defined by the `flip` rule -- changes.
#' `Inf` when no cell in the grid flips, which is a statement about this
#' grid, not a proof of robustness.
#'
#' @param audit An [audit_run()] result.
#' @param reference Reference cell (default 1).
#' @param flip A flip rule (see [flip_rules]); default [sign_flip()].
#' @return Integer (or `Inf`), with the flipping cells in a
#'   `flipping_cells` attribute. When the reference cell's estimate is NA the
#'   value is `Inf` with a `status = "reference_failed"` attribute, since a
#'   failed reference has no sign or threshold to flip against.
#' @export
audit_fragility <- function(audit, reference = 1L, flip = sign_flip()) {
  stopifnot(inherits(audit, "audit"), is.function(flip))
  ref_rows <- which(audit$cell == reference)
  if (!length(ref_rows)) {
    abort(sprintf("Reference cell %s is not among the audit's cells.",
                  as.character(reference)))
  }
  ref <- audit[ref_rows[1], ]
  # A failed reference estimate has no sign/threshold to flip against; fragility
  # is undefined. Report it as Inf (nothing flips) with a status attribute.
  if (is.na(ref$estimate)) {
    out <- Inf
    attr(out, "flipping_cells") <- integer(0)
    attr(out, "status") <- "reference_failed"
    return(out)
  }
  flips <- flip(audit$estimate, ref$estimate)
  flipped <- audit[flips %in% TRUE, ]
  if (!nrow(flipped)) {
    out <- Inf
    attr(out, "flipping_cells") <- integer(0)
    return(out)
  }
  dims <- c("prompt", "model", "label_order", "temperature")
  dist <- vapply(seq_len(nrow(flipped)), function(i) {
    sum(vapply(dims, function(d)
      !identical(flipped[[d]][i], ref[[d]]), logical(1)))
  }, numeric(1))
  out <- as.integer(min(dist))
  attr(out, "flipping_cells") <- flipped$cell[dist == min(dist)]
  out
}

#' Audit diagnostics through LLMR's shared generic
#'
#' Combines [audit_stability()] with [audit_fragility()] so callers using
#' LLMR's shared `diagnostics()` generic receive the same one-row summary.
#'
#' @param x An [audit_run()] result.
#' @param reference Reference cell (default 1).
#' @param flip A flip rule (see [flip_rules]); default [sign_flip()].
#' @param ... Unused; accepted for generic compatibility.
#' @return A one-row tibble with `n_cells`, `reference_estimate`,
#'   `sign_agreement`, `min`, `median`, `max`, `iqr`, `n_failed_cells`, and
#'   `fragility`.
#' @exportS3Method LLMR::diagnostics audit
diagnostics.audit <- function(x, reference = 1L, flip = sign_flip(), ...) {
  s <- audit_stability(x, reference = reference)
  # `status` is a stability-level field; the diagnostics row keeps its stable
  # documented column set and conveys failure through NA values + fragility.
  s$status <- NULL
  f <- audit_fragility(x, reference = reference, flip = flip)
  s$fragility <- if (is.infinite(f)) Inf else as.integer(f)
  s
}

#' Placebo tests for the measurement pipeline
#'
#' Negative controls for the audit. A placebo asks whether the pipeline can
#' produce the reported number when, by construction, it should not: when
#' the link between labels and units is broken, or when the measured texts
#' do not contain the construct. A pipeline that "finds" the effect under
#' either condition is measuring its instrument.
#'
#' @param audit An `audit` returned by [audit_run()].
#' @param type Placebo construction. `"label_permutation"` permutes labels
#'   within each audited cell, with no new model calls.
#'   `"irrelevant_text"` replaces the plan's text column with
#'   researcher-supplied construct-free texts and re-runs the grid.
#' @param reps Number of label permutations per cell for
#'   `"label_permutation"`.
#' @param texts For `"irrelevant_text"`: a character vector of texts in
#'   which the construct is absent by design (weather reports for a
#'   partisanship estimand, say). Choosing them is a research decision the
#'   package cannot make for you. The vector is recycled deterministically
#'   (`rep_len()`) to the number of units.
#' @param .runner Internal seam for tests, passed to [audit_run()] for the
#'   irrelevant-text rerun.
#' @param ... Passed to [audit_run()] for the irrelevant-text rerun.
#' @return An `audit_placebo` object: a list with `type`, `cells`, `reps`,
#'   and `n_units`, with a print method. For `"label_permutation"`,
#'   `cells` has one row per audit cell with the observed `estimate`, the
#'   centered permutation `p`, the null interval (`null_lo`, `null_hi`,
#'   the 2.5% and 97.5% permutation quantiles), a `degenerate` flag, and
#'   permutation counts. For `"irrelevant_text"`, `cells` has the real
#'   `estimate`, `estimate_placebo`, and `parse_failures_placebo` per
#'   cell; the comparison is descriptive, with no p-values.
#' @details
#' The permutation placebo holds each cell's label marginal fixed and
#' shuffles which unit got which label, recomputing the estimator each
#' time. Estimators that use only the marginal (a share, say) are
#' permutation-invariant: every permuted estimate equals the observed one,
#' the cell is flagged `degenerate = TRUE` with `p = NA`, and the print
#' method says the placebo is uninformative for that estimand. For
#' association estimands the permutation distribution is the no-association
#' null, and the p-value is centered on its median:
#' `(1 + #(|perm - m| >= |obs - m|)) / (#valid + 1)`. Estimator errors
#' inside a permutation become `NA`, are excluded, and are counted.
#'
#' Permutations use the current RNG state; the function never sets a seed.
#' Set one beforehand when the draw must be reproducible.
#'
#' The irrelevant-text placebo re-runs the full grid and therefore costs
#' calls unless a `.runner` is injected.
#' @examples
#' \dontrun{
#' speeches <- data.frame(
#'   text = c("cut taxes now", "deregulate markets",
#'            "fund the schools", "expand care"),
#'   half = c("first", "first", "second", "second"))
#' plan <- audit_plan(
#'   data = speeches, text = "text",
#'   estimator = function(d) {
#'     mean(d$label[d$half == "first"] == "conservative", na.rm = TRUE) -
#'       mean(d$label[d$half == "second"] == "conservative", na.rm = TRUE)
#'   },
#'   labels = c("conservative", "progressive"),
#'   prompt = "Classify as one of: {labels}.\n\n{text}\n\nLabel:")
#' plan <- audit_add_models(plan,
#'   list(oss = LLMR::llm_config("groq", "openai/gpt-oss-20b", temperature = 0)))
#' audit <- audit_run(plan)
#'
#' set.seed(110)            # the permutation draws locally
#' audit_placebo(audit, reps = 199L)
#'
#' audit_placebo(audit, type = "irrelevant_text",
#'               texts = c("Rain is likely this afternoon.",
#'                         "Winds stay light overnight."))
#' }
#' @export
audit_placebo <- function(audit, type = c("label_permutation", "irrelevant_text"),
                          reps = 200L, texts = NULL, .runner = NULL, ...) {
  type <- match.arg(type)
  if (!inherits(audit, "audit")) {
    abort("`audit` must be an audit_run() result.")
  }
  plan <- attr(audit, "plan")
  units <- attr(audit, "units")
  if (!inherits(plan, "audit_plan")) {
    abort("`audit` is missing its audit plan.")
  }
  if (!is.data.frame(units) ||
      !all(c("cell", "unit_id", "label") %in% names(units))) {
    abort("`audit` is missing its unit-level labels.")
  }
  n_units <- nrow(plan$data)

  if (identical(type, "label_permutation")) {
    if (!is.numeric(reps) || length(reps) != 1L || is.na(reps) ||
        reps < 1 || reps != floor(reps)) {
      abort("`reps` must be a positive whole number.")
    }
    reps <- as.integer(reps)
    cells <- .placebo_label_permutation(audit, plan, units, reps)
    return(structure(list(type = type, cells = cells, reps = reps,
                          n_units = as.integer(n_units)),
                     class = "audit_placebo"))
  }

  if (is.null(texts)) {
    abort("`texts` is required for type = 'irrelevant_text'.")
  }
  if (!is.character(texts) || length(texts) < 1L || anyNA(texts)) {
    abort("`texts` must be a non-empty character vector with no missing values.")
  }
  plan2 <- plan
  plan2$data <- tibble::as_tibble(plan$data)
  plan2$data[[plan2$text]] <- rep_len(texts, n_units)
  placebo <- audit_run(plan2, .runner = .runner, ...)
  idx <- match(audit$cell, placebo$cell)

  cells <- tibble::tibble(
    cell = audit$cell, prompt = audit$prompt, model = audit$model,
    label_order = audit$label_order, temperature = audit$temperature,
    estimate = audit$estimate,
    estimate_placebo = placebo$estimate[idx],
    parse_failures_placebo = placebo$parse_failures[idx])
  structure(list(type = type, cells = cells, reps = NA_integer_,
                 n_units = as.integer(n_units)),
            class = "audit_placebo")
}

# Internal: per-cell permutation null. The marginal is held fixed; the
# label-unit link is what the shuffle destroys.
.placebo_label_permutation <- function(audit, plan, units, reps) {
  rows <- vector("list", nrow(audit))
  for (i in seq_len(nrow(audit))) {
    g <- audit$cell[i]
    ug <- units[units$cell == g, , drop = FALSE]
    ug <- ug[order(ug$unit_id), , drop = FALSE]
    labels <- ug$label
    if (length(labels) != nrow(plan$data)) {
      abort(sprintf("Unit labels do not match the plan's data for cell %s.", g))
    }
    perm <- rep(NA_real_, reps)
    d <- plan$data
    for (r in seq_len(reps)) {
      d$label <- sample(labels)
      perm[r] <- tryCatch(as.numeric(plan$estimator(d))[1],
                          error = function(e) NA_real_)
    }
    ok <- !is.na(perm)
    obs <- as.numeric(audit$estimate[i])[1]
    p <- null_lo <- null_hi <- NA_real_
    degenerate <- NA
    if (any(ok)) {
      qs <- stats::quantile(perm[ok], probs = c(.025, .975), names = FALSE)
      null_lo <- qs[1]; null_hi <- qs[2]
      if (!is.na(obs)) {
        degenerate <- all(abs(perm[ok] - obs) <= 1e-12)
        if (!degenerate) {
          m <- stats::median(perm[ok])
          p <- (1 + sum(abs(perm[ok] - m) >= abs(obs - m))) / (sum(ok) + 1)
        }
      }
    }
    rows[[i]] <- tibble::tibble(
      cell = audit$cell[i], prompt = audit$prompt[i], model = audit$model[i],
      label_order = audit$label_order[i], temperature = audit$temperature[i],
      estimate = obs, p = p, null_lo = null_lo, null_hi = null_hi,
      degenerate = degenerate,
      n_perm_ok = sum(ok), n_perm_failed = sum(!ok))
  }
  do.call(rbind, rows)
}

#' @export
print.audit_placebo <- function(x, ...) {
  cat(sprintf("<audit_placebo | %s | %d cell(s) | %d unit(s)>\n",
              x$type, nrow(x$cells), x$n_units))
  if (identical(x$type, "label_permutation")) {
    cat(sprintf("  %d permutation(s) per cell; p-values centered on the permutation median.\n",
                x$reps))
    deg <- sum(x$cells$degenerate %in% TRUE)
    if (deg) {
      cat(sprintf("  %d degenerate cell(s): every permuted estimate equals the observed one. The estimator uses only the label marginal, which permutation cannot test.\n",
                  deg))
    }
    failed <- sum(x$cells$n_perm_failed, na.rm = TRUE)
    if (failed) {
      cat(sprintf("  %d permutation estimate(s) errored to NA and were excluded.\n",
                  failed))
    }
  } else {
    cat("  Irrelevant-text rerun; the comparison is descriptive, with no p-values.\n")
    cat("  A placebo estimate that tracks the real one means the pipeline manufactures the number from the instrument, not the construct.\n")
  }
  print(x$cells, ...)
  invisible(x)
}

#' Coerce an audit placebo to a tibble
#'
#' @param x An [audit_placebo()] result.
#' @param ... Passed to [tibble::as_tibble()].
#' @return The placebo `cells` tibble.
#' @exportS3Method tibble::as_tibble audit_placebo
as_tibble.audit_placebo <- function(x, ...) {
  tibble::as_tibble(x$cells, ...)
}

#' Draft the robustness appendix
#'
#' @param audit An [audit_run()] result.
#' @param ... Unused; accepted for generic compatibility.
#' @return Character lines of class `audit_report`, with a print
#'   method: grid dimensions, stability metrics, fragility, parse-failure
#'   accounting, and the no-badge disclaimer.
#' @export
audit_report <- function(audit, ...) {
  stopifnot(inherits(audit, "audit"))
  plan <- attr(audit, "plan")
  s <- audit_stability(audit)
  f <- audit_fragility(audit)
  lines <- c(
    sprintf("MEASUREMENT ROBUSTNESS AUDIT over %d cells: %d prompt(s) x %d model(s) x %d label order(s) x %d temperature(s); %d unit(s) measured per cell.",
            nrow(audit), length(unique(audit$prompt)),
            length(unique(audit$model)), length(unique(audit$label_order)),
            length(unique(audit$temperature)), nrow(plan$data)),
    sprintf("ESTIMATES. Reference %.4f; range [%.4f, %.4f]; median %.4f; IQR %.4f.",
            s$reference_estimate, s$min, s$max, s$median, s$iqr),
    sprintf("SIGN. %.0f%% of cells agree with the reference sign.",
            100 * s$sign_agreement),
    sprintf("FRAGILITY. %s",
            if (is.infinite(f)) "No cell in this grid flips the sign (a statement about this grid, not a guarantee)."
            else sprintf("Changing %d measurement choice(s) suffices to flip the sign.", f)),
    sprintf("PARSING. %d cell(s) had parse failures; %d cell(s) failed to estimate.",
            sum(audit$parse_failures > 0), s$n_failed_cells),
    "NOTE. A \"passed audit\" badge is deliberately absent: the deliverable is the distribution of estimates, not a blessing.",
    "NOTE. Perturbation robustness is not construct validity; pair with the gold-set validation in the coding workflow."
  )
  structure(lines, class = "audit_report")
}

#' Report an audit through LLMR's shared generic
#'
#' A "passed audit" badge is deliberately absent: the deliverable is the
#' distribution of estimates, not a blessing.
#'
#' @param x An [audit_run()] result.
#' @param ... Passed to [audit_report()].
#' @return An `audit_report`.
#' @exportS3Method LLMR::report audit
report.audit <- function(x, ...) {
  audit_report(x, ...)
}

#' @export
print.audit_report <- function(x, ...) {
  cat(paste(unclass(x), collapse = "\n"), "\n")
  invisible(x)
}
