# valid_plan.R ------------------------------------------------------------------------
# Declaring the audit: data, estimand, labels, baseline prompt, and the grid
# of measurement choices the conclusion will be tested against.

#' Declare a measurement-robustness audit
#'
#' @param data The data frame whose texts are measured.
#' @param text Name of the text column.
#' @param estimator The estimand as code: a function that receives `data`
#'   with one added character column `label` (the LLM measurement under one
#'   grid cell) and returns a single numeric value -- a share, a regression
#'   coefficient, a difference in means. Whatever number the paper reports,
#'   this function computes it. Two contract points: `label` **may contain
#'   `NA`** (parse failures), and your estimator must decide what they mean
#'   -- exclude, impute, or treat as a category -- explicitly; and if the
#'   estimator errors in a cell, [audit_run()] records `NA` for that cell
#'   and counts it, rather than aborting the grid.
#' @param labels Valid labels, in their baseline order. The prompt may
#'   reference them via the `{labels}` placeholder, which is what the
#'   label-order perturbation reorders.
#' @param prompt Baseline prompt template; must contain `{text}`, may
#'   contain `{labels}`. Placeholders are substituted literally, so braces
#'   in the measured text are safe.
#' @return An `audit_plan` with a single prompt variant (`"base"`), no
#'   models, label order `"as_given"`, and temperature `0`. Add the grid
#'   with [audit_add_models()], [audit_add_prompts()], [audit_add_perturbations()].
#' @examples
#' plan <- audit_plan(
#'   data = data.frame(text = c("cut taxes now", "fund the schools")),
#'   text = "text",
#'   estimator = function(d) mean(d$label == "conservative"),
#'   labels = c("conservative", "progressive"),
#'   prompt = "Classify the slogan as one of: {labels}.\n\n{text}\n\nLabel:"
#' )
#' plan
#' @export
audit_plan <- function(data, text, estimator, labels, prompt) {
  stopifnot(is.data.frame(data), nrow(data) >= 1L,
            is.character(text), text %in% names(data),
            is.function(estimator),
            is.character(labels), length(labels) >= 2L,
            is.character(prompt), length(prompt) == 1L)
  if (!grepl("{text}", prompt, fixed = TRUE)) {
    abort("`prompt` must contain the {text} placeholder.")
  }
  structure(
    list(data = tibble::as_tibble(data), text = text,
         estimator = estimator, labels = labels,
         prompts = c(base = prompt),
         models = list(),
         label_orders = "as_given",
         temperatures = 0),
    class = "audit_plan"
  )
}

#' Add the model arm of the grid
#'
#' @param plan An [audit_plan()].
#' @param config A named list of `LLMR::llm_config()` objects. Spanning
#'   model *families* (not just sizes of one family) is what makes the arm
#'   informative; agreement within one family is family resemblance, not
#'   robustness.
#' @return The plan, extended.
#' @export
audit_add_models <- function(plan, config) {
  stopifnot(inherits(plan, "audit_plan"), is.list(config),
            length(config) >= 1L)
  if (is.null(names(config)) || any(!nzchar(names(config)))) {
    abort("`config` must be a *named* list (names appear in the report).")
  }
  for (cf in config) {
    if (!inherits(cf, "llm_config")) {
      abort("Every element of `config` must be an LLMR::llm_config().")
    }
  }
  plan$models <- c(plan$models, config)
  plan
}

#' Add prompt paraphrases
#'
#' @param plan An [audit_plan()].
#' @param ... Named prompt templates (same placeholder rules as the
#'   baseline). Honest paraphrases ask the same question in different
#'   words; do not "improve" the prompt here -- that is tuning, and it
#'   belongs in the coding tournament, before the audit.
#' @return The plan, extended.
#' @export
audit_add_prompts <- function(plan, ...) {
  stopifnot(inherits(plan, "audit_plan"))
  variants <- c(...)
  if (is.null(names(variants)) || any(!nzchar(names(variants)))) {
    abort("Prompt variants must be named.")
  }
  for (v in variants) {
    if (!grepl("{text}", v, fixed = TRUE)) {
      abort("Every prompt variant must contain the {text} placeholder.")
    }
  }
  plan$prompts <- c(plan$prompts, variants)
  plan
}

#' Add measurement perturbations
#'
#' @param plan An [audit_plan()].
#' @param label_order `"as_given"`, `"reversed"`, or both. With LLMs the
#'   order in which options are listed is a real effect, not a nuisance;
#'   auditing it is the point.
#' @param temperature Numeric vector of temperatures to cross (e.g.
#'   `c(0, 0.7)`).
#' @return The plan, extended.
#' @export
audit_add_perturbations <- function(plan, label_order = NULL, temperature = NULL) {
  stopifnot(inherits(plan, "audit_plan"))
  if (!is.null(label_order)) {
    bad <- setdiff(label_order, c("as_given", "reversed"))
    if (length(bad)) abort("`label_order` must be 'as_given' and/or 'reversed'.")
    plan$label_orders <- unique(c(plan$label_orders, label_order))
  }
  if (!is.null(temperature)) {
    stopifnot(is.numeric(temperature))
    plan$temperatures <- sort(unique(c(plan$temperatures, temperature)))
  }
  plan
}

#' @export
print.audit_plan <- function(x, ...) {
  n_cells <- length(x$prompts) * max(1L, length(x$models)) *
    length(x$label_orders) * length(x$temperatures)
  cat(sprintf("<audit_plan | %d unit(s) | grid: %d prompt(s) x %d model(s) x %d order(s) x %d temperature(s) = %d cell(s)>\n",
              nrow(x$data), length(x$prompts), length(x$models),
              length(x$label_orders), length(x$temperatures), n_cells))
  if (!length(x$models)) cat("  (no models yet: audit_add_models())\n")
  invisible(x)
}

# Internal: render one prompt for one text under one label order.
.render_audit_prompt <- function(template, text, labels, order) {
  labs <- if (identical(order, "reversed")) rev(labels) else labels
  out <- gsub("{labels}", paste(labs, collapse = ", "), template, fixed = TRUE)
  gsub("{text}", text, out, fixed = TRUE)
}
