# coder_codebook.R --------------------------------------------------------------------
# The codebook is the measurement instrument: a versioned, hashable artifact
# that compiles into the prompt, so what the model was told is never a mystery.

#' Define one category of a codebook
#'
#' @param label The label the coder (human or model) assigns. Keep it short
#'   and machine-friendly; it is matched verbatim (then case-insensitively)
#'   when parsing model output.
#' @param definition What the category means, in one or two sentences.
#' @param include Optional character vector of terms or rules for when this
#'   category applies.
#' @param exclude Optional character vector of terms or rules for when this
#'   category does not apply.
#' @param examples Optional verbatim examples that belong to the category.
#' @param counterexamples Optional near-misses that do not.
#' @return A `cb_category` object.
#' @examples
#' cb_category(
#'   "populist",
#'   "Frames politics as a struggle between the virtuous people and a corrupt elite.",
#'   include = "Attacks on 'the elite' or 'the establishment' as a class.",
#'   exclude = "Criticism of a named politician without people-vs-elite framing.",
#'   examples = "Brussels bureaucrats have never done an honest day's work."
#' )
#' @seealso [codebook()]
#' @export
cb_category <- function(label, definition, include = NULL, exclude = NULL,
                        examples = NULL, counterexamples = NULL) {
  stopifnot(is.character(label), length(label) == 1L, nzchar(label),
            is.character(definition), length(definition) == 1L)
  structure(
    list(label = label, definition = definition,
         include = include, exclude = exclude,
         examples = examples, counterexamples = counterexamples),
    class = "cb_category"
  )
}

#' @export
print.cb_category <- function(x, ...) {
  cat(sprintf("<cb_category '%s'>\n  %s\n", x$label, x$definition))
  invisible(x)
}

#' Create a codebook
#'
#' A codebook is the measurement instrument of a content analysis: construct
#' definitions, category boundaries, decision rules, and examples. Here it is
#' a first-class object -- versioned, printable, hashable -- that compiles
#' into the prompt via [format_codebook()], so the instrument the model saw
#' is exactly the instrument in the paper's appendix.
#'
#' @param name Short name of the construct (e.g. `"populist framing"`).
#' @param unit What one coding unit is (e.g. `"one parliamentary speech"`).
#' @param categories A list of [cb_category()] objects. Labels must be unique.
#' @param instructions Optional free-text instructions placed before the
#'   category definitions (coder-neutral: the same text serves human coders).
#' @param version A version string you control (e.g. `"1.2"`); bump it when
#'   the instrument changes. The content hash changes with any edit either
#'   way; the version is for humans.
#' @return A `codebook` object.
#' @examples
#' cb <- codebook(
#'   name = "populist framing",
#'   unit = "one speech",
#'   categories = list(
#'     cb_category("populist",     "People-vs-elite framing is present."),
#'     cb_category("not_populist", "No people-vs-elite framing.")
#'   ),
#'   version = "1.0"
#' )
#' cb
#' codebook_hash(cb)
#' @seealso [cb_category()], [format_codebook()], [protocol()]
#' @export
codebook <- function(name, unit, categories, instructions = NULL,
                     version = "1.0") {
  stopifnot(is.character(name), length(name) == 1L,
            is.character(unit), length(unit) == 1L,
            is.list(categories), length(categories) >= 2L)
  for (ct in categories) {
    if (!inherits(ct, "cb_category")) {
      abort("Every element of `categories` must be created with cb_category().")
    }
  }
  labs <- vapply(categories, `[[`, "", "label")
  if (anyDuplicated(labs)) abort("Category labels must be unique.")
  structure(
    list(name = name, unit = unit, categories = categories,
         instructions = instructions, version = as.character(version)),
    class = "codebook"
  )
}

#' @export
print.codebook <- function(x, ...) {
  cat(sprintf("<codebook '%s' v%s | unit: %s | %d categories | %s>\n",
              x$name, x$version, x$unit, length(x$categories),
              substr(codebook_hash(x), 1, 12)))
  for (ct in x$categories) {
    cat(sprintf("  - %s: %s\n", ct$label, ct$definition))
  }
  invisible(x)
}

#' Labels defined by a codebook
#' @param x A [codebook()].
#' @return Character vector of category labels.
#' @export
codebook_labels <- function(x) {
  stopifnot(inherits(x, "codebook"))
  vapply(x$categories, `[[`, "", "label")
}

#' Content hash of a codebook
#'
#' A SHA-256 over the canonical serialization. Any edit to any definition,
#' rule, or example changes the hash; the hash is recorded in locked
#' protocols and reports, which is what makes "we used codebook v1.2"
#' verifiable rather than aspirational.
#'
#' @param x A [codebook()].
#' @return A character scalar (64 hex digits).
#' @export
codebook_hash <- function(x) {
  stopifnot(inherits(x, "codebook"))
  .hash(unclass(x))
}

#' Render a codebook as prompt text
#'
#' Compiles the codebook into the plain-text block that is interpolated into
#' the protocol's prompt template (at `{codebook}`). The same rendering can
#' be handed to human coders, which is the point: one instrument, two coder
#' populations.
#'
#' @param x A [codebook()].
#' @return A character scalar.
#' @examples
#' cb <- codebook("tone", "one sentence",
#'   list(cb_category("positive", "Approving or hopeful."),
#'        cb_category("negative", "Critical or alarmed.")))
#' cat(format_codebook(cb))
#' @export
format_codebook <- function(x) {
  stopifnot(inherits(x, "codebook"))
  block <- function(ct) {
    parts <- c(
      sprintf("LABEL: %s", ct$label),
      sprintf("  Definition: %s", ct$definition),
      if (length(ct$include))
        sprintf("  Code here when: %s", paste(ct$include, collapse = "; ")),
      if (length(ct$exclude))
        sprintf("  Do not code here when: %s", paste(ct$exclude, collapse = "; ")),
      if (length(ct$examples))
        sprintf("  Example(s): %s", paste(ct$examples, collapse = " | ")),
      if (length(ct$counterexamples))
        sprintf("  Near-miss(es): %s", paste(ct$counterexamples, collapse = " | "))
    )
    paste(parts, collapse = "\n")
  }
  paste(c(
    sprintf("CODING TASK: %s (unit: %s; codebook v%s)",
            x$name, x$unit, x$version),
    if (!is.null(x$instructions)) x$instructions,
    "Assign exactly one of the following labels.",
    vapply(x$categories, block, character(1))
  ), collapse = "\n\n")
}
