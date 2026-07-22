# coder_protocol.R --------------------------------------------------------------------
# A coding protocol is everything that determines a label: codebook + prompt
# template + model configuration + parser. Locking computes a content hash
# over all of it; only locked protocols may evaluate on the sealed holdout
# split or code a corpus, so the thing that was validated is provably the
# thing that was used.

# Internal default parser: normalize a reply against the codebook labels.
parse_label <- function() {
  function(text, labels) .normalize_label(text, labels)
}

#' Assemble a coding protocol
#'
#' @param codebook A [codebook()].
#' @param config An `LLMR::llm_config()` for a generative model. For
#'   annotation work prefer `temperature = 0`.
#' @param prompt Prompt template containing the placeholder `{text}` and,
#'   optionally, `{codebook}` (replaced by [format_codebook()]'s rendering).
#'   `NULL` uses a sensible default. Placeholders are substituted literally,
#'   so braces in the coded text itself are safe.
#' @param parser A function `(text, labels) -> label` turning a model reply
#'   into one of the codebook's labels (or `NA`). `NULL` uses the package's
#'   label-matching parser.
#' @param replicates How many times each unit is coded before its modal label is
#'   used by [tune_protocol()], [validate_protocol()], and [code_corpus()]
#'   (replicates make model self-disagreement measurable; see
#'   [coder_agreement()]).
#' @param label Optional human-readable tag used in tournaments and reports
#'   (e.g. `"v2-fewshot-qwen"`); defaults to provider/model.
#' @return A `coding_protocol` (unlocked).
#' @examples
#' cb  <- codebook("tone", "one sentence",
#'   list(cb_category("positive", "Approving."),
#'        cb_category("negative", "Critical.")))
#' cfg <- LLMR::llm_config("groq", "openai/gpt-oss-20b", temperature = 0)
#' p   <- protocol(cb, cfg)
#' protocol_lock(p)
#' @seealso [protocol_lock()], [tune_protocol()], [validate_protocol()]
#' @export
protocol <- function(codebook, config, prompt = NULL, parser = NULL,
                     replicates = 1L, label = NULL) {
  if (is.null(parser)) parser <- parse_label()
  stopifnot(inherits(codebook, "codebook"), is.function(parser))
  if (!inherits(config, "llm_config")) {
    abort("`config` must be an LLMR::llm_config() object.")
  }
  if (!is.numeric(replicates) || length(replicates) != 1L ||
      !is.finite(replicates) || replicates <= 0 ||
      replicates != floor(replicates)) {
    abort("`replicates` must be a finite, positive whole-number scalar.")
  }
  prompt <- prompt %||% paste(
    "{codebook}",
    "",
    "TEXT TO CODE:",
    "{text}",
    "",
    "Reply with exactly one label, nothing else.",
    sep = "\n")
  if (!grepl("{text}", prompt, fixed = TRUE)) {
    abort("`prompt` must contain the {text} placeholder.")
  }
  structure(
    list(codebook = codebook, config = config, prompt = prompt,
         parser = parser, replicates = as.integer(replicates),
         label = label %||% paste0(config$provider, "/", config$model),
         locked = FALSE, hash = NULL),
    class = "coding_protocol"
  )
}

.parser_hash_spec <- function(parser) {
  variables <- codetools::findGlobals(parser, merge = FALSE)$variables
  parser_env <- environment(parser)
  captured <- stats::setNames(lapply(variables, function(variable) {
    if (exists(variable, envir = parser_env, inherits = TRUE)) {
      get(variable, envir = parser_env, inherits = TRUE)
    } else {
      NULL
    }
  }), variables)
  list(source = paste(deparse(parser), collapse = "\n"), captured = captured)
}

.protocol_hash <- function(x) {
  .hash(list(
    codebook = codebook_hash(x$codebook),
    prompt = x$prompt,
    provider = x$config$provider,
    model = x$config$model,
    params = x$config$model_params,
    no_change = x$config$no_change,
    embedding = x$config$embedding,
    replicates = x$replicates,
    parser = .parser_hash_spec(x$parser)
  ))
}

#' Lock a protocol
#'
#' Computes and stores a content hash covering the codebook, prompt template,
#' provider, model, generation parameters, `no_change`, embedding flag,
#' replicate count, and parser source and captured values. [validate_protocol()]
#' on the sealed split and [code_corpus()] require a locked protocol and compare
#' its current content with the stored hash. Hashes use `LLMR::llm_hash()`.
#'
#' @param x A [protocol()].
#' @return The protocol, locked, with `$hash` set.
#' @examples
#' cb <- codebook("tone", "one sentence",
#'   list(cb_category("positive", "Approving."),
#'        cb_category("negative", "Critical.")))
#' p  <- protocol(cb, LLMR::llm_config("groq", "openai/gpt-oss-20b"))
#' protocol_lock(p)
#' @export
protocol_lock <- function(x) {
  stopifnot(inherits(x, "coding_protocol"))
  x$hash <- .protocol_hash(x)
  x$locked <- TRUE
  x
}

#' @export
print.coding_protocol <- function(x, ...) {
  cat(sprintf("<coding_protocol '%s' | %s/%s | codebook '%s' v%s | %s>\n",
              x$label, x$config$provider, x$config$model,
              x$codebook$name, x$codebook$version,
              if (x$locked) paste0("LOCKED ", substr(x$hash, 1, 12)) else "unlocked"))
  invisible(x)
}

# Internal: render the prompt for one text (literal substitution; no glue,
# so braces inside the coded text cannot break anything).
.render_prompt <- function(protocol, text) {
  out <- protocol$prompt
  out <- gsub("{codebook}", format_codebook(protocol$codebook), out, fixed = TRUE)
  out <- gsub("{text}", text, out, fixed = TRUE)
  out
}
