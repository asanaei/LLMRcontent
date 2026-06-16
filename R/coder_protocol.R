# coder_protocol.R --------------------------------------------------------------------
# A coding protocol is everything that determines a label: codebook + prompt
# template + model configuration + parser. Locking computes a content hash
# over all of it; only locked protocols may evaluate on the sealed test split
# or code a corpus, so the thing that was validated is provably the thing
# that was used.

#' Default parser: match the reply to a codebook label
#'
#' Trims the model's reply and matches it against the codebook's labels,
#' exactly first, then case-insensitively; anything else becomes `NA` (a
#' parse failure you will see in the validation tables, not a silently
#' invented category).
#'
#' @return A function `(text, labels) -> label or NA` suitable for
#'   [protocol()]'s `parser` argument.
#' @export
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
#'   into one of the codebook's labels (or `NA`). Default [parse_label()].
#' @param replicates How many times each unit is coded by [code_corpus()]
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
protocol <- function(codebook, config, prompt = NULL, parser = parse_label(),
                     replicates = 1L, label = NULL) {
  stopifnot(inherits(codebook, "codebook"), is.function(parser))
  if (!inherits(config, "llm_config")) {
    abort("`config` must be an LLMR::llm_config() object.")
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
         parser = parser, replicates = max(1L, as.integer(replicates)),
         label = label %||% paste0(config$provider, "/", config$model),
         locked = FALSE, hash = NULL),
    class = "coding_protocol"
  )
}

#' Lock a protocol
#'
#' Computes the protocol's content hash -- over the codebook, the prompt
#' template, the provider, model, all model parameters, the replicate count,
#' **and the parser** (by its deparsed source; the parser decides the final
#' label, so it is part of the instrument) -- and freezes it.
#' [validate_protocol()] on the sealed split and [code_corpus()] both require
#' a locked protocol, so the validated instrument and the deployed instrument
#' are the same object, verifiably. Hashes use `LLMR::llm_hash()`, the
#' ecosystem-wide convention.
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
  x$hash <- .hash(list(
    codebook = codebook_hash(x$codebook),
    prompt = x$prompt,
    provider = x$config$provider,
    model = x$config$model,
    params = x$config$model_params,
    replicates = x$replicates,
    parser = x$parser
  ))
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
  out <- sub("{codebook}", format_codebook(protocol$codebook), out, fixed = TRUE)
  out <- sub("{text}", text, out, fixed = TRUE)
  out
}
