# archive_horizon.R ---------------------------------------------------------------------
# The verifiability horizon: not all calls age equally. Open-weight
# checkpoints can be pinned and re-run in ten years; closed API models drift
# and die. The horizon makes that difference visible per archive -- which is,
# quietly, the strongest argument for open weights in published research.

#' The verifiability horizon of an archive
#'
#' Classifies every model in the archive by how long its calls remain
#' re-runnable:
#'
#' - `"open-pinnable"`: open-weight families (re-runnable indefinitely
#'   against a pinned checkpoint; record the checkpoint hash in the paper).
#' - `"api-contingent"`: closed models behind a live API (re-runnable only
#'   while the provider serves this version; the archived `model_version`
#'   tells you which one to ask for).
#'
#' Classification is a heuristic over provider and model names; override
#' with `open_patterns` when you serve something unusual.
#'
#' @param archive An `archive`.
#' @param open_patterns Regular expression matched (case-insensitively)
#'   against model names to classify them as open-weight.
#' @return A tibble: `model`, `provider`, `calls`, `class`.
#' @examples
#' log <- tempfile(fileext = ".jsonl")
#' writeLines(c(
#'   paste0('{"ts":"2026-06-01T10:00:01+0000","schema_version":"1.0",',
#'     '"kind":"call","provider":"groq","model":"openai/gpt-oss-20b",',
#'     '"usage":{"sent":5,"rec":2},"response_id":"r-1"}'),
#'   paste0('{"ts":"2026-06-01T10:00:02+0000","schema_version":"1.0",',
#'     '"kind":"call","provider":"openai","model":"gpt-4o",',
#'     '"usage":{"sent":5,"rec":2},"response_id":"r-2"}')), log)
#' verifiability_horizon(archive_build(log))
#' @export
verifiability_horizon <- function(
    archive,
    open_patterns = "gpt-oss|llama|qwen|deepseek|mistral|mixtral|gemma|phi-|kimi|glm-|yi-") {
  stopifnot(inherits(archive, "archive"))
  m <- archive$manifest
  m <- m[!is.na(m$model) & !is.na(m$provider), , drop = FALSE]
  if (!nrow(m)) {
    return(tibble::tibble(model = character(0), provider = character(0),
                          calls = integer(0), class = character(0)))
  }
  agg <- stats::aggregate(idx ~ model + provider, data = m, FUN = length)
  names(agg)[names(agg) == "idx"] <- "calls"
  agg$class <- ifelse(
    agg$provider %in% "ollama" |
      grepl(open_patterns, agg$model, ignore.case = TRUE),
    "open-pinnable", "api-contingent")
  tibble::as_tibble(agg[order(-agg$calls), c("model", "provider", "calls", "class")])
}

#' Draft the reproducibility appendix
#'
#' Generated from the archive itself: call counts by provider, model, and
#' served `model_version`; date range; token totals; failure counts; seal
#' root; and the verifiability horizon. Companion to
#' `LLMR::llm_methods_text()`, which drafts the in-text methods paragraph
#' from a results frame; this drafts the appendix from the log.
#'
#' @param archive An `archive` (seal it first; the root is cited).
#' @param ... Passed to [verifiability_horizon()], such as `open_patterns`.
#' @return Character lines of class `archive_appendix`, with a print method.
#' @export
archive_appendix <- function(archive, ...) {
  stopifnot(inherits(archive, "archive"))
  m <- archive$manifest
  usage_tot <- Reduce(`+`, lapply(archive$records, function(r) {
    u <- r$rec$usage
    c(sent = as.numeric(u$sent %||% 0), rec = as.numeric(u$rec %||% 0))
  }), accumulate = FALSE) %||% c(sent = 0, rec = 0)
  fails <- sum(m$kind %in% "error" | (!is.na(m$status) & m$status >= 400))
  by_model <- verifiability_horizon(archive, ...)
  versions <- unique(stats::na.omit(m$model_version))
  lines <- c(
    sprintf("REPLICATION ARCHIVE '%s'%s.", archive$name,
            if (archive$redacted) " (content redacted; hash tree intact)" else ""),
    sprintf("SEAL. %s", if (archive$sealed)
      sprintf("Root %s over %d record(s), sealed %s.",
              archive$seal$root, archive$seal$n_records, archive$seal$sealed_at)
      else "UNSEALED -- seal before depositing."),
    if (all(is.na(m$ts)))
      sprintf("CALLS. %d total (timestamps unavailable); %d failure(s).",
              nrow(m), fails)
    else
      sprintf("CALLS. %d total between %s and %s; %d failure(s).",
              nrow(m), min(m$ts, na.rm = TRUE), max(m$ts, na.rm = TRUE), fails),
    sprintf("TOKENS. %s sent, %s received (as reported by providers).",
            format(usage_tot[["sent"]], big.mark = ","),
            format(usage_tot[["rec"]], big.mark = ",")),
    if (length(versions))
      sprintf("SERVED VERSIONS. %s.", paste(versions, collapse = ", ")),
    "VERIFIABILITY HORIZON.",
    sprintf("  %-34s %-10s %5d call(s)  %s",
            by_model$model, by_model$provider, by_model$calls, by_model$class),
    sprintf("ENVIRONMENT. R %s; LLMR %s; log schema %s; built %s.",
            archive$env$r_version, archive$env$llmr_version,
            paste(unique(stats::na.omit(m$schema_version)), collapse = "/"),
            archive$env$built)
  )
  structure(lines, class = "archive_appendix")
}

#' @export
print.archive_appendix <- function(x, ...) {
  cat(paste(unclass(x), collapse = "\n"), "\n")
  invisible(x)
}

#' Compare two archives
#'
#' Set comparison over canonical request hashes: which calls are unique to
#' each archive, which questions were asked in both. The everyday use is
#' revision hygiene -- did the R1 resubmission re-run what it claims to have
#' re-run?
#'
#' @param a,b `archive` objects.
#' @return A list of class `archive_diff`: `only_a`, `only_b`, `common`
#'   (counts), plus the hash sets as attributes.
#' @export
archive_diff <- function(a, b) {
  stopifnot(inherits(a, "archive"), inherits(b, "archive"))
  ha <- stats::na.omit(a$manifest$request_hash)
  hb <- stats::na.omit(b$manifest$request_hash)
  out <- list(only_a = length(setdiff(ha, hb)),
              only_b = length(setdiff(hb, ha)),
              common = length(intersect(ha, hb)))
  attr(out, "only_a_hashes") <- setdiff(ha, hb)
  attr(out, "only_b_hashes") <- setdiff(hb, ha)
  class(out) <- "archive_diff"
  out
}

#' @export
print.archive_diff <- function(x, ...) {
  cat(sprintf("<archive_diff | common %d | only in A %d | only in B %d>\n",
              x$common, x$only_a, x$only_b))
  invisible(x)
}
