# 00_package.R ----------------------------------------------------------------
# Shared package documentation, imports, and the few internal helpers used by
# more than one module (coding, robustness audits, archiving). The module files
# are prefixed coder_, valid_, and archive_.

#' LLMRcontent: LLM-assisted content analysis for the social sciences
#'
#' A validated workflow for content analysis with large language models, built
#' on 'LLMR'. Three connected concerns, one package.
#'
#' \strong{Coding} turns a codebook and a sealed gold standard into
#' error-corrected category prevalences: [gold_set()] -> [protocol_lock()] ->
#' [validate_protocol()] -> [gold_correct()]. A sealed gold split audits the
#' instrument, a content hash identifies the locked codebook protocol, and
#' [gold_correct()] carries remaining label error into corrected corpus-level
#' prevalences with standard errors. For accessible qualitative coding use
#' 'quallmer'; use this package's coding tools when the label becomes a variable
#' in quantitative analysis.
#'
#' \strong{Robustness audits} ask whether a coded conclusion survives the
#' measurement multiverse: [audit_plan()] -> [audit_run()] ->
#' [audit_stability()] / [audit_fragility()]. The deliverable is the distribution
#' of estimates and the smallest measurement change that flips the conclusion,
#' not a blessing. Perturbation robustness is not construct validity; pair it
#' with the gold-set validation above.
#'
#' \strong{Archives} turn the audit log that LLMR writes
#' (`LLMR::llm_log_enable()`) into a reviewer-runnable replication record:
#' [archive_build()] -> [archive_seal()] -> [archive_check()], with
#' [archive_redact()] for IRB-restricted text and [archive_replay()] for offline
#' recomputation.
#'
#' The shared generics [LLMR::diagnostics()], [LLMR::report()], and
#' [tibble::as_tibble()] dispatch across the result objects of all three.
#'
#' An optional Shiny GUI drives the same workflow interactively. Install its
#' extra dependencies with [install_gui_deps()], then launch it with
#' [run_content_studio()].
#'
#' @keywords internal
#' @importFrom rlang %||% abort
#' @importFrom tibble as_tibble
"_PACKAGE"

utils::globalVariables(c("protocol_id", "label", "estimate"))

# ---- shared internal helpers -------------------------------------------------

# Object hashing is LLMR infrastructure (LLMR::llm_hash): one convention for the
# whole ecosystem, pinned by the tests so the packages cannot drift apart. The
# coding module historically called this `.hash`; the archive module called it
# `.hash_obj`. Keep one definition and alias the other so both call sites work.
.hash_obj <- function(x) LLMR::llm_hash(x)
.hash <- .hash_obj

# Raw lines hash over their bytes directly (tamper evidence for archived log
# lines), distinct from the object hash above.
.hash_chr <- function(x) digest::digest(x, algo = "sha256", serialize = FALSE)

# Normalize a model reply to one of a set of labels: exact match first, then
# case-insensitive with internal whitespace collapsed on both the reply and the
# labels; NA when nothing matches. This is the coding module's version (a strict
# superset of the audit module's, which omitted the whitespace collapse), shared
# by both.
.normalize_label <- function(x, labels) {
  x <- gsub("\\s+", " ", trimws(as.character(x)))
  if (x %in% labels) return(x)
  hit <- match(tolower(x), tolower(gsub("\\s+", " ", trimws(labels))))
  if (!is.na(hit)) return(labels[hit])
  NA_character_
}
