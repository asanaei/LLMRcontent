# 00_package.R ----------------------------------------------------------------
# Shared package documentation, imports, and the few internal helpers used by
# more than one module (coding, robustness audits, archiving). The module files
# are prefixed coder_, valid_, and archive_.

#' LLMRcontent: LLM-assisted content analysis for the social sciences
#'
#' LLMRcontent is used to conduct content analysis with labels returned by
#' large language models. It uses 'LLMR' for model configuration, execution,
#' and audit logs.
#'
#' [codebook()] defines the categories and coding rules. [protocol()] combines
#' the codebook with a prompt, model configuration, and parser, and
#' [protocol_lock()] records a content hash for that specification. [gold_set()]
#' stores human-labeled units and their development and holdout assignments.
#' [validate_protocol()] compares model labels with holdout labels.
#' [code_corpus()] applies the protocol to a corpus, and [gold_correct()]
#' estimates category prevalences and their standard errors from the coded
#' corpus and holdout data.
#'
#' [audit_plan()] defines an estimator and a grid of coding specifications.
#' [audit_run()] codes the data and recomputes the estimator for each grid cell.
#' [audit_stability()] summarizes the resulting estimates, and
#' [audit_fragility()] counts the specification changes needed to cross a
#' stated conclusion rule.
#'
#' [archive_build()] reads an 'LLMR' JSONL audit log into records and a manifest.
#' [archive_seal()] computes a root hash, and [archive_check()] verifies the
#' stored record hashes. [archive_replay()] returns stored responses for offline
#' recomputation. [archive_redact()] removes prompts and response text while
#' retaining the archive's hash records.
#'
#' The optional Shiny interface runs these functions interactively. Use
#' [install_gui_deps()] to install its suggested packages and
#' [run_content_studio()] to start it.
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
