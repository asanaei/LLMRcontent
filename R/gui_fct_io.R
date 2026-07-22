# fct_io.R ---------------------------------------------------------------------
# LLMRcontent coding-workflow glue: map user columns into gold_set()/code_corpus() calls
# and bundle the coded corpus plus methods text into a downloadable zip. The
# generic IO helpers (read_csv_*, map_columns, as_display_table) come from
# LLMR.shiny.

call_gold_set_mapped <- function(data, text_col, label_col, split, stratify,
                                 seal_holdout = TRUE) {
  # LLMR.shiny::map_columns names the working columns "text" and "labels";
  # gold_set takes those as column-name strings.
  mapped <- LLMR.shiny::map_columns(data, text_col, label_col, keep_original = TRUE)
  LLMRcontent::gold_set(
    data = mapped,
    text = "text",
    label = "labels",
    split = split,
    stratify = stratify,
    seal_holdout = seal_holdout
  )
}

call_code_corpus_mapped <- function(corpus, text_col, protocol, .runner = NULL) {
  # Replicate count is carried by the locked protocol (protocol$replicates),
  # not a code_corpus() argument.
  mapped <- LLMR.shiny::map_columns(corpus, text_col, keep_original = TRUE)
  LLMRcontent::code_corpus(
    corpus = mapped,
    protocol = protocol,
    text = "text",
    .runner = .runner
  )
}

bundle_coder_artifacts <- function(coded, validation, gold, protocol, file, demo = FALSE) {
  if (!pkg_available("LLMRcontent")) {
    stop("LLMRcontent is required to export artifacts.", call. = FALSE)
  }

  out_dir <- tempfile("llmrstudio-artifacts-")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  coded_path <- file.path(out_dir, "coded.csv")
  methods_path <- file.path(out_dir, "methods.txt")
  summary_path <- file.path(out_dir, "summary.txt")
  notice <- LLMR.shiny::demo_notice()

  utils::write.csv(tibble::as_tibble(coded), coded_path, row.names = FALSE)

  if (isTRUE(demo) && file.exists(coded_path)) {
    exported <- tryCatch(LLMR.shiny::read_csv_path(coded_path), error = function(e) NULL)
    if (is.data.frame(exported)) {
      exported$demo_notice <- notice
      utils::write.csv(exported, coded_path, row.names = FALSE)
    }
  }

  report_obj <- LLMR::report(validation, gold = gold, protocol = protocol)
  report_text <- paste(utils::capture.output(print(report_obj)), collapse = "\n")
  if (isTRUE(demo)) report_text <- paste(notice, report_text, sep = "\n\n")
  writeLines(report_text, methods_path)

  summary_text <- c(
    if (isTRUE(demo)) notice else character(),
    "Artifacts included:",
    "coded.csv: coded corpus as a flat CSV.",
    "methods.txt: methods report from LLMR::report()."
  )
  writeLines(summary_text, summary_path)

  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(out_dir)
  utils::zip(zipfile = file, files = c("coded.csv", "methods.txt", "summary.txt"))
  invisible(file)
}

# The coder demo responder: a keyword heuristic over codebook labels, used by the
# offline demo runner so the workflow runs without a key.
coder_demo_responder <- function(codebook = NULL) {
  labels <- demo_labels_from_codebook(codebook)
  function(text) {
    text <- tolower(text %||% "")
    labs <- labels[nzchar(labels)]
    if (length(labs) == 0) labs <- c("policy", "community", "other")
    if (grepl("policy|government|law|vote|rights|regulation|public", text)) return(labs[[1]])
    if (length(labs) >= 2 && grepl("family|friend|community|neighbor|group|social", text)) return(labs[[2]])
    labs[[length(labs)]]
  }
}

demo_labels_from_codebook <- function(codebook) {
  if (is.null(codebook)) return(c("policy", "community", "other"))
  # The codebook's own label accessor is exact; only fall back to defaults if it
  # is unavailable or empty.
  labs <- tryCatch(
    if (pkg_available("LLMRcontent")) {
      as.character(LLMRcontent::codebook_labels(codebook))
    } else NULL,
    error = function(e) NULL
  )
  labs <- labs[nzchar(labs %||% "")]
  if (length(labs) >= 2) labs else c("policy", "community", "other")
}
