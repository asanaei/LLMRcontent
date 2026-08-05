# fct_io.R ---------------------------------------------------------------------
# LLMRcontent coding-workflow glue: map user columns into gold_set()/code_corpus() calls
# and bundle the coded corpus plus methods text into a downloadable zip. The
# generic IO helpers (read_csv_*, map_columns, as_display_table) come from
# LLMR.shiny.

call_gold_set_mapped <- function(data, text_col, label_col, split, stratify,
                                 seal_holdout = TRUE, id_col = NULL) {
  # LLMR.shiny::map_columns names the working columns "text" and "labels";
  # gold_set takes those as column-name strings. An id column keeps its
  # original name and rides through keep_original.
  mapped <- LLMR.shiny::map_columns(data, text_col, label_col, keep_original = TRUE)
  LLMRcontent::gold_set(
    data = mapped,
    text = "text",
    label = "labels",
    split = split,
    stratify = stratify,
    seal_holdout = seal_holdout,
    id = id_col
  )
}

call_code_corpus_mapped <- function(corpus, text_col, protocol, .runner = NULL,
                                    id_col = NULL) {
  # Replicate count is carried by the locked protocol (protocol$replicates),
  # not a code_corpus() argument. An id column keeps its original name.
  mapped <- LLMR.shiny::map_columns(corpus, text_col, keep_original = TRUE)
  LLMRcontent::code_corpus(
    corpus = mapped,
    protocol = protocol,
    text = "text",
    id = id_col,
    .runner = .runner
  )
}

bundle_coder_artifacts <- function(coded, validation, gold, protocol, file,
                                   demo = FALSE, coded_demo = demo,
                                   validation_demo = demo) {
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

  if (isTRUE(coded_demo) && file.exists(coded_path)) {
    exported <- tryCatch(LLMR.shiny::read_csv_path(coded_path), error = function(e) NULL)
    if (is.data.frame(exported)) {
      exported$demo_notice <- notice
      utils::write.csv(exported, coded_path, row.names = FALSE)
    }
  }

  report_obj <- LLMR::report(validation, gold = gold, protocol = protocol)
  report_text <- paste(utils::capture.output(print(report_obj)), collapse = "\n")
  if (isTRUE(validation_demo)) {
    report_text <- paste(notice, report_text, sep = "\n\n")
  }
  writeLines(report_text, methods_path)

  summary_text <- c(
    if (isTRUE(coded_demo)) paste("coded.csv:", notice) else character(),
    if (isTRUE(validation_demo)) {
      paste("methods.txt:", notice)
    } else {
      character()
    },
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

bind_call_batches <- function(batches) {
  if (!length(batches)) return(NULL)
  batches <- Filter(is.data.frame, batches)
  if (!length(batches)) return(NULL)
  out <- do.call(rbind, batches)
  rownames(out) <- NULL
  out
}

finish_call_timing <- function(batches, started) {
  calls <- bind_call_batches(batches)
  if (is.null(calls)) return(NULL)
  duration_cols <- intersect(c("duration", "duration_s"), names(calls))
  if (!length(duration_cols)) return(NULL)
  duration_col <- duration_cols[[1]]
  duration <- suppressWarnings(as.numeric(calls[[duration_col]]))
  keep <- is.finite(duration) & duration >= 0
  if (!any(keep)) return(NULL)
  calls <- calls[keep, , drop = FALSE]
  calls$.duration_seconds <- duration[keep]
  list(
    wall_seconds = max(0, proc.time()[["elapsed"]] - started),
    calls = calls
  )
}

timing_summary_text <- function(timing) {
  if (is.null(timing)) return(NULL)
  d <- timing$calls$.duration_seconds
  sprintf(
    paste0(
      "Wall time %.2f seconds. Recorded call time %.2f seconds across %d ",
      "%s; median %.2f seconds per call."
    ),
    timing$wall_seconds,
    sum(d),
    length(d),
    if (length(d) == 1L) "call" else "calls",
    stats::median(d)
  )
}

timing_by_unit <- function(timing) {
  if (is.null(timing)) return(NULL)
  calls <- timing$calls
  if (!"unit_id" %in% names(calls)) return(NULL)
  unit_id <- suppressWarnings(as.integer(calls$unit_id))
  keep <- !is.na(unit_id)
  if (!any(keep)) return(NULL)
  out <- stats::aggregate(
    calls$.duration_seconds[keep],
    list(unit_id = unit_id[keep]),
    FUN = sum
  )
  names(out)[2] <- "duration_seconds"
  out
}

archive_timing <- function(archive) {
  if (is.null(archive) || !inherits(archive, "archive")) return(NULL)
  records <- lapply(archive$records, function(record) {
    tryCatch(.archive_record_from_raw(record), error = function(e) NULL)
  })
  duration <- vapply(records, function(record) {
    if (is.null(record)) return(NA_real_)
    value <- record$duration %||% record$duration_s %||% NA_real_
    suppressWarnings(as.numeric(value)[1])
  }, numeric(1))
  keep <- is.finite(duration) & duration >= 0
  if (!any(keep)) return(NULL)
  data.frame(
    call = which(keep),
    provider = vapply(records[keep], function(record)
      as.character(record$provider %||% NA_character_), character(1)),
    model = vapply(records[keep], function(record)
      as.character(record$model %||% NA_character_), character(1)),
    duration_seconds = duration[keep],
    stringsAsFactors = FALSE
  )
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
