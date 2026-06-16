# archive_archive.R -------------------------------------------------------------
# The archive: every logged call, content-addressed. Two hashes per record,
# both computed by LLMR::llm_log_read(): the record hash (over the verbatim log
# line: tamper evidence) and the request hash (LLMR::llm_request_hash over the
# call's canonical turns and generation parameters: identity of the question
# asked, the key for replay and dedup).

#' Build a replication archive from an LLMR audit log
#'
#' Parses a JSONL log written by `LLMR::llm_log_enable()` into a
#' content-addressed archive: one entry per call, each carrying a record
#' hash (over the verbatim line) and, where the request body was logged, a
#' canonical request hash. Environment metadata (R, LLMR, and log schema
#' versions) is captured alongside.
#'
#' @param log Path to the JSONL audit log.
#' @param name Optional label for the archive (defaults to the file name).
#' @return An `archive` (unsealed).
#' @examples
#' # In a real study the log comes from LLMR::llm_log_enable("study.jsonl"),
#' # left on for the whole project. Here, one record written by hand:
#' log <- tempfile(fileext = ".jsonl")
#' writeLines(paste0('{"ts":"2026-06-01T10:00:01+0000","schema_version":"1.0",',
#'   '"kind":"call","provider":"groq","model":"openai/gpt-oss-20b",',
#'   '"request":{"messages":[{"role":"user","content":"Label: positive?"}]},',
#'   '"usage":{"sent":5,"rec":2},"response_id":"r-1","text":"positive"}'), log)
#' a <- archive_build(log)
#' a <- archive_seal(a)
#' archive_check(a)
#' @seealso [archive_seal()], [archive_check()], [archive_redact()]
#' @export
archive_build <- function(log, name = NULL) {
  stopifnot(file.exists(log))
  # Parsing a log into records + a per-record manifest (with the canonical
  # request hash and the record-line hash) is generic LLMR-log infrastructure;
  # building the sealable archive object on top of it is this package's concern.
  read <- LLMR::llm_log_read(log)

  structure(
    list(name = name %||% basename(log),
         manifest = read$manifest,
         records = read$records,
         env = list(
           r_version = as.character(getRversion()),
           llmr_version = as.character(utils::packageVersion("LLMR")),
           llmrcontent_version = as.character(utils::packageVersion("LLMRcontent")),
           platform = R.version$platform,
           built = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")),
         sealed = FALSE, seal = NULL, redacted = FALSE),
    class = "archive"
  )
}

#' Archive the currently active audit log
#'
#' Convenience for the end of a session: archives whatever log
#' `LLMR::llm_log_enable()` is currently writing (logging is left on; build
#' the final sealed archive after `LLMR::llm_log_disable()`).
#'
#' @param name Optional archive label.
#' @return An `archive`, or an error when no log is active.
#' @export
archive_current <- function(name = NULL) {
  path <- suppressMessages(LLMR::llm_log_status())
  if (is.null(path)) {
    abort("No audit log is active; enable one with LLMR::llm_log_enable().")
  }
  archive_build(path, name = name)
}

#' Seal an archive under a root hash
#'
#' Computes a single root hash over the ordered record hashes plus the
#' environment block. Any subsequent change to any record -- one character
#' of one prompt -- changes the root. Cite the root in the paper; deposit
#' the archive; the two now vouch for each other.
#'
#' @param archive An [archive_build()] result.
#' @return The archive, sealed, with `$seal` set.
#' @examples
#' log <- tempfile(fileext = ".jsonl")
#' writeLines(paste0('{"ts":"2026-06-01T10:00:01+0000","schema_version":"1.0",',
#'   '"kind":"call","provider":"groq","model":"openai/gpt-oss-20b",',
#'   '"request":{"q":1},"usage":{"sent":5,"rec":2},',
#'   '"response_id":"r-1","text":"reply"}'), log)
#' a <- archive_seal(archive_build(log))
#' substr(a$seal$root, 1, 12)   # cite this root in the paper
#' @export
archive_seal <- function(archive) {
  stopifnot(inherits(archive, "archive"))
  root <- .hash_chr(paste(c(archive$manifest$record_hash,
                            .hash_obj(archive$env)), collapse = "|"))
  archive$seal <- list(root = root,
                       sealed_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
                       n_records = nrow(archive$manifest))
  archive$sealed <- TRUE
  archive
}

#' Verify an archive's integrity and (optionally) a result's completeness
#'
#' Integrity: every record hash is recomputed from the stored verbatim
#' lines and compared to the manifest -- against the original record hashes
#' for a full archive, against the `public_record_hash` family for a
#' redacted one (whose original hashes attest content held elsewhere; see
#' [archive_redact()]). For a sealed archive the root is recomputed too;
#' the root always refers to the original content. Completeness: given a
#' results frame that carries `response_id` (as `LLMR::call_llm_par()`
#' returns), reports how many of the paper's results map to a logged call
#' -- numbers without provenance are exactly what this package exists to
#' make visible.
#'
#' @param archive An `archive`.
#' @param results Optional data frame with a `response_id` column.
#' @return A list of class `archive_check`: `intact`, `redacted` (a logical
#'   redaction flag; when TRUE, `intact` was checked against the
#'   `public_record_hash` family rather than the original record hashes),
#'   `root_ok`, `n_records`,
#'   `bad_records` (indices), `duplicate_response_ids` and
#'   `duplicate_request_hashes` (each a character vector of any values that
#'   appear on more than one record -- a sign of a malformed or merged log), and,
#'   when `results` was given, `n_results`, `n_matched`, `unmatched_ids`.
#' @examples
#' log <- tempfile(fileext = ".jsonl")
#' writeLines(paste0('{"ts":"2026-06-01T10:00:01+0000","schema_version":"1.0",',
#'   '"kind":"call","provider":"groq","model":"openai/gpt-oss-20b",',
#'   '"request":{"q":1},"usage":{"sent":5,"rec":2},',
#'   '"response_id":"r-1","text":"reply"}'), log)
#' a <- archive_seal(archive_build(log))
#' archive_check(a)
#' archive_check(a, results = data.frame(response_id = c("r-1", "orphan")))
#' @export
archive_check <- function(archive, results = NULL) {
  stopifnot(inherits(archive, "archive"))
  rehash <- vapply(archive$records, function(r) .hash_chr(r$raw), character(1))
  reference <- if (isTRUE(archive$redacted)) {
    if (is.null(archive$manifest$public_record_hash)) {
      abort("Redacted archive without public hashes; redact with archive_redact().")
    }
    archive$manifest$public_record_hash
  } else {
    archive$manifest$record_hash
  }
  bad <- which(rehash != reference)
  root_ok <- NA
  if (isTRUE(archive$sealed)) {
    root <- .hash_chr(paste(c(archive$manifest$record_hash,
                              .hash_obj(archive$env)), collapse = "|"))
    root_ok <- identical(root, archive$seal$root)
  }
  # Duplicate diagnostics: a well-formed log gives each call a distinct
  # response_id, and a distinct request_hash unless a call was genuinely
  # repeated. Duplicates in either are a sign of a malformed or merged log and
  # undermine the response_id -> call mapping used for completeness, so surface
  # them.
  logged_ids <- vapply(archive$records, function(r)
    as.character(r$rec$response_id %||% NA_character_), character(1))
  id_present <- logged_ids[!is.na(logged_ids)]
  dup_response_ids <- unique(id_present[duplicated(id_present)])
  rh <- archive$manifest$request_hash
  rh_present <- rh[!is.na(rh)]
  dup_request_hashes <- unique(rh_present[duplicated(rh_present)])

  out <- list(intact = length(bad) == 0L, redacted = isTRUE(archive$redacted),
              root_ok = root_ok,
              n_records = nrow(archive$manifest), bad_records = bad,
              duplicate_response_ids = dup_response_ids,
              duplicate_request_hashes = dup_request_hashes)
  if (!is.null(results)) {
    stopifnot(is.data.frame(results))
    if (!"response_id" %in% names(results)) {
      abort("`results` must carry a `response_id` column to check completeness.")
    }
    ids <- results$response_id
    out$n_results <- length(ids)
    out$n_matched <- sum(ids %in% logged_ids, na.rm = TRUE)
    out$unmatched_ids <- ids[!ids %in% logged_ids & !is.na(ids)]
  }
  class(out) <- "archive_check"
  out
}

#' @export
print.archive_check <- function(x, ...) {
  cat(sprintf("<archive_check | %d record(s) | integrity%s: %s%s>\n",
              x$n_records,
              if (isTRUE(x$redacted)) " (public hashes)" else "",
              if (x$intact) "INTACT" else
                sprintf("TAMPERED (%d bad)", length(x$bad_records)),
              if (!is.na(x$root_ok))
                paste0(" | seal: ", if (x$root_ok) "VALID" else "BROKEN") else ""))
  if (isTRUE(x$redacted)) {
    cat("  original hash tree preserved; content attestable against the unredacted original\n")
  }
  if (!is.null(x$n_results)) {
    cat(sprintf("  completeness: %d/%d result rows map to a logged call\n",
                x$n_matched, x$n_results))
  }
  if (length(x$duplicate_response_ids)) {
    cat(sprintf("  WARNING: %d duplicate response_id(s) -- the log may be malformed or merged\n",
                length(x$duplicate_response_ids)))
  }
  if (length(x$duplicate_request_hashes)) {
    cat(sprintf("  note: %d request(s) appear more than once (same call logged repeatedly)\n",
                length(x$duplicate_request_hashes)))
  }
  invisible(x)
}

#' Redact an archive's content while keeping its hash tree
#'
#' Removes prompts and reply text from every record and re-serializes them
#' with a `redacted` marker. Two hash families then coexist, explicitly:
#'
#' - the **original** record hashes and the seal root stay in the manifest
#'   untouched -- they attest the full content, checkable by whoever holds
#'   the unredacted archive (the authors, under IRB terms);
#' - a **public** hash per redacted record (`public_record_hash`) is added,
#'   and [archive_check()] verifies a redacted archive against these, so
#'   the public artifact has its own working integrity check.
#'
#' What a reviewer gets from the public artifact: how many calls, to which
#' models, with which parameters, when, at what token cost, under which
#' root -- everything except the sentences.
#'
#' @param archive An `archive` (not already redacted).
#' @return The archive with content removed, `$redacted = TRUE`, and
#'   `public_record_hash` filled in the manifest.
#' @examples
#' log <- tempfile(fileext = ".jsonl")
#' writeLines(paste0('{"ts":"2026-06-01T10:00:01+0000","schema_version":"1.0",',
#'   '"kind":"call","provider":"groq","model":"openai/gpt-oss-20b",',
#'   '"request":{"messages":[{"role":"user","content":"secret text"}]},',
#'   '"usage":{"sent":5,"rec":2},"response_id":"r-1","text":"reply"}'), log)
#' a <- archive_seal(archive_build(log))
#' r <- archive_redact(a)
#' archive_check(r)                       # verifies against public hashes
#' identical(r$seal$root, a$seal$root)    # original root preserved
#' @export
archive_redact <- function(archive) {
  stopifnot(inherits(archive, "archive"))
  if (isTRUE(archive$redacted)) abort("This archive is already redacted.")
  archive$records <- lapply(archive$records, function(r) {
    rec <- r$rec
    rec$request <- NULL
    rec$text <- NULL
    rec$redacted <- TRUE
    list(raw = as.character(jsonlite::toJSON(rec, auto_unbox = TRUE,
                                             null = "null")),
         rec = rec)
  })
  archive$manifest$public_record_hash <-
    vapply(archive$records, function(r) .hash_chr(r$raw), character(1))
  archive$redacted <- TRUE
  archive
}

#' @export
print.archive <- function(x, ...) {
  m <- x$manifest
  cat(sprintf("<archive '%s' | %d call(s) | %s | %s%s>\n",
              x$name, nrow(m),
              paste(unique(stats::na.omit(m$provider)), collapse = ", "),
              if (x$sealed) paste0("SEALED ", substr(x$seal$root, 1, 12))
              else "unsealed",
              if (x$redacted) " | REDACTED" else ""))
  h <- verifiability_horizon(x)
  for (i in seq_len(nrow(h))) {
    cat(sprintf("  %-34s %4d call(s)  %s\n", h$model[i], h$calls[i], h$class[i]))
  }
  invisible(x)
}

#' Convert an archive manifest to a tibble
#'
#' @param x An `archive`.
#' @param ... Passed to [tibble::as_tibble()].
#' @return The archive manifest as a tibble.
#' @exportS3Method tibble::as_tibble
as_tibble.archive <- function(x, ...) {
  stopifnot(inherits(x, "archive"))
  tibble::as_tibble(x$manifest, ...)
}

#' Machine-readable archive diagnostics
#'
#' @param x An `archive`.
#' @param ... Passed to [verifiability_horizon()], such as `open_patterns`.
#' @return A one-row tibble with record count, seal state, root, redaction
#'   state, and horizon counts.
#' @exportS3Method LLMR::diagnostics
diagnostics.archive <- function(x, ...) {
  stopifnot(inherits(x, "archive"))
  h <- verifiability_horizon(x, ...)
  tibble::tibble(
    n_records = as.integer(nrow(x$manifest)),
    sealed = isTRUE(x$sealed),
    root = if (isTRUE(x$sealed) && !is.null(x$seal$root))
      as.character(x$seal$root) else NA_character_,
    redacted = isTRUE(x$redacted),
    n_open_pinnable = as.integer(sum(h$calls[h$class %in% "open-pinnable"],
                                     na.rm = TRUE)),
    n_api_contingent = as.integer(sum(h$calls[h$class %in% "api-contingent"],
                                      na.rm = TRUE))
  )
}

#' Draft an archive report through the shared LLMR generic
#'
#' @param x An `archive`.
#' @param ... Passed to [archive_appendix()].
#' @return Character lines of class `archive_appendix`.
#' @exportS3Method LLMR::report
report.archive <- function(x, ...) {
  stopifnot(inherits(x, "archive"))
  archive_appendix(x, ...)
}
