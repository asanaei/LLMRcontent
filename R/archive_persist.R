# archive_persist.R ---------------------------------------------------------------------
# On disk an archive is two files in one directory: the records verbatim
# (records.jsonl) and the manifest with environment, hashes, and seal
# (manifest.json). Plain text, diffable, depositable.

#' Write an archive to a directory
#'
#' @param archive An `archive`.
#' @param dir Target directory (created if missing).
#' @return `dir`, invisibly.
#' @examples
#' log <- tempfile(fileext = ".jsonl")
#' writeLines(paste0('{"ts":"2026-06-01T10:00:01+0000","schema_version":"1.0",',
#'   '"kind":"call","provider":"groq","model":"openai/gpt-oss-20b",',
#'   '"usage":{"sent":5,"rec":2},"response_id":"r-1","text":"reply"}'), log)
#' a <- archive_seal(archive_build(log, name = "demo"))
#' dir <- file.path(tempdir(), "demo-archive")
#' archive_write(a, dir)
#' b <- archive_read(dir)
#' archive_check(b)          # reading and verifying are separate acts
#' identical(b$seal$root, a$seal$root)
#' @seealso [archive_read()]
#' @export
archive_write <- function(archive, dir) {
  stopifnot(inherits(archive, "archive"))
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  writeLines(vapply(archive$records, `[[`, "", "raw"),
             file.path(dir, "records.jsonl"), useBytes = TRUE)
  meta <- list(name = archive$name, env = archive$env,
               sealed = archive$sealed, seal = archive$seal,
               redacted = archive$redacted,
               manifest = archive$manifest)
  writeLines(as.character(jsonlite::toJSON(meta, auto_unbox = TRUE,
                                           null = "null", digits = NA)),
             file.path(dir, "manifest.json"), useBytes = TRUE)
  invisible(dir)
}

#' Read an archive from a directory
#'
#' @param dir A directory written by [archive_write()].
#' @return An `archive`. Run [archive_check()] right after: reading and
#'   verifying are deliberately separate acts.
#' @export
archive_read <- function(dir) {
  rec_path <- file.path(dir, "records.jsonl")
  man_path <- file.path(dir, "manifest.json")
  if (!file.exists(rec_path) || !file.exists(man_path)) {
    abort("Not an archive directory (records.jsonl + manifest.json expected).")
  }
  meta <- jsonlite::fromJSON(readLines(man_path, warn = FALSE),
                             simplifyVector = TRUE)
  lines <- readLines(rec_path, warn = FALSE)
  records <- lapply(lines, function(ln) {
    list(raw = ln, rec = jsonlite::fromJSON(ln, simplifyVector = FALSE))
  })
  structure(
    list(name = meta$name, manifest = tibble::as_tibble(meta$manifest),
         records = records, env = as.list(meta$env),
         sealed = isTRUE(meta$sealed),
         seal = if (!is.null(meta$seal)) as.list(meta$seal) else NULL,
         redacted = isTRUE(meta$redacted)),
    class = "archive"
  )
}
