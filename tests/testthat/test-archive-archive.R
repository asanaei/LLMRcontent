fix_archive_log <- function(lines = NULL) {
  if (is.null(lines)) {
    lines <- c(
      paste0('{"ts":"2026-06-01T10:00:01+0000","schema_version":"1.0",',
             '"kind":"call","provider":"groq","model":"openai/gpt-oss-20b",',
             '"model_version":"openai/gpt-oss-20b-2026-06-01","status":200,',
             '"request":{"messages":[{"role":"user","content":"Label: positive?"}],',
             '"temperature":0},"usage":{"sent":5,"rec":2},',
             '"response_id":"r-1","text":"positive"}'),
      paste0('{"ts":"2026-06-01T10:00:02+0000","schema_version":"1.0",',
             '"kind":"call","provider":"openai","model":"gpt-4o-mini",',
             '"model_version":"gpt-4o-mini-2026-06-01","status":200,',
             '"request":{"messages":[{"role":"user","content":"Label: negative?"}],',
             '"temperature":0},"usage":{"sent":6,"rec":2},',
             '"response_id":"r-2","text":"negative"}')
    )
  }
  path <- tempfile(fileext = ".jsonl")
  writeLines(lines, path, useBytes = TRUE)
  path
}

fix_archive <- function(name = "demo") {
  archive_seal(archive_build(fix_archive_log(), name = name))
}

test_that("archives build, seal, print, and check under the archive class", {
  a <- archive_build(fix_archive_log(), name = "demo")
  expect_s3_class(a, "archive")
  expect_false(inherits(a, "llmr_archive"))
  expect_false(a$sealed)
  expect_equal(nrow(a$manifest), 2L)
  expect_true(all(c("request_hash", "record_hash") %in% names(a$manifest)))

  a <- archive_seal(a)
  expect_true(a$sealed)
  expect_match(a$seal$root, "^[a-f0-9]{64}$")
  expect_output(print(a), "<archive 'demo'", fixed = TRUE)

  chk <- archive_check(a, results = data.frame(response_id = c("r-1", "orphan")))
  expect_s3_class(chk, "archive_check")
  expect_true(chk$intact)
  expect_true(chk$records_ok)
  expect_true(chk$root_ok)
  expect_true(is.na(chk$public_root_ok))
  expect_equal(chk$n_records, 2L)
  expect_equal(chk$n_results, 2L)
  expect_equal(chk$n_matched, 1L)
  expect_identical(chk$unmatched_ids, "orphan")
})

test_that("redaction preserves the original seal and creates public hashes", {
  a <- fix_archive()
  r <- archive_redact(a)

  expect_s3_class(r, "archive")
  expect_true(r$redacted)
  expect_true("public_record_hash" %in% names(r$manifest))
  expect_identical(r$seal$root, a$seal$root)
  expect_match(r$seal$public_root, "^[a-f0-9]{64}$")
  public_record <- jsonlite::fromJSON(r$records[[1]]$raw,
                                      simplifyVector = FALSE)
  expect_null(public_record$request)
  expect_null(public_record$text)
  expect_identical(public_record$parameters$temperature, 0L)
  checked <- archive_check(r)
  expect_true(checked$intact)
  expect_true(checked$records_ok)
  expect_true(checked$root_ok)
  expect_true(checked$public_root_ok)
  expect_error(archive_redact(archive_build(fix_archive_log())),
               "requires a sealed archive")
  expect_error(archive_redact(r), "already redacted")
  expect_error(archive_replay(r), "redacted")
})

test_that("the public root detects coordinated redacted record tampering", {
  r <- archive_redact(fix_archive())
  rec <- jsonlite::fromJSON(r$records[[1]]$raw, simplifyVector = FALSE)
  rec$model <- "tampered-model"
  r$records[[1]]$raw <- as.character(jsonlite::toJSON(
    rec, auto_unbox = TRUE, null = "null"))
  r$records[[1]]$rec <- rec
  r$manifest$public_record_hash[1] <-
    LLMRcontent:::.hash_chr(r$records[[1]]$raw)

  checked <- archive_check(r)
  expect_true(checked$records_ok)
  expect_length(checked$bad_records, 0L)
  expect_true(checked$root_ok)
  expect_false(checked$public_root_ok)
  expect_false(checked$intact)
  expect_output(print(checked), "public root: BROKEN", fixed = TRUE)
})

test_that("redaction uses authenticated raw records and refuses broken input", {
  a <- fix_archive()
  original_model <- a$records[[1]]$rec$model
  a$records[[1]]$rec$model <- "forged-cache-model"

  r <- archive_redact(a)
  expect_identical(r$records[[1]]$rec$model, original_model)
  expect_true(archive_check(r)$intact)

  broken <- fix_archive()
  rec <- jsonlite::fromJSON(broken$records[[1]]$raw,
                            simplifyVector = FALSE)
  rec$model <- "forged-raw-model"
  broken$records[[1]]$raw <- as.character(jsonlite::toJSON(
    rec, auto_unbox = TRUE, null = "null"))
  broken$records[[1]]$rec <- rec
  expect_error(archive_redact(broken), "intact sealed archive")
})

test_that("the original root detects coordinated raw and manifest tampering", {
  a <- fix_archive()
  rec <- jsonlite::fromJSON(a$records[[1]]$raw, simplifyVector = FALSE)
  rec$text <- "tampered response"
  a$records[[1]]$raw <- as.character(jsonlite::toJSON(
    rec, auto_unbox = TRUE, null = "null"))
  a$records[[1]]$rec <- rec
  a$manifest$record_hash[1] <-
    LLMRcontent:::.hash_chr(a$records[[1]]$raw)

  checked <- archive_check(a)
  expect_true(checked$records_ok)
  expect_length(checked$bad_records, 0L)
  expect_false(checked$root_ok)
  expect_true(is.na(checked$public_root_ok))
  expect_false(checked$intact)
  expect_output(print(checked), "integrity: BROKEN", fixed = TRUE)
})

test_that("archives write, read, and verify as separate acts", {
  a <- fix_archive()
  dir <- tempfile("archive-")

  expect_invisible(archive_write(a, dir))
  expect_true(file.exists(file.path(dir, "records.jsonl")))
  expect_true(file.exists(file.path(dir, "manifest.json")))
  expect_error(archive_write(a, dir), "already exists")
  expect_invisible(archive_write(a, dir, overwrite = TRUE))

  b <- archive_read(dir)
  expect_s3_class(b, "archive")
  expect_false(inherits(b, "llmr_archive"))
  expect_true(archive_check(b)$intact)
  expect_true(archive_check(b)$root_ok)
  expect_identical(b$seal$root, a$seal$root)
})

test_that("diagnostics, report, and as_tibble dispatch on sealed archives", {
  a <- fix_archive()

  d <- LLMR::diagnostics(a)
  expect_s3_class(d, "tbl_df")
  expect_named(d, c("n_records", "sealed", "root", "redacted",
                    "n_open_pinnable", "n_api_contingent"))
  expect_equal(d$n_records, 2L)
  expect_true(d$sealed)
  expect_match(d$root, "^[a-f0-9]{64}$")
  expect_false(d$redacted)
  expect_equal(d$n_open_pinnable, 1L)
  expect_equal(d$n_api_contingent, 1L)

  rep <- LLMR::report(a)
  expect_s3_class(rep, "archive_appendix")
  expect_true(any(grepl("VERIFIABILITY HORIZON", unclass(rep), fixed = TRUE)))

  man <- tibble::as_tibble(a)
  expect_s3_class(man, "tbl_df")
  expect_equal(nrow(man), 2L)
  expect_false("response_id" %in% names(man))
  expect_true(all(c("provider", "model", "record_hash") %in% names(man)))
})

test_that("archive_check flags duplicate response_ids and repeated requests", {
  dup <- c(
    paste0('{"ts":"2026-06-01T10:00:01+0000","schema_version":"1.0","kind":"call",',
           '"provider":"groq","model":"m","status":200,',
           '"request":{"messages":[{"role":"user","content":"Q?"}],"temperature":0},',
           '"usage":{"sent":5,"rec":2},"response_id":"dup","text":"a"}'),
    paste0('{"ts":"2026-06-01T10:00:02+0000","schema_version":"1.0","kind":"call",',
           '"provider":"groq","model":"m","status":200,',
           '"request":{"messages":[{"role":"user","content":"Q?"}],"temperature":0},',
           '"usage":{"sent":5,"rec":2},"response_id":"dup","text":"b"}'))
  a <- archive_build(fix_archive_log(lines = dup))
  chk <- archive_check(a)
  # both records share response_id "dup" -> one duplicate id flagged
  expect_equal(chk$duplicate_response_ids, "dup")
  # identical request bodies -> one duplicate request hash flagged
  expect_length(chk$duplicate_request_hashes, 1L)
  expect_output(print(chk), "duplicate response_id")
})

test_that("a clean archive reports no duplicates", {
  chk <- archive_check(archive_build(fix_archive_log()))
  expect_named(chk, c(
    "records_ok", "root_ok", "public_root_ok", "intact", "redacted",
    "n_records", "bad_records",
    "duplicate_response_ids", "duplicate_request_hashes", "n_results",
    "n_matched", "unmatched_ids"
  ))
  expect_length(chk$duplicate_response_ids, 0L)
  expect_length(chk$duplicate_request_hashes, 0L)
  expect_identical(chk$n_results, NA_integer_)
  expect_identical(chk$n_matched, NA_integer_)
  expect_identical(chk$unmatched_ids, character(0))
})

test_that("a round-trip keeps manifest columns that are entirely NA (regression)", {
  # On a log whose records never set model_version (and whose only failure is
  # a status-less error record), toJSON without na = "null" used to drop the
  # all-NA columns row-wise, and archive_read() then lost them -- silently
  # zeroing the appendix failure count.
  lines <- c(
    paste0('{"ts":"2026-06-01T10:00:01+0000","schema_version":"1.0",',
           '"kind":"call","provider":"groq","model":"openai/gpt-oss-20b",',
           '"request":{"messages":[{"role":"user","content":"hi"}],',
           '"temperature":0},"usage":{"sent":5,"rec":2},',
           '"response_id":"r-1","text":"reply"}'),
    paste0('{"ts":"2026-06-01T10:00:02+0000","schema_version":"1.0",',
           '"kind":"error","provider":"groq","model":"openai/gpt-oss-20b",',
           '"message":"boom"}'))
  a <- archive_seal(archive_build(fix_archive_log(lines), name = "na-cols"))
  expect_true(all(is.na(a$manifest$model_version)))
  expect_true(all(is.na(a$manifest$status)))

  dir <- tempfile("archive-na-")
  archive_write(a, dir)
  b <- archive_read(dir)

  expect_setequal(names(b$manifest), names(a$manifest))
  fails_before <- grep("failure", LLMR::report(a), value = TRUE)
  fails_after <- grep("failure", LLMR::report(b), value = TRUE)
  expect_identical(fails_after, fails_before)
  expect_match(fails_after, "1 failure(s)", fixed = TRUE)
  expect_true(archive_check(b)$intact)
  expect_true(archive_check(b)$root_ok)
})
