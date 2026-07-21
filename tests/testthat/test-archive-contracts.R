fix_contract_log <- function() {
  path <- tempfile(fileext = ".jsonl")
  writeLines(c(
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
  ), path, useBytes = TRUE)
  path
}

fix_contract_archive <- function() {
  archive_build(fix_contract_log(), name = "contracts")
}

test_that("archive_replay returns the LLMR runner shape without changing the glue name", {
  a <- fix_contract_archive()
  replay <- archive_replay(a)

  expect_s3_class(replay, "archive_replayer")
  expect_true(is.function(replay))
  expect_equal(attr(replay, "n_replayable"), 2L)
  expect_equal(attr(replay, "n_keys"), 2L)

  experiments <- tibble::tibble(
    config = list(
      LLMR::llm_config("groq", "openai/gpt-oss-20b", temperature = 0),
      LLMR::llm_config("openai", "gpt-4o-mini", temperature = 0)
    ),
    messages = list(
      c(user = "Label: positive?"),
      c(user = "Label: negative?")
    )
  )

  out <- replay(experiments)
  expect_s3_class(out, "tbl_df")
  expect_true(all(c("response_text", "sent_tokens", "rec_tokens",
                    "response_id", "success") %in% names(out)))
  expect_identical(out$response_text, c("positive", "negative"))
  expect_equal(out$sent_tokens, c(5, 6))
  expect_equal(out$rec_tokens, c(2, 2))
  expect_identical(out$response_id, c("r-1", "r-2"))
  expect_true(all(out$success))
})

test_that("sealed replay and checks derive from raw records", {
  a <- archive_seal(fix_contract_archive())
  experiments <- tibble::tibble(
    config = list(
      LLMR::llm_config("groq", "openai/gpt-oss-20b", temperature = 0),
      LLMR::llm_config("openai", "gpt-4o-mini", temperature = 0)
    ),
    messages = list(
      c(user = "Label: positive?"),
      c(user = "Label: negative?")
    )
  )
  expected <- c("positive", "negative")

  expect_identical(archive_replay(a)(experiments)$response_text, expected)

  manifest_tamper <- a
  manifest_tamper$manifest$request_hash[1] <-
    manifest_tamper$manifest$request_hash[2]
  checked <- archive_check(manifest_tamper)
  expect_true(checked$intact)
  expect_true(checked$root_ok)
  expect_length(checked$duplicate_request_hashes, 0L)
  expect_identical(
    archive_replay(manifest_tamper)(experiments)$response_text,
    expected
  )

  cache_tamper <- a
  cache_tamper$records[[1]]$rec <- list(
    kind = "call", request = list(), text = "tampered",
    response_id = "r-2"
  )
  checked <- archive_check(cache_tamper,
                           results = data.frame(response_id = "r-1"))
  expect_true(checked$intact)
  expect_true(checked$root_ok)
  expect_length(checked$duplicate_response_ids, 0L)
  expect_equal(checked$n_matched, 1L)
  expect_identical(archive_replay(cache_tamper)(experiments)$response_text,
                   expected)
})

test_that("archive_replay reports unmatched calls through the runner output", {
  replay <- archive_replay(fix_contract_archive())
  experiments <- tibble::tibble(
    config = list(LLMR::llm_config("openai", "gpt-4o-mini")),
    messages = list(c(user = "Not in the archive"))
  )

  expect_warning(out <- replay(experiments), "could not match")
  expect_false(out$success)
  expect_true(is.na(out$response_text))
  expect_identical(out$error_message, "not in archive")
})

# Same prompt, same provider/model, two different temperatures -> two distinct
# archived responses that must NOT collide on replay.
fix_temperature_log <- function() {
  path <- tempfile(fileext = ".jsonl")
  writeLines(c(
    paste0('{"ts":"2026-06-01T10:00:01+0000","schema_version":"1.0",',
           '"kind":"call","provider":"openai","model":"gpt-4o-mini","status":200,',
           '"request":{"messages":[{"role":"user","content":"Pick a number"}],',
           '"temperature":0},"usage":{"sent":3,"rec":1},',
           '"response_id":"cold","text":"7"}'),
    paste0('{"ts":"2026-06-01T10:00:02+0000","schema_version":"1.0",',
           '"kind":"call","provider":"openai","model":"gpt-4o-mini","status":200,',
           '"request":{"messages":[{"role":"user","content":"Pick a number"}],',
           '"temperature":1},"usage":{"sent":3,"rec":1},',
           '"response_id":"hot","text":"42"}')
  ), path, useBytes = TRUE)
  path
}

test_that("the same prompt at different temperatures does not collide on replay", {
  replay <- archive_replay(archive_build(fix_temperature_log()))
  expect_equal(attr(replay, "n_keys"), 2L)

  experiments <- tibble::tibble(
    config = list(
      LLMR::llm_config("openai", "gpt-4o-mini", temperature = 1),
      LLMR::llm_config("openai", "gpt-4o-mini", temperature = 0)
    ),
    messages = list(c(user = "Pick a number"), c(user = "Pick a number"))
  )
  out <- replay(experiments)
  # the temperature-1 row must get the temperature-1 response, not the first
  expect_identical(out$response_text, c("42", "7"))
  expect_identical(out$response_id, c("hot", "cold"))
})

test_that("replay is resettable so a deterministic pipeline runs twice", {
  replay <- archive_replay(fix_contract_archive())
  experiments <- tibble::tibble(
    config = list(LLMR::llm_config("groq", "openai/gpt-oss-20b", temperature = 0)),
    messages = list(c(user = "Label: positive?"))
  )
  first <- replay(experiments)
  expect_true(first$success)
  # the queue advanced; a second pass misses until reset
  expect_warning(second <- replay(experiments), "could not match")
  expect_false(second$success)

  LLMR::reset(replay)
  third <- replay(experiments)
  expect_true(third$success)
  expect_identical(third$response_text, "positive")
})

test_that("replay_mode 'first' is idempotent and 'strict_once' refuses reuse", {
  experiments <- tibble::tibble(
    config = list(LLMR::llm_config("groq", "openai/gpt-oss-20b", temperature = 0)),
    messages = list(c(user = "Label: positive?"))
  )

  first_mode <- archive_replay(fix_contract_archive(), replay_mode = "first")
  expect_true(first_mode(experiments)$success)
  expect_true(first_mode(experiments)$success)   # never exhausts

  strict <- archive_replay(fix_contract_archive(), replay_mode = "strict_once")
  expect_true(strict(experiments)$success)
  expect_error(strict(experiments), "strict_once")
})

test_that("calls differing only in presence_penalty get distinct replay keys", {
  # The replay key is now the full request hash (LLMR::llm_request_hash), which
  # keys on every generation parameter, not a fixed four. Two records that differ
  # only in presence_penalty therefore hash differently: two distinct keys, no
  # collision, and each replays to its own response.
  path <- tempfile(fileext = ".jsonl")
  writeLines(c(
    paste0('{"ts":"2026-06-01T10:00:01+0000","schema_version":"1.0",',
           '"kind":"call","provider":"openai","model":"gpt-4o-mini","status":200,',
           '"request":{"messages":[{"role":"user","content":"Hi"}],',
           '"temperature":0,"presence_penalty":0},"usage":{"sent":1,"rec":1},',
           '"response_id":"a","text":"one"}'),
    paste0('{"ts":"2026-06-01T10:00:02+0000","schema_version":"1.0",',
           '"kind":"call","provider":"openai","model":"gpt-4o-mini","status":200,',
           '"request":{"messages":[{"role":"user","content":"Hi"}],',
           '"temperature":0,"presence_penalty":2},"usage":{"sent":1,"rec":1},',
           '"response_id":"b","text":"two"}')
  ), path, useBytes = TRUE)
  a <- archive_build(path)
  replay <- archive_replay(a)
  expect_equal(attr(replay, "n_keys"), 2L)

  exps <- tibble::tibble(
    config = list(
      LLMR::llm_config("openai", "gpt-4o-mini", temperature = 0, presence_penalty = 2),
      LLMR::llm_config("openai", "gpt-4o-mini", temperature = 0, presence_penalty = 0)),
    messages = list(c(user = "Hi"), c(user = "Hi")))
  out <- replay(exps)
  expect_identical(out$response_text, c("two", "one"))
})

test_that("archive_drift uses the .runner seam and reports drift details", {
  a <- fix_contract_archive()
  echo <- function(experiments, ...) {
    user <- vapply(experiments$messages, `[[`, character(1), "user")
    experiments$response_text <- ifelse(grepl("positive", user), "positive", "negative")
    experiments$model_version <- ifelse(
      grepl("positive", user),
      "openai/gpt-oss-20b-2026-06-01",
      "gpt-4o-mini-2026-06-01"
    )
    experiments
  }

  set.seed(110)
  drift <- archive_drift(a, fraction = 1, .runner = echo)
  expect_s3_class(drift, "archive_drift")
  expect_s3_class(drift$table, "tbl_df")
  expect_s3_class(drift$details, "tbl_df")
  expect_equal(nrow(drift$details), 2L)
  expect_equal(sum(drift$details$exact), 2L)
  expect_true(all(c("n_exact", "exact_rate", "n_version_changed") %in% names(drift$table)))
  expect_true(all(drift$table$exact_rate == 1))
})

test_that("archive_drift separates fractional and count sampling", {
  a <- fix_contract_archive()
  echo <- function(experiments, ...) {
    user <- vapply(experiments$messages, `[[`, character(1), "user")
    experiments$response_text <- ifelse(grepl("positive", user),
                                        "positive", "negative")
    experiments
  }

  set.seed(110)
  by_fraction <- archive_drift(a, fraction = 1, .runner = echo)
  set.seed(110)
  by_count <- archive_drift(a, n = 1, .runner = echo)
  by_default <- archive_drift(a, .runner = echo)

  expect_equal(nrow(by_fraction$details), 2L)
  expect_equal(nrow(by_count$details), 1L)
  expect_equal(sum(by_count$table$n_sampled), 1L)
  expect_equal(nrow(by_default$details), 2L)
  expect_equal(nrow(archive_drift(
    a, n = .Machine$integer.max + 1, .runner = echo)$details), 2L)
  expect_error(archive_drift(a, fraction = 0.5, n = 1, .runner = echo),
               "only one")
  expect_error(archive_drift(a, fraction = 1.1, .runner = echo),
               "fraction")
  expect_error(archive_drift(a, n = 1.5, .runner = echo),
               "whole number")
})

test_that("archive_drift aligns reordered runner rows by sampled record", {
  a <- fix_contract_archive()
  reversed <- function(experiments, ...) {
    user <- vapply(experiments$messages, `[[`, character(1), "user")
    experiments$response_text <- ifelse(grepl("positive", user),
                                        "positive", "negative")
    experiments[rev(seq_len(nrow(experiments))), , drop = FALSE]
  }

  drift <- archive_drift(a, fraction = 1, .runner = reversed)
  expect_true(all(drift$details$exact))
})

test_that("archive_drift validates runner shape and failures", {
  bad_runner <- function(experiments, ...) experiments
  expect_error(
    archive_drift(fix_contract_archive(), fraction = 1, .runner = bad_runner),
    "response_text"
  )

  failed_runner <- function(experiments, ...) {
    experiments$response_text <- "unscored response"
    experiments$success <- FALSE
    experiments
  }
  expect_error(
    archive_drift(fix_contract_archive(), fraction = 1,
                  .runner = failed_runner),
    "runner failed"
  )
})

test_that("archive_drift derives sampled calls from raw records", {
  a <- archive_seal(fix_contract_archive())
  for (i in seq_along(a$records)) {
    a$records[[i]]$rec <- list(kind = "error", text = "cached tamper")
  }
  echo <- function(experiments, ...) {
    user <- vapply(experiments$messages, `[[`, character(1), "user")
    experiments$response_text <- ifelse(grepl("positive", user),
                                        "positive", "negative")
    experiments
  }

  drift <- archive_drift(a, fraction = 1, .runner = echo)
  expect_equal(nrow(drift$details), 2L)
  expect_true(all(drift$details$exact))
})

test_that("archive_drift excludes raw records without reconstructible messages", {
  path <- tempfile(fileext = ".jsonl")
  writeLines(paste0(
    '{"ts":"2026-06-01T10:00:01+0000","schema_version":"1.0",',
    '"kind":"call","provider":"openai","model":"m",',
    '"request":{"model":"m","temperature":0},',
    '"usage":{"sent":1,"rec":1},"response_id":"r1","text":"A"}'
  ), path, useBytes = TRUE)

  expect_error(
    archive_drift(archive_build(path), fraction = 1,
                  .runner = function(experiments, ...) stop("not reached")),
    "reconstructible requests"
  )
})

test_that("documented surface names remain real exports", {
  archive_exports <- c(
    "archive_build", "archive_check", "archive_drift", "archive_read",
    "archive_redact", "archive_replay", "archive_seal", "archive_write"
  )
  expect_true(all(archive_exports %in% getNamespaceExports("LLMRcontent")))

  llmr_exports <- c(
    "call_llm_par", "diagnostics", "llm_config", "llm_log_disable",
    "llm_log_enable", "llm_log_status", "report"
  )
  expect_true(all(llmr_exports %in% getNamespaceExports("LLMR")))

  base_exports <- c("data.frame", "readLines", "sample", "writeLines")
  expect_true(all(base_exports %in% getNamespaceExports("base")))
})

test_that("archive_drift names the unknown strata columns in plain text (regression)", {
  err <- tryCatch(
    archive_drift(fix_contract_archive(), fraction = 1,
                  strata = "no_such_column"),
    error = conditionMessage
  )
  expect_match(err, "no_such_column", fixed = TRUE)
  expect_false(grepl("{.field", err, fixed = TRUE))
})

test_that("archive_drift excludes NA temperatures from n_temperature0 (regression)", {
  # One record at temperature 0, one with no temperature at all: the
  # exact-reproduction denominator counts only the former.
  lines <- c(
    paste0('{"ts":"2026-06-01T10:00:01+0000","schema_version":"1.0",',
           '"kind":"call","provider":"openai","model":"m",',
           '"request":{"messages":[{"role":"user","content":"q1"}],',
           '"temperature":0},"usage":{"sent":1,"rec":1},',
           '"response_id":"r1","text":"A"}'),
    paste0('{"ts":"2026-06-01T10:00:02+0000","schema_version":"1.0",',
           '"kind":"call","provider":"openai","model":"m",',
           '"request":{"messages":[{"role":"user","content":"q2"}]},',
           '"usage":{"sent":1,"rec":1},"response_id":"r2","text":"B"}'))
  path <- tempfile(fileext = ".jsonl")
  writeLines(lines, path, useBytes = TRUE)
  a <- archive_build(path)
  echo <- function(experiments, ...) {
    experiments$response_text <- rep("A", nrow(experiments))
    experiments
  }
  set.seed(110)
  drift <- archive_drift(a, fraction = 1, .runner = echo)
  expect_true(anyNA(drift$details$temperature))
  expect_equal(sum(drift$table$n_temperature0), 1L)
})
