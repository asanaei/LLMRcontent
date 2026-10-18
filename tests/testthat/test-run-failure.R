# A failed call stops a run with a condition that keeps the completed rows
# and names the failed ones.

# Fails the calls for which `fail(experiments)` is TRUE on pass `on_pass`;
# answers every other call correctly. Counts the passes it was asked to run.
failing_runner <- function(fail, on_pass = 1L, missing = FALSE) {
  calls <- 0L
  runner <- function(experiments, ...) {
    calls <<- calls + 1L
    out <- fake_runner_perfect(experiments, ...)
    if (calls == on_pass) {
      bad <- fail(out)
      if (missing) {
        out$response_text[bad] <- NA_character_
        out$success <- NULL
      } else {
        out$response_text[bad] <- NA_character_
        out$success[bad] <- FALSE
        out$error_message <- ifelse(bad, "HTTP 503", NA_character_)
      }
    }
    out
  }
  list(runner = runner, calls = function() calls)
}

catch_run_failure <- function(expr) {
  tryCatch({
    expr
    NULL
  }, llmrcontent_run_failure = function(e) e)
}

test_that("a failure in a later replicate pass returns the completed rows", {
  p <- protocol_lock(protocol(fix_codebook(), fix_config(), replicates = 3L))
  corpus <- data.frame(doc_id = c("007", "010", "123"),
                       text = c("great news today", "terrible outcome",
                                "lovely weather"))
  r <- failing_runner(function(out) out$unit_id == 2L, on_pass = 2L)
  e <- catch_run_failure(
    code_corpus(corpus, p, "text", id = "doc_id", .runner = r$runner))

  expect_s3_class(e, "llmrcontent_run_failure")
  expect_s3_class(e, "rlang_error")
  expect_match(conditionMessage(e), "reported failure for 1 experiment row")
  expect_identical(r$calls(), 2L)

  done <- e$completed
  expect_s3_class(done, "tbl_df")
  expect_identical(names(done)[1], "doc_id")
  expect_identical(nrow(done), 5L)
  expect_identical(as.integer(table(done$replicate)), c(3L, 2L))
  expect_identical(done$doc_id[done$replicate == 2L], c("007", "123"))
  expect_true(all(done$success))
  expect_false(anyNA(done$response_text))
  expect_true(all(c("protocol_id", "unit_id", "messages", "config",
                    "response_text", "sent_tokens") %in% names(done)))

  bad <- e$failed
  expect_named(bad, c("doc_id", "protocol_id", "unit_id", "replicate",
                      "error_message"))
  expect_identical(bad$doc_id, "010")
  expect_identical(as.integer(bad$unit_id), 2L)
  expect_identical(as.integer(bad$replicate), 2L)
  expect_identical(bad$error_message, "HTTP 503")
})

test_that("a missing response without a success column is a failed call", {
  p <- protocol_lock(protocol(fix_codebook(), fix_config()))
  corpus <- data.frame(text = c("great news today", "terrible outcome",
                                "lovely weather"))
  r <- failing_runner(function(out) out$unit_id %in% c(1L, 3L), missing = TRUE)
  e <- catch_run_failure(code_corpus(corpus, p, "text", .runner = r$runner))

  expect_s3_class(e, "llmrcontent_run_failure")
  expect_match(conditionMessage(e), "missing response for 2 experiment row")
  expect_identical(as.integer(e$completed$unit_id), 2L)
  expect_false("success" %in% names(e$completed))
  expect_named(e$failed, c("protocol_id", "unit_id", "replicate"))
  expect_identical(as.integer(e$failed$unit_id), c(1L, 3L))
})

test_that("the message counts reported failures and missing responses", {
  p <- protocol_lock(protocol(fix_codebook(), fix_config()))
  corpus <- data.frame(text = c("great news today", "terrible outcome",
                                "lovely weather"))
  runner <- function(experiments, ...) {
    out <- fake_runner_perfect(experiments, ...)
    out$success[1] <- FALSE
    out$response_text[1] <- "positive"
    out$response_text[3] <- NA_character_
    out
  }
  e <- catch_run_failure(code_corpus(corpus, p, "text", .runner = runner))
  expect_match(conditionMessage(e), paste(
    "reported failure for 1 experiment row\\(s\\) and returned a missing",
    "response for 1 more"))
  expect_identical(as.integer(e$failed$unit_id), c(1L, 3L))
  expect_identical(as.integer(e$completed$unit_id), 2L)
})

test_that("parse failures and successful runs raise no run failure", {
  p <- protocol_lock(protocol(fix_codebook(), fix_config(), replicates = 2L))
  corpus <- data.frame(text = c("great news today", "terrible outcome",
                                "lovely weather"))
  junk <- function(experiments, ...) {
    out <- fake_runner_perfect(experiments, ...)
    out$response_text <- "???"
    out
  }
  coded <- code_corpus(corpus, p, "text", .runner = junk)
  expect_true(all(is.na(coded$data$label)))
  expect_identical(coded$data$parse_failures, c(2L, 2L, 2L))

  coded <- code_corpus(corpus, p, "text", .runner = fake_runner_perfect)
  expect_identical(coded$data$label, c("positive", "negative", "positive"))
})

test_that("a malformed runner result is not a recoverable run failure", {
  p <- protocol_lock(protocol(fix_codebook(), fix_config()))
  dropped <- function(experiments, ...) {
    fake_runner_perfect(experiments, ...)[-1, , drop = FALSE]
  }
  e <- tryCatch(code_corpus(data.frame(text = c("great", "bad")), p, "text",
                            .runner = dropped),
                error = function(e) e)
  expect_match(conditionMessage(e), "must return the experiment rows")
  expect_false(inherits(e, "llmrcontent_run_failure"))
})

test_that("an identifier column named unit_id replaces the position", {
  p <- protocol_lock(protocol(fix_codebook(), fix_config()))
  corpus <- data.frame(unit_id = c(901L, 902L),
                       text = c("great news today", "terrible outcome"))
  r <- failing_runner(function(out) out$unit_id == 2L)
  e <- catch_run_failure(
    code_corpus(corpus, p, "text", id = "unit_id", .runner = r$runner))
  expect_identical(e$failed$unit_id, 902L)
  expect_identical(e$completed$unit_id, 901L)
  expect_identical(names(e$failed)[1], "unit_id")
})

test_that("tuning and validation failures carry the gold-set id", {
  set.seed(110)
  gold <- suppressWarnings(gold_set(
    data.frame(case = sprintf("c%03d", 1:8),
               text = c("great news today", "terrible outcome",
                        "lovely weather", "awful decision",
                        "wonderful result", "dreadful mess",
                        "fine work", "bad call"),
               label = rep(c("positive", "negative"), 4)),
    text = "text", label = "label", split = c(dev = 0.5, test = 0.5),
    id = "case"))
  dev <- gold_split(gold, "dev")
  target <- dev$text[3]
  protocols <- list(
    protocol(fix_codebook(), fix_config(), label = "a"),
    protocol(fix_codebook(), fix_config(), label = "b"))
  r <- failing_runner(function(out) out$protocol_id == 2L & out$text == target)
  e <- catch_run_failure(tune_protocol(protocols, gold, .runner = r$runner))
  expect_s3_class(e, "llmrcontent_run_failure")
  expect_identical(e$failed$case, dev$case[3])
  expect_identical(as.integer(e$failed$protocol_id), 2L)
  expect_identical(as.integer(e$failed$unit_id), 3L)
  expect_identical(nrow(e$completed), 2L * nrow(dev) - 1L)
  expect_setequal(e$completed$case[e$completed$protocol_id == 1L], dev$case)

  test <- gold_split(gold, "test")
  locked <- protocol_lock(protocols[[1]])
  r <- failing_runner(function(out) out$text == test$text[1])
  e <- catch_run_failure(validate_protocol(locked, gold, .runner = r$runner))
  expect_identical(e$failed$case, test$case[1])
  expect_identical(sort(e$completed$case), sort(test$case[-1]))
  expect_identical(nrow(gold_ledger(gold)), 0L)

  plain <- fix_gold(8)
  r <- failing_runner(function(out) out$unit_id == 1L)
  e <- catch_run_failure(validate_protocol(locked, plain, split = "dev",
                                           .runner = r$runner))
  expect_named(e$failed, c("protocol_id", "unit_id", "replicate",
                           "error_message"))
})

test_that("audit_run failures keep the completed rows of the grid", {
  plan <- audit_plan(
    data.frame(text = c("cut taxes now", "fund the schools", "expand care")),
    "text", function(d) mean(d$label == "yes", na.rm = TRUE),
    c("yes", "no"), "{text}")
  plan <- audit_add_models(plan, list(
    a = LLMR::llm_config("groq", "a", temperature = 0),
    b = LLMR::llm_config("groq", "b", temperature = 0)))
  runner <- function(experiments, ...) {
    experiments$response_text <- "yes"
    experiments$success <- !(experiments$cell == 2L & experiments$unit_id == 3L)
    experiments$error_message <- ifelse(experiments$success, NA, "timeout")
    experiments
  }
  e <- catch_run_failure(audit_run(plan, .runner = runner))
  expect_s3_class(e, "llmrcontent_run_failure")
  expect_match(conditionMessage(e), "reported 1 unsuccessful row")
  expect_named(e$failed, c("cell", "unit_id", "error_message"))
  expect_identical(as.integer(e$failed$cell), 2L)
  expect_identical(as.integer(e$failed$unit_id), 3L)
  expect_identical(e$failed$error_message, "timeout")
  expect_identical(nrow(e$completed), 5L)
  expect_identical(e$completed$response_text, rep("yes", 5L))

  missing <- function(experiments, ...) {
    experiments$response_text <- ifelse(experiments$unit_id == 1L, NA, "no")
    experiments
  }
  e <- catch_run_failure(audit_run(plan, .runner = missing))
  expect_match(conditionMessage(e), "missing response for 2 row")
  expect_named(e$failed, c("cell", "unit_id"))
  expect_identical(nrow(e$completed), 4L)
})

test_that("completed rows keep list and date-time columns across passes", {
  p <- protocol_lock(protocol(fix_codebook(), fix_config(), replicates = 2L))
  corpus <- data.frame(text = c("great news today", "terrible outcome"))
  stamp <- as.POSIXct("2026-10-18 09:30:00", tz = "UTC")
  calls <- 0L
  runner <- function(experiments, ...) {
    calls <<- calls + 1L
    out <- fake_runner_perfect(experiments, ...)
    if (calls == 1L) {
      out$sent_tokens <- NULL
      out$rec_tokens <- NULL
    } else {
      out$finished <- rep(stamp, nrow(out))
      out$request_id <- I(lapply(seq_len(nrow(out)), function(i)
        c(paste0("req", i), "retry")))
      out$success[2] <- FALSE
    }
    out
  }
  e <- catch_run_failure(code_corpus(corpus, p, "text", .runner = runner))
  done <- e$completed
  expect_identical(nrow(done), 3L)
  expect_s3_class(done$finished, "POSIXct")
  expect_identical(attr(done$finished, "tzone"), "UTC")
  expect_identical(is.na(done$finished), c(TRUE, TRUE, FALSE))
  expect_true(is.list(done$request_id))
  expect_null(done$request_id[[1]])
  expect_identical(done$request_id[[3]], c("req1", "retry"))
  expect_type(done$sent_tokens, "integer")
  expect_identical(done$sent_tokens, c(NA, NA, 10L))
})

test_that("call batches bind with typed missing values in either order", {
  a <- tibble::tibble(
    x = 1:2,
    when = as.POSIXct(c("2026-01-01 10:00:00", "2026-01-02 11:00:00"),
                      tz = "UTC"),
    ids = list(c("a", "b"), "c"),
    kind = factor(c("u", "v")))
  b <- data.frame(x = 3L)
  out <- .bind_call_rows(list(b, a))
  expect_s3_class(out$when, "POSIXct")
  expect_identical(format(out$when[2], tz = "UTC"), "2026-01-01 10:00:00")
  expect_true(is.na(out$when[1]))
  expect_true(is.list(out$ids))
  expect_null(out$ids[[1]])
  expect_s3_class(out$kind, "factor")
  expect_identical(as.character(out$kind), c(NA, "u", "v"))
  out <- .bind_call_rows(list(a, b))
  expect_s3_class(out$when, "POSIXct")
  expect_identical(out$x, 1:3)
})
