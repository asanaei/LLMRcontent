# Token usage a runner does not report is unknown, not zero: `tokens` sums the
# calls that reported usage, is NA when none did, and the number of calls
# without usage travels with it.

usage_runner <- function(sent, rec, total = NULL) {
  function(experiments, ...) {
    out <- fake_runner_perfect(experiments)
    n <- nrow(out)
    out$sent_tokens <- rep_len(sent, n)
    out$rec_tokens <- rep_len(rec, n)
    if (!is.null(total)) out$total_tokens <- rep_len(total, n)
    out
  }
}

test_that("tuning sums reported usage and counts the calls without it", {
  g <- fix_gold(8)
  p <- protocol(fix_codebook(), fix_config())
  full <- tune_protocol(p, g, .runner = usage_runner(10L, 2L))
  expect_equal(full$table$tokens, 48L)
  expect_equal(full$usage$tokens_unknown, 0L)
  half <- tune_protocol(p, g, .runner = usage_runner(c(10L, NA), c(2L, NA)))
  expect_equal(half$table$tokens, 24L)
  expect_equal(half$usage$calls, 4L)
  expect_equal(half$usage$tokens_unknown, 2L)
  expect_output(print(half), "token usage unknown for 2 of 4 call(s)",
                fixed = TRUE)
  none <- tune_protocol(p, g, .runner = usage_runner(NA_integer_, NA_integer_))
  expect_true(is.na(none$table$tokens))
  expect_output(print(none), "unknown for all 4 call(s)", fixed = TRUE)
})

test_that("a total reported by the provider is used as the call's total", {
  g <- fix_gold(8)
  p <- protocol(fix_codebook(), fix_config())
  thinking <- tune_protocol(p, g, .runner = usage_runner(10L, 2L, 20L))
  expect_equal(thinking$table$tokens, 80L)
})

test_that("validation counts unknown usage across replicates", {
  g <- fix_gold(8)
  p <- protocol_lock(protocol(fix_codebook(), fix_config(), replicates = 3L))
  v <- validate_protocol(p, g, .runner = usage_runner(c(10L, NA, 10L),
                                                      c(2L, 2L, NA)))
  expect_equal(v$tokens, 72L)
  expect_equal(v$calls, 12L)
  expect_equal(v$tokens_unknown, 6L)
  expect_output(print(v), "6 of 12 call(s)", fixed = TRUE)
  silent <- validate_protocol(p, g, .runner = fake_runner_constant("positive"))
  expect_true(is.na(silent$tokens))
  expect_equal(silent$tokens_unknown, 12L)
})

test_that("non-numeric token columns are not read as counts", {
  g <- fix_gold(8)
  p <- protocol(fix_codebook(), fix_config())
  factor_runner <- function(experiments, ...) {
    out <- fake_runner_perfect(experiments)
    out$sent_tokens <- factor(out$sent_tokens)
    out
  }
  expect_warning(tuned <- tune_protocol(p, g, .runner = factor_runner),
                 "not numeric")
  expect_true(is.na(tuned$table$tokens))
})

test_that("audit cells count the calls without usage", {
  plan <- audit_plan(
    data.frame(text = c("cut taxes", "expand care", "free trade")), "text",
    function(d) mean(d$label == "yes"), c("yes", "no"), "{text}")
  plan <- audit_add_models(plan, list(m = fix_config()))
  plan <- audit_add_perturbations(plan, temperature = c(0, 0.7))
  runner <- function(x, ...) {
    x$response_text <- "yes"
    x$sent_tokens <- rep_len(c(5L, NA, 5L), nrow(x))
    x$rec_tokens <- 1L
    x
  }
  a <- audit_run(plan, .runner = runner)
  expect_equal(a$cells$tokens, c(12L, 12L))
  expect_equal(a$cells$tokens_unknown, c(1L, 1L))
  expect_output(print(a), "2 of 6 call(s)", fixed = TRUE)
})

test_that("the archive appendix does not read null usage as zero", {
  record <- function(i, usage) {
    paste0('{"ts":"2026-06-01T10:00:0', i, '+0000","schema_version":"1.0",',
           '"kind":"call","provider":"groq","model":"openai/gpt-oss-20b",',
           '"request":{"messages":[{"role":"user","content":"hi ', i, '"}],',
           '"temperature":0},"usage":', usage, ',"response_id":"r-', i,
           '","text":"reply"}')
  }
  failure <- paste0('{"ts":"2026-06-01T10:00:09+0000","schema_version":"1.0",',
                    '"kind":"error","provider":"groq",',
                    '"model":"openai/gpt-oss-20b","message":"boom"}')
  tokens_line <- function(lines) {
    log <- tempfile(fileext = ".jsonl")
    writeLines(lines, log)
    a <- archive_seal(archive_build(log, name = "usage"))
    grep("^TOKENS", unclass(LLMR::report(a)), value = TRUE)
  }
  expect_identical(
    tokens_line(c(record(1, '{"sent":5,"rec":2}'), record(2, '{"sent":7,"rec":3}'),
                  failure)),
    paste("TOKENS. 12 sent, 5 received (as reported by providers);",
          "usage incomplete for 1 of 3 call(s)."))
  expect_identical(
    tokens_line(c(record(1, '{"sent":5,"rec":2}'),
                  record(2, '{"sent":null,"rec":null}'), failure)),
    paste("TOKENS. 5 sent, 2 received (as reported by providers);",
          "usage incomplete for 2 of 3 call(s)."))
  expect_identical(
    tokens_line(c(record(1, '{"sent":null,"rec":null}'),
                  record(2, '{"sent":null,"rec":null}'))),
    "TOKENS. Usage unknown for all 2 call(s).")
  expect_identical(tokens_line(record(1, '{"sent":0,"rec":0}')),
                   "TOKENS. 0 sent, 0 received (as reported by providers).")
})

test_that("the Studio accumulates and renders component usage", {
  skip_if_not_installed("LLMR.shiny", "0.1.3")
  state <- LLMR.shiny::usage_empty()
  live <- list(mode = function() "live", add_usage = function(tokens) {
    state <<- LLMR.shiny::usage_add(state, tokens)
  })
  calls <- data.frame(response_id = c("r1", "r2"), sent_tokens = c(0, 0),
                      rec_tokens = c(5, NA))
  add_call_usage(live, calls, 2L)
  expect_identical(state[c("sent", "received", "total")],
                   list(sent = 0, received = 5, total = 5))
  expect_identical(state$calls, 2L)
  expect_identical(state$unknown, 1L)
  expect_match(paste(as.character(LLMR.shiny::usage_tile(state)), collapse = " "),
               "Sent 0 / Received at least 5 tokens", fixed = TRUE)
  demo <- live
  demo$mode <- function() "demo"
  add_call_usage(demo, LLMR.shiny::annotate_demo_result(calls), 2L)
  expect_identical(state$result_rows, 2L)
  expect_identical(state$calls, 2L)
  expect_identical(state[c("sent", "received", "total")],
                   list(sent = 0, received = 5, total = 5))
})

test_that("reported totals survive missing components and changing replicate reporting", {
  g <- fix_gold(8)
  p <- protocol_lock(protocol(fix_codebook(), fix_config(), replicates = 3L))
  reported <- validate_protocol(p, g, .runner = usage_runner(NA, NA, 20))
  expect_identical(reported$tokens, 240)
  expect_identical(reported$tokens_unknown, 0L)
  pass <- 0L
  runner <- function(experiments, ...) {
    pass <<- pass + 1L
    out <- fake_runner_perfect(experiments)
    missing <- (seq_len(nrow(out)) + pass) %% 2L == 0L
    out$sent_tokens[missing] <- NA
    out$rec_tokens[missing] <- NA
    out
  }
  varied <- validate_protocol(p, g, .runner = runner)
  expect_identical(varied$tokens, 72)
  expect_identical(varied$calls, 12L)
  expect_identical(varied$tokens_unknown, 6L)
})

test_that("content token summaries retain large values and reject invalid counts", {
  p <- protocol(fix_codebook(), fix_config())
  g <- fix_gold(8)
  out <- tune_protocol(p, g, .runner = usage_runner(1e9, 1))
  expect_identical(out$table$tokens, 4000000004)
  expect_identical(out$usage$calls, 4L)
  expect_identical(out$usage$tokens_unknown, 0L)
  expect_warning(out <- tune_protocol(p, g, .runner = usage_runner(c(-1, 1.5, Inf, NaN), 2)),
                 "sent_tokens.*invalid counts")
  expect_identical(out$table$tokens, NA_real_)
  expect_identical(out$usage$tokens_unknown, 4L)
  zero <- tune_protocol(p, g, .runner = usage_runner(c(0, NA), c(0, NA)))
  expect_identical(zero$table$tokens, 0)
  expect_identical(zero$usage$tokens_unknown, 2L)
  expect_identical(.token_sum(.call_tokens(data.frame())),
                   list(tokens = 0, calls = 0L, unknown = 0L))
})

packed_runner <- function(experiments, ...) {
  out <- fake_runner_perfect(experiments)
  out$response_id <- paste0("r", ceiling(seq_len(nrow(out)) / 2))
  out$rowpack_id <- out$response_id
  out$rowpack_row <- rep_len(1:2, nrow(out))
  out$sent_tokens[out$rowpack_row > 1L] <- NA
  out$rec_tokens[out$rowpack_row > 1L] <- NA
  out
}

test_that("packing preserves call provenance through unit aggregation", {
  p <- protocol(fix_codebook(), fix_config())
  out <- tune_protocol(p, fix_gold(8), .runner = packed_runner)
  expect_identical(out$table$tokens, 24)
  expect_identical(out$usage$calls, 2L)
  expect_identical(out$usage$tokens_unknown, 0L)
  follow <- data.frame(response_id = "r", rowpack_id = "p", rowpack_row = 2L)
  expect_identical(.token_sum(.call_tokens(follow)),
                   list(tokens = NA_real_, calls = 1L, unknown = 1L))
  incomplete <- data.frame(response_id = c(NA, NA), success = c(FALSE, FALSE))
  expect_identical(.token_sum(.call_tokens(incomplete)),
                   list(tokens = NA_real_, calls = 2L, unknown = 2L))
  incomplete$response_id <- I(list(c("a", NA), "b"))
  incomplete$success <- TRUE
  expect_no_warning(out <- .token_sum(.call_tokens(incomplete)))
  expect_identical(out$calls, 2L)
  incomplete$response_id <- I(list(list("a", NULL), "b"))
  expect_no_warning(out <- .token_sum(.call_tokens(incomplete)))
  expect_identical(out$calls, 2L)
  incomplete$call_id <- NA_character_
  incomplete$response_id <- c("shared", "shared")
  incomplete$success <- FALSE
  expect_identical(.token_sum(.call_tokens(incomplete))$calls, 1L)
  plan <- audit_plan(data.frame(text = c("one", "two")), "text",
                     function(d) mean(d$label == "positive"),
                     c("positive", "negative"), "{text}")
  plan <- audit_add_models(plan, list(m = fix_config()))
  audit <- audit_run(plan, .runner = packed_runner)
  expect_identical(audit$cells$tokens, 12)
  expect_identical(audit$cells$tokens_unknown, 0L)
  no_first <- function(experiments, ...) {
    out <- packed_runner(experiments)
    out$rowpack_row <- 2L
    out
  }
  audit <- audit_run(plan, .runner = no_first)
  expect_identical(audit$cells$tokens, 12)
  expect_identical(audit$cells$tokens_unknown, 1L)
})

test_that("appendices retain each reported component across all record kinds", {
  line <- function(usages, kinds = rep("call", length(usages))) {
    log <- tempfile(fileext = ".jsonl")
    records <- vapply(seq_along(usages), function(i) {
      jsonlite::toJSON(list(schema_version = "1.0", kind = kinds[i],
                            provider = "groq", model = "fake-model",
                            response_id = paste0("r", i), usage = usages[[i]]),
                       auto_unbox = TRUE, null = "null", na = "null")
    }, character(1))
    writeLines(records, log)
    grep("^TOKENS", unclass(LLMR::report(archive_build(log))), value = TRUE)
  }
  expect_identical(line(list(list(sent = 5), list(sent = 7))),
                   paste("TOKENS. 12 sent, unknown received (as reported by providers);",
                         "usage incomplete for 2 of 2 call(s)."))
  expect_identical(line(list(list(rec = 0), list(rec = 3))),
                   paste("TOKENS. unknown sent, 3 received (as reported by providers);",
                         "usage incomplete for 2 of 2 call(s)."))
  expect_identical(line(list(list(sent = 5, rec = 2), NULL), c("error", "error")),
                   paste("TOKENS. 5 sent, 2 received (as reported by providers);",
                         "usage incomplete for 1 of 2 call(s)."))
  expect_identical(line(list(NULL, NULL), c("error", "other")),
                   "TOKENS. Usage unknown for all 2 call(s).")
  expect_warning(out <- line(list(list(sent = -1, rec = 2))), "usage$sent", fixed = TRUE)
  expect_identical(out, paste("TOKENS. unknown sent, 2 received (as reported by providers);",
                              "usage incomplete for 1 of 1 call(s)."))
})

test_that("the launcher rejects an older substrate with version and install guidance", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")
  skip_if_not_installed("LLMR.shiny")
  installed <- "0.1.2"
  local_mocked_bindings(packageVersion = function(...) package_version(installed),
                        .package = "utils")
  expect_error(.content_gui_require(),
               "LLMR.shiny >= 0.1.3; installed version is 0.1.2. Install it with install.packages(\"LLMR.shiny\")",
               fixed = TRUE)
  installed <- "0.1.3"
  expect_no_error(.content_gui_require())
})

test_that("accounting failures never raise or record a fallback", {
  skip_if_not_installed("shiny")
  recorded <- 0L
  shared <- list(mode = function() "live", add_usage = function(tokens) {
    recorded <<- recorded + 1L
  })
  local_mocked_bindings(extract_token_counts = function(...) stop("extraction failed"))
  expect_message(expect_no_error(add_call_usage(shared, NULL, 10L)), "extraction failed")
  expect_identical(recorded, 0L)
  local_mocked_bindings(extract_token_counts = function(...) warning("incomplete identifier"))
  withr::local_options(warn = 2)
  expect_message(expect_no_error(add_call_usage(shared, NULL, 10L)), "incomplete identifier")
  expect_identical(recorded, 0L)
  local_mocked_bindings(extract_token_counts = function(...) list(calls = 2L))
  shared$add_usage <- function(...) stop("accumulation failed")
  expect_message(expect_no_error(add_call_usage(shared, NULL, 10L)), "accumulation failed")
  expect_identical(recorded, 0L)
})

studio_usage_fixture <- function() {
  state <- shiny::reactiveVal(LLMR.shiny::usage_empty())
  list(state = state, shared = list(
    mode = shiny::reactive("live"), provider = shiny::reactive("groq"),
    model = shiny::reactive("fake-model"), temperature = shiny::reactive(0),
    max_tokens = shiny::reactive(128L), reasoning_effort = shiny::reactive("low"),
    can_run = shiny::reactive(TRUE), key = shiny::reactive(list()),
    set_plan = function(...) NULL,
    add_usage = function(tokens) state(LLMR.shiny::usage_add(state(), tokens))
  ))
}

test_that("failed tuning retains returned usage and excludes unattempted replicates", {
  skip_if_not_installed("LLMR.shiny", "0.1.3")
  skip_if_not_installed("DT")
  fixture <- studio_usage_fixture()
  local_mocked_bindings(build_runner = function(...) function(experiments, ...) {
    out <- fake_runner_perfect(experiments)
    out$response_id <- paste0("r", seq_len(nrow(out)))
    out$success[nrow(out)] <- FALSE
    out$sent_tokens[nrow(out)] <- out$rec_tokens[nrow(out)] <- NA
    out
  })
  test_gold <- fix_gold(8)
  test_protocol <- protocol(fix_codebook(), fix_config(), replicates = 2L)
  shiny::testServer(mod_coder_server,
    args = list(shared = fixture$shared, active = shiny::reactive("home")), {
      session$flushReact()
      gold(test_gold)
      protocols(list(test_protocol))
      run_tune(confirmed = TRUE)
      expect_null(tuning())
      expect_false(is.null(run_error()))
      expect_identical(fixture$state()$calls, 4L)
      expect_identical(fixture$state()[c("sent", "received", "total")],
                       list(sent = 30, received = 6, total = 36))
      expect_identical(fixture$state()$unknown, 1L)
    })
})

test_that("an interrupted audit accounts for requests before the interruption", {
  skip_if_not_installed("LLMR.shiny", "0.1.3")
  fixture <- studio_usage_fixture()
  attempt <- 0L
  local_mocked_bindings(build_runner = function(...) function(experiments, ...) {
    attempt <<- attempt + 1L
    if (attempt == 2L) stop("runner interrupted")
    out <- fake_runner_perfect(experiments)
    out$response_id <- paste0("r", attempt)
    out
  })
  shiny::testServer(mod_valid_server,
    args = list(shared = fixture$shared, active = shiny::reactive("home")), {
      session$setInputs(text_col = "text", labels = "positive, negative",
                        estimand = "share", target = "positive", prompt = "{text}",
                        orders = "as_given", temps = "0", add_paraphrase = FALSE)
      data_raw(data.frame(text = c("great", "bad", "fine")))
      run_audit(confirmed = TRUE)
      expect_null(audit())
      expect_false(is.null(run_error()))
      expect_identical(fixture$state()$calls, 1L)
      expect_identical(fixture$state()[c("sent", "received", "total")],
                       list(sent = 10, received = 2, total = 12))
    })
})

test_that("a failure before any batch records no planned usage", {
  skip_if_not_installed("LLMR.shiny", "0.1.3")
  skip_if_not_installed("DT")
  fixture <- studio_usage_fixture()
  local_mocked_bindings(build_runner = function(...) function(...) stop("before first batch"))
  test_gold <- fix_gold(8)
  test_protocol <- protocol(fix_codebook(), fix_config())
  shiny::testServer(mod_coder_server,
    args = list(shared = fixture$shared, active = shiny::reactive("home")), {
      session$flushReact()
      gold(test_gold)
      protocols(list(test_protocol))
      run_tune(confirmed = TRUE)
      expect_identical(fixture$state(), LLMR.shiny::usage_empty())
      expect_false(is.null(run_error()))
    })
})

test_that("replicate batches can omit token columns when usage is unavailable", {
  pass <- 0L
  runner <- function(experiments, ...) {
    pass <<- pass + 1L
    out <- fake_runner_perfect(experiments)
    if (pass == 2L) out[c("sent_tokens", "rec_tokens")] <- NULL
    out
  }
  p <- protocol(fix_codebook(), fix_config(), replicates = 3L)
  out <- tune_protocol(p, fix_gold(8), .runner = runner)
  expect_identical(out$table$tokens, 96)
  expect_identical(out$usage$calls, 12L)
  expect_identical(out$usage$tokens_unknown, 4L)
  batches <- list(data.frame(sent_tokens = 10, rec_tokens = 2), data.frame(success = FALSE))
  combined <- bind_call_batches(batches)
  expect_equal(nrow(combined), 2L)
  expect_identical(combined$sent_tokens, c(10, NA_real_))
})

test_that("an accounting error preserves a successful Studio result", {
  skip_if_not_installed("LLMR.shiny", "0.1.3")
  skip_if_not_installed("DT")
  fixture <- studio_usage_fixture()
  fixture$shared$add_usage <- function(...) stop("accounting unavailable")
  local_mocked_bindings(build_runner = function(...) fake_runner_perfect)
  test_gold <- fix_gold(8)
  test_protocol <- protocol(fix_codebook(), fix_config())
  shiny::testServer(mod_coder_server,
    args = list(shared = fixture$shared, active = shiny::reactive("home")), {
      session$flushReact()
      gold(test_gold)
      protocols(list(test_protocol))
      expect_message(expect_no_error(run_tune(confirmed = TRUE)), "accounting unavailable")
      expect_s3_class(tuning(), "protocol_tuning")
      expect_identical(tuning()$table$tokens, 48)
      expect_null(run_error())
      expect_identical(fixture$state(), LLMR.shiny::usage_empty())
    })
})

test_that("validation and coding failures account for their returned batches", {
  skip_if_not_installed("LLMR.shiny", "0.1.3")
  skip_if_not_installed("DT")
  fixture <- studio_usage_fixture()
  local_mocked_bindings(build_runner = function(...) function(experiments, ...) {
    out <- fake_runner_perfect(experiments)
    out$response_id <- paste0("r", seq_len(nrow(out)))
    out$success[nrow(out)] <- FALSE
    out
  })
  test_gold <- fix_gold(8)
  test_protocol <- protocol_lock(protocol(fix_codebook(), fix_config()))
  saved_validation <- validate_protocol(test_protocol, test_gold, .runner = fake_runner_perfect)
  shiny::testServer(mod_coder_server,
    args = list(shared = fixture$shared, active = shiny::reactive("home")), {
      session$setInputs(confirm_ledger = TRUE, corpus_text_col = "text")
      gold(test_gold)
      locked_protocol(test_protocol)
      run_validate(confirmed = TRUE)
      expect_null(validation())
      expect_false(is.null(run_error()))
      expect_identical(fixture$state()$calls, 4L)
      expect_identical(fixture$state()[c("sent", "received", "total")],
                       list(sent = 40, received = 8, total = 48))
      validation(saved_validation)
      corpus_raw(data.frame(text = c("great", "bad")))
      run_code_corpus(confirmed = TRUE)
      expect_null(coded())
      expect_false(is.null(run_error()))
      expect_identical(fixture$state()$calls, 6L)
      expect_identical(fixture$state()[c("sent", "received", "total")],
                       list(sent = 60, received = 12, total = 72))
    })
})
