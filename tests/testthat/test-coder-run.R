test_that("tuning ranks protocols and refuses the test split", {
  g <- fix_gold(8)
  good <- protocol(fix_codebook(), fix_config(), label = "good")
  bad  <- protocol(fix_codebook(), fix_config("bad-model"), label = "always-pos")

  res <- tune_protocol(
    list(good, bad), g, split = "dev",
    .runner = function(experiments, ...) {
      # protocol 1 codes by keyword; protocol 2 answers a constant
      experiments$response_text <- vapply(seq_len(nrow(experiments)), function(i) {
        if (experiments$protocol_id[i] == 1L) {
          if (grepl("great|lovely|wonderful|fine",
                    experiments$messages[[i]][["user"]])) "positive" else "negative"
        } else "positive"
      }, character(1))
      experiments$success <- TRUE
      experiments
    })

  expect_s3_class(res, "protocol_tuning")
  expect_named(res, c("table", "per_category", "split"))
  expect_identical(res$table$protocol[1], "good")
  expect_equal(res$table$accuracy[res$table$protocol == "good"], 1)
  expect_lt(res$table$accuracy[res$table$protocol == "always-pos"], 1)
  expect_true(all(c("acc_lo", "acc_hi", "macro_f1") %in%
                    names(res$table)))
  expect_named(res$per_category, c("good", "always-pos"))
  expect_identical(res$split, "dev")

  expect_error(tune_protocol(list(good), g, split = "test"), "sealed")
})

test_that("validation requires a lock on test and writes the ledger", {
  g <- fix_gold(8)
  p <- protocol(fix_codebook(), fix_config(), label = "p")

  expect_error(validate_protocol(p, g, split = "test",
                                 .runner = fake_runner_perfect),
               "unlocked")
  # dev evaluation is allowed without a lock and leaves no ledger entry
  v_dev <- validate_protocol(p, g, split = "dev",
                             .runner = fake_runner_perfect)
  expect_s3_class(v_dev, "protocol_validation")
  expect_equal(nrow(gold_ledger(g)), 0L)

  pl <- protocol_lock(p)
  v1 <- validate_protocol(pl, g, .runner = fake_runner_perfect)
  v2 <- validate_protocol(pl, g, .runner = fake_runner_perfect)
  expect_s3_class(v1, "protocol_validation")
  expect_equal(v1$accuracy, 1)
  expect_equal(v1$tokens, 4L * 12L)
  led <- gold_ledger(g)
  expect_equal(nrow(led), 2L)
  expect_identical(unique(led$protocol_hash), pl$hash)
  expect_output(print(v2), "ledgered so far: 2")
})

test_that("locked protocols refuse changes at validation and coding gates", {
  g <- fix_gold(8)
  pl <- protocol_lock(protocol(fix_codebook(), fix_config(), label = "fixed"))
  corpus <- data.frame(text = "great work")

  expect_s3_class(validate_protocol(pl, g, .runner = fake_runner_perfect),
                  "protocol_validation")
  expect_s3_class(code_corpus(corpus, pl, "text",
                              .runner = fake_runner_perfect),
                  "coded_corpus")

  changed <- pl
  changed$prompt <- paste(changed$prompt, "Changed after locking.")
  expect_error(validate_protocol(changed, g, .runner = fake_runner_perfect),
               "changed since protocol_lock")
  expect_error(code_corpus(corpus, changed, "text",
                           .runner = fake_runner_perfect),
               "changed since protocol_lock")
})

test_that("validation applies the modal rule across protocol replicates", {
  g <- fix_gold(8)
  pl <- protocol_lock(protocol(fix_codebook(), fix_config(),
                               replicates = 3L))
  batch <- 0L
  calls <- 0L
  scripted <- function(experiments, ...) {
    batch <<- batch + 1L
    calls <<- calls + nrow(experiments)
    positive <- grepl("great|lovely|wonderful|fine", experiments$text)
    truth <- ifelse(positive, "positive", "negative")
    experiments$response_text <- if (batch == 1L) {
      ifelse(truth == "positive", "negative", "positive")
    } else {
      truth
    }
    experiments$sent_tokens <- 10L
    experiments$rec_tokens <- 2L
    experiments
  }

  v <- validate_protocol(pl, g, .runner = scripted)
  expect_equal(v$accuracy, 1)
  expect_equal(batch, 3L)
  expect_equal(calls, 3L * v$n)
  expect_equal(v$tokens, calls * 12L)
})

test_that("seal_holdout = FALSE means no ledger entries", {
  g <- fix_gold(8, seal_holdout = FALSE)
  expect_output(print(g), "unsealed")
  pl <- protocol_lock(protocol(fix_codebook(), fix_config()))
  invisible(validate_protocol(pl, g, .runner = fake_runner_perfect))
  expect_equal(nrow(gold_ledger(g)), 0L)
})

test_that("macro-F1 scores a never-predicted category as zero", {
  g <- fix_gold(8)
  pl <- protocol_lock(protocol(fix_codebook(), fix_config()))
  v <- validate_protocol(pl, g, split = "dev",
                         .runner = fake_runner_constant("positive"))
  # accuracy is the positive share; macro-F1 must be dragged down by the
  # zero-scored 'negative' category, not flattered by dropping it
  expect_equal(v$accuracy, 0.5)
  f1_pos <- v$per_category$f1[v$per_category$label == "positive"]
  expect_equal(v$macro_f1, mean(c(f1_pos, 0)))
  expect_lt(v$macro_f1, v$accuracy)
})

test_that("parse failures are counted, never silently dropped", {
  g <- fix_gold(8)
  pl <- protocol_lock(protocol(fix_codebook(), fix_config()))
  v <- validate_protocol(pl, g, split = "dev",
                         .runner = fake_runner_flaky(every = 2L))
  expect_gt(v$parse_failures, 0L)
  # flaky runner answers with case/space noise; parser still normalizes
  expect_gt(v$accuracy, 0)
})

test_that("code_corpus needs a lock, replicates, and reports stability", {
  corp <- data.frame(uid = 1:3,
                     text = c("great stuff", "awful stuff", "fine then"))
  p <- protocol(fix_codebook(), fix_config(), replicates = 3L)
  expect_error(code_corpus(corp, p, "text"), "locked")

  pl <- protocol_lock(p)
  out <- code_corpus(corp, pl, "text", id = "uid",
                     .runner = fake_runner_perfect)
  expect_s3_class(out, "coded_corpus")
  expect_named(out, c("data", "protocol_hash", "protocol_label", "text",
                      "label", "id", "labels"))
  expect_identical(out$protocol_hash, pl$hash)
  expect_identical(out$protocol_label, pl$label)
  expect_identical(out$text, "text")
  expect_identical(out$label, "label")
  expect_identical(out$id, "uid")
  expect_identical(out$labels, c("positive", "negative"))

  tab <- tibble::as_tibble(out)
  expect_identical(tab$label, c("positive", "negative", "positive"))
  expect_equal(tab$label_share, c(1, 1, 1))
  expect_true(all(c("label_rep1", "label_rep2", "label_rep3") %in% names(tab)))
  expect_output(print(out), "<coded_corpus")
  expect_identical(names(formals(code_corpus)),
                   c("corpus", "protocol", "text", "id", ".runner", "..."))
})

test_that("runner failures abort instead of becoming coded labels", {
  g <- fix_gold(8)
  p <- protocol_lock(protocol(fix_codebook(), fix_config()))
  failed_runner <- function(experiments, ...) {
    experiments$response_text <- NA_character_
    experiments$success <- FALSE
    experiments$error_message <- "not in archive"
    experiments
  }

  expect_error(
    validate_protocol(p, g, split = "dev", .runner = failed_runner),
    "reported failure"
  )
  expect_error(
    code_corpus(data.frame(text = "great"), p, "text",
                .runner = failed_runner),
    "reported failure"
  )

  missing_response <- function(experiments, ...) {
    experiments$response_text <- NA_character_
    experiments
  }
  expect_error(
    validate_protocol(p, g, split = "dev", .runner = missing_response),
    "missing response"
  )
})

test_that("coding runners preserve row identity and may return rows reordered", {
  g <- fix_gold(8)
  p <- protocol_lock(protocol(fix_codebook(), fix_config()))
  reversed <- function(experiments, ...) {
    out <- fake_runner_perfect(experiments, ...)
    out[rev(seq_len(nrow(out))), , drop = FALSE]
  }
  v <- validate_protocol(p, g, split = "dev", .runner = reversed)
  expect_equal(v$accuracy, 1)

  duplicated <- function(experiments, ...) {
    out <- fake_runner_perfect(experiments, ...)
    out$unit_id[2] <- out$unit_id[1]
    out
  }
  expect_error(
    validate_protocol(p, g, split = "dev", .runner = duplicated),
    "row identity"
  )
})

test_that("code_corpus has a typed empty state", {
  p <- protocol_lock(protocol(fix_codebook(), fix_config(), replicates = 2L))
  out <- code_corpus(
    data.frame(uid = integer(), text = character()),
    p, "text", id = "uid",
    .runner = function(experiments, ...) stop("runner should not be called")
  )

  expect_s3_class(out, "coded_corpus")
  expect_equal(nrow(out$data), 0L)
  expect_type(out$data$label, "character")
  expect_type(out$data$label_share, "double")
  expect_type(out$data$parse_failures, "integer")
  expect_type(out$data$label_rep1, "character")
  expect_type(out$data$.text_hash, "character")
})

test_that("coder_agreement wraps llm_agreement for gold coders and frames", {
  df <- data.frame(
    text = c("a", "b", "c", "d"),
    label = c("x", "y", "x", "y"),
    coder1 = c("x", "y", "x", "y"),
    coder2 = c("x", "y", "y", "y")
  )
  g <- suppressWarnings(gold_set(df, "text", "label",
                                 coders = c("coder1", "coder2")))
  agr <- coder_agreement(g)
  expect_s3_class(agr, "llmr_agreement")

  agr2 <- coder_agreement(df, cols = c("coder1", "coder2"))
  expect_s3_class(agr2, "llmr_agreement")

  coded <- code_corpus(
    data.frame(text = c("great", "awful")),
    protocol_lock(protocol(fix_codebook(), fix_config(), replicates = 2L)),
    "text", .runner = fake_runner_perfect
  )
  agr3 <- coder_agreement(coded, cols = c("label_rep1", "label_rep2"))
  expect_s3_class(agr3, "llmr_agreement")

  g0 <- suppressWarnings(gold_set(df, "text", "label"))
  expect_error(coder_agreement(g0), "coders")
})

test_that("the report tells the whole story, ledger included", {
  g <- fix_gold(8)
  pl <- protocol_lock(protocol(fix_codebook(), fix_config(), label = "final"))
  v <- validate_protocol(pl, g, .runner = fake_runner_perfect)
  invisible(validate_protocol(pl, g, .runner = fake_runner_perfect))

  rep <- LLMR::report(v, gold = g, protocol = pl)
  expect_s3_class(rep, "coding_report")
  txt <- paste(unclass(rep), collapse = "\n")
  expect_match(txt, "codebook 'tone' v1.0")
  expect_match(txt, "evaluated 2 time")
  expect_output(print(rep), "TEST-SPLIT LEDGER")
})

test_that("tuning results carry the cost column when the runner reports usage", {
  g <- fix_gold(8)
  res <- tune_protocol(protocol(fix_codebook(), fix_config()), g,
                       .runner = fake_runner_perfect)
  expect_equal(res$table$tokens, 4L * 12L)
  # and NA when the runner reports nothing
  res2 <- tune_protocol(protocol(fix_codebook(), fix_config()), g,
                        .runner = function(experiments, ...) {
                          experiments$response_text <- "positive"
                          experiments
                        })
  expect_true(is.na(res2$table$tokens))
})

test_that("duplicate protocol labels stay one-to-one in tuning (regression)", {
  # Tuning prompt variants on one model gives every protocol the same default
  # label; the per_category field and the comparison rows must not
  # overwrite each other.
  g <- fix_gold()
  p1 <- protocol(fix_codebook(), fix_config(),
                 prompt = "{codebook}\nA:\n{text}\nLabel:")
  p2 <- protocol(fix_codebook(), fix_config(),
                 prompt = "{codebook}\nB variant:\n{text}\nLabel:")
  expect_identical(p1$label, p2$label)
  t <- tune_protocol(list(p1, p2), g, .runner = fake_runner_perfect)
  expect_equal(nrow(t$table), 2L)
  expect_equal(anyDuplicated(t$table$protocol), 0L)
  expect_length(t$per_category, 2L)
  expect_setequal(names(t$per_category), t$table$protocol)
})

test_that("every placeholder occurrence is rendered, not just the first (regression)", {
  p <- protocol(fix_codebook(), fix_config(),
                prompt = "{codebook}\nRepeat: {text}\nAgain: {text}\nEnd")
  out <- LLMRcontent:::.render_prompt(p, "UNITTEXT")
  expect_false(grepl("{text}", out, fixed = TRUE))
  expect_false(grepl("{codebook}", out, fixed = TRUE))
  expect_length(gregexpr("UNITTEXT", out, fixed = TRUE)[[1]], 2L)
})

test_that("experiments carry the raw unit text as a metadata column", {
  # An injected runner (the GUI's demo responder, say) must be able to key on
  # the unit itself rather than the full rendered prompt.
  g <- fix_gold()
  seen <- NULL
  spy <- function(experiments, ...) {
    seen <<- experiments$text
    fake_runner_perfect(experiments, ...)
  }
  invisible(tune_protocol(protocol(fix_codebook(), fix_config()), g,
                          .runner = spy))
  expect_false(is.null(seen))
  expect_true(all(seen %in% g$data$text))
})
