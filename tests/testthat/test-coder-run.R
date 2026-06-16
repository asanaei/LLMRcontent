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
  expect_identical(res$protocol[1], "good")
  expect_equal(res$accuracy[res$protocol == "good"], 1)
  expect_lt(res$accuracy[res$protocol == "always-pos"], 1)
  expect_true(all(c("acc_lo", "acc_hi", "macro_f1") %in% names(res)))
  expect_named(attr(res, "per_category"), c("good", "always-pos"))

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

test_that("seal_test = FALSE means no ledger entries", {
  g <- fix_gold(8, seal_test = FALSE)
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
  corp <- data.frame(text = c("great stuff", "awful stuff", "fine then"))
  p <- protocol(fix_codebook(), fix_config(), replicates = 3L)
  expect_error(code_corpus(corp, p, "text"), "locked")

  pl <- protocol_lock(p)
  out <- code_corpus(corp, pl, "text", .runner = fake_runner_perfect)
  expect_identical(out$label, c("positive", "negative", "positive"))
  expect_equal(out$label_share, c(1, 1, 1))
  expect_true(all(c("label_rep1", "label_rep2", "label_rep3") %in% names(out)))
  expect_identical(attr(out, "protocol_hash"), pl$hash)
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
  g0 <- suppressWarnings(gold_set(df, "text", "label"))
  expect_error(coder_agreement(g0), "coders")
})

test_that("the report tells the whole story, ledger included", {
  g <- fix_gold(8)
  pl <- protocol_lock(protocol(fix_codebook(), fix_config(), label = "final"))
  v <- validate_protocol(pl, g, .runner = fake_runner_perfect)
  invisible(validate_protocol(pl, g, .runner = fake_runner_perfect))

  rep <- coding_report(v, g, pl)
  expect_s3_class(rep, "coding_report")
  txt <- paste(unclass(rep), collapse = "\n")
  expect_match(txt, "codebook 'tone' v1.0")
  expect_match(txt, "evaluated 2 time")
  expect_output(print(rep), "TEST-SPLIT LEDGER")
})

test_that("export_caqdas writes csv and jsonl", {
  corp <- data.frame(text = c("great", "awful"))
  pl <- protocol_lock(protocol(fix_codebook(), fix_config()))
  out <- code_corpus(corp, pl, "text", .runner = fake_runner_perfect)

  f1 <- tempfile(fileext = ".csv"); f2 <- tempfile(fileext = ".jsonl")
  on.exit(unlink(c(f1, f2)))
  export_caqdas(out, f1, "csv")
  expect_equal(nrow(utils::read.csv(f1)), 2L)
  export_caqdas(out, f2, "jsonl")
  recs <- lapply(readLines(f2), jsonlite::fromJSON)
  expect_length(recs, 2L)
  expect_identical(recs[[1]]$label, "positive")
})

test_that("tuning results carry the cost column when the runner reports usage", {
  g <- fix_gold(8)
  res <- tune_protocol(protocol(fix_codebook(), fix_config()), g,
                       .runner = fake_runner_perfect)
  expect_equal(res$tokens, 4L * 12L)
  # and NA when the runner reports nothing
  res2 <- tune_protocol(protocol(fix_codebook(), fix_config()), g,
                        .runner = function(experiments, ...) {
                          experiments$response_text <- "positive"
                          experiments
                        })
  expect_true(is.na(res2$tokens))
})
