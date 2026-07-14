# The held-out split name is stored on the gold set at creation and followed
# everywhere it used to be hard-wired as "test": the seal and size guards,
# tune_protocol()'s refusal, validate_protocol()'s default and ledger,
# gold_correct()'s audit and ledger, and the report heading. A gold set whose
# holdout is named "holdout" must be sealed, ledgered, validated, and
# corrected exactly like the default.

test_that("gold_set stores and guards the holdout name", {
  df <- data.frame(text = paste("unit", 1:40),
                   label = rep(c("x", "y"), each = 20))

  expect_error(gold_set(df, "text", "label", holdout = ""), "holdout")
  expect_error(gold_set(df, "text", "label", holdout = c("a", "b")), "holdout")

  # a non-"test" holdout that names a real split raises no vacuous-seal noise
  set.seed(110)
  expect_no_warning(
    g <- gold_set(df, "text", "label",
                  split = c(dev = 0.5, holdout = 0.5), holdout = "holdout"))
  expect_identical(g$holdout, "holdout")
  expect_true(g$sealed)
  expect_output(print(g), "holdout 'holdout' SEALED")

  # the vacuous-seal warning names the declared holdout, not "test"
  expect_warning(
    gold_set(df, "text", "label",
             split = c(dev = 0.5, eval = 0.5), holdout = "screening"),
    "no split is named 'screening'")

  # the small-split warning follows the holdout name too
  expect_warning(
    gold_set(df[1:10, ], "text", "label",
             split = c(dev = 0.5, holdout = 0.5), holdout = "holdout"),
    "holdout split \\('holdout'\\)")

  # objects saved before the field existed fall back to "test"
  g$holdout <- NULL
  expect_identical(LLMRcontent:::.gold_holdout(g), "test")
})

test_that("tuning refuses the named holdout; validation defaults to it", {
  df <- data.frame(text = c(paste("great news", 1:20), paste("awful news", 1:20)),
                   label = rep(c("positive", "negative"), each = 20))
  set.seed(110)
  g <- gold_set(df, "text", "label",
                split = c(dev = 0.5, holdout = 0.5), holdout = "holdout")
  p <- protocol(fix_codebook(), fix_config(), label = "p")

  expect_error(tune_protocol(list(p), g, split = "holdout"),
               "holdout split \\('holdout'\\) is sealed")
  expect_s3_class(tune_protocol(list(p), g, split = "dev",
                                .runner = fake_runner_perfect),
                  "protocol_tuning")

  # the unlocked refusal names the actual holdout
  expect_error(validate_protocol(p, g, .runner = fake_runner_perfect),
               "unlocked protocol on the holdout split \\('holdout'\\)")

  pl <- protocol_lock(p)
  v <- validate_protocol(pl, g, .runner = fake_runner_perfect)
  expect_identical(v$split, "holdout")
  expect_identical(v$holdout, "holdout")
  expect_equal(v$n, 20L)
  led <- gold_ledger(g)
  expect_equal(nrow(led), 1L)
  expect_identical(led$split, "holdout")
  expect_output(print(v), "holdout-split evaluations ledgered so far: 1")

  # a dev evaluation still stays off the ledger
  invisible(validate_protocol(pl, g, split = "dev",
                              .runner = fake_runner_perfect))
  expect_equal(nrow(gold_ledger(g)), 1L)

  # and the report heads the ledger with the holdout's name
  rep <- coding_report(v, g, pl)
  expect_match(paste(unclass(rep), collapse = "\n"), "HOLDOUT-SPLIT LEDGER")
})

test_that("gold_correct audits and ledgers the named holdout end to end", {
  cb <- codebook("stance", "one text",
                 list(cb_category("A", "Category A."),
                      cb_category("B", "Category B.")))
  pl <- protocol_lock(protocol(cb, LLMR::llm_config("groq", "any-model"),
                               label = "holdout-demo"))
  truth <- rep(c("A", "B"), each = 30)
  flip <- seq_along(truth) %in% 1:6
  texts <- sprintf("unit%02d truth=%s%s", seq_along(truth), truth,
                   ifelse(flip, " flip", ""))
  corpus <- data.frame(text = texts)
  gold_rows <- c(1:20, 31:50)
  gold <- gold_set(data.frame(text = texts[gold_rows],
                              label = truth[gold_rows]),
                   text = "text", labels = "label",
                   split = c(holdout = 1), holdout = "holdout")
  fake <- function(experiments, ...) {
    is_a <- grepl("truth=A", experiments$text, fixed = TRUE)
    experiments$response_text <-
      ifelse(is_a & grepl("flip", experiments$text), "B",
             ifelse(is_a, "A", "B"))
    experiments
  }

  validation <- validate_protocol(pl, gold, .runner = fake)
  expect_identical(validation$split, "holdout")
  expect_equal(validation$accuracy, 34 / 40)
  expect_equal(nrow(gold_ledger(gold)), 1L)

  coded <- code_corpus(corpus, pl, "text", .runner = fake)

  res <- gold_correct(coded, gold)
  expect_identical(res$holdout, "holdout")
  expect_true(res$sealed)
  expect_equal(res$n_audit, 40L)
  expect_equal(res$accuracy_audit, 34 / 40)
  a <- res$table[res$table$category == "A", ]
  expect_equal(a$share_naive, 24 / 60)
  expect_equal(a$share_corrected, 24 / 60 + 6 / 40)
  expect_output(print(res), "holdout split \\('holdout'\\)")

  led <- gold_ledger(gold)
  expect_equal(nrow(led), 2L)
  expect_identical(led$split, rep("holdout", 2L))
  expect_equal(led$accuracy, rep(34 / 40, 2L))
})
