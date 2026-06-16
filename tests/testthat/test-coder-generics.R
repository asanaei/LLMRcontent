test_that("LLMR generics dispatch on protocol validation objects", {
  g <- fix_gold(8)
  pl <- protocol_lock(protocol(fix_codebook(), fix_config(), label = "final"))

  validation <- validate_protocol(pl, g, .runner = fake_runner_perfect)

  diag <- LLMR::diagnostics(validation)
  expect_s3_class(diag, "tbl_df")
  expect_equal(nrow(diag), 1L)
  expect_named(diag, c(
    "accuracy", "acc_lo", "acc_hi", "macro_f1",
    "parse_failures", "n", "ledger_entries"
  ))
  expect_equal(diag$accuracy, validation$accuracy)
  expect_equal(diag$macro_f1, validation$macro_f1)
  expect_equal(diag$parse_failures, validation$parse_failures)
  expect_equal(diag$n, validation$n)
  expect_equal(diag$ledger_entries, validation$ledger_entries)

  rep <- LLMR::report(validation, gold = g, protocol = pl)
  expect_s3_class(rep, "coding_report")
  expect_true(length(unclass(rep)) > 0L)

  expect_error(LLMR::report(validation), "gold")
  expect_error(LLMR::report(validation, gold = g), "protocol")
})

test_that("LLMR diagnostics and tibble coercion dispatch on gold corrections", {
  g <- fix_gold(8)
  pl <- protocol_lock(protocol(fix_codebook(), fix_config(), label = "final"))

  corpus <- data.frame(text = g$data[[g$text]], stringsAsFactors = FALSE)
  coded <- code_corpus(corpus, pl, "text", .runner = fake_runner_perfect)
  correction <- suppressWarnings(gold_correct(coded, g))

  diag <- LLMR::diagnostics(correction)
  tab <- tibble::as_tibble(correction)

  expect_s3_class(diag, "tbl_df")
  expect_s3_class(tab, "tbl_df")
  expect_named(tab, c(
    "category", "share_naive", "share_corrected", "se", "ci_lo", "ci_hi"
  ))
  expect_equal(diag, tab)
  expect_equal(nrow(tab), length(codebook_labels(pl$codebook)))
  expect_true(all(tab$category %in% codebook_labels(pl$codebook)))
})

test_that("tibble coercion dispatches on protocol tuning results", {
  g <- fix_gold(8)
  p <- protocol(fix_codebook(), fix_config(), label = "candidate")

  tuning <- tune_protocol(p, g, .runner = fake_runner_perfect)
  tab <- tibble::as_tibble(tuning)

  expect_s3_class(tuning, "protocol_tuning")
  expect_s3_class(tab, "tbl_df")
  expect_false(inherits(tab, "protocol_tuning"))
  expect_named(tab, c(
    "protocol", "n", "accuracy", "acc_lo", "acc_hi",
    "macro_f1", "parse_failures", "tokens"
  ))
  expect_equal(tab$protocol, "candidate")
  expect_equal(tab$accuracy, 1)
})
