# gold_correct runs entirely offline: the fake runner errs at a known,
# asymmetric rate, so every corrected number is hand-computable.

gold_correct_fixture <- function(seal_holdout = TRUE, parse_failure = FALSE,
                                 unmatched = FALSE) {
  cb <- codebook("stance", "one text",
    list(cb_category("A", "Category A."),
         cb_category("B", "Category B."),
         cb_category("C", "Category C.")))
  p <- protocol_lock(protocol(cb, LLMR::llm_config("groq", "any-model"),
                              label = "fake-asymmetric"))

  truth <- c(rep("A", 20), rep("B", 20))
  flip <- seq_along(truth) %in% 1:8
  texts <- sprintf("unit%02d truth=%s%s", seq_along(truth), truth,
                   ifelse(flip, " flip", ""))
  if (parse_failure) texts[21] <- paste(texts[21], "parsefail")
  corpus <- data.frame(text = texts)

  gold_rows <- if (parse_failure) c(1:4, 9:14, 21:31) else c(1:4, 9:14, 21:30)
  gold_text <- texts[gold_rows]
  if (unmatched) gold_text <- paste("unmatched", gold_text)

  gold <- gold_set(
    data.frame(text = gold_text, label = truth[gold_rows]),
    text = "text", label = "label", split = c(test = 1),
    seal_holdout = seal_holdout)

  fake <- function(experiments, ...) {
    user <- vapply(experiments$messages, `[[`, "", "user")
    is_a <- grepl("truth=A", user, fixed = TRUE)
    flip <- grepl("flip", user, fixed = TRUE)
    pf <- grepl("parsefail", user, fixed = TRUE)
    experiments$response_text <- ifelse(pf, "not a label",
                                 ifelse(is_a & flip, "B",
                                 ifelse(is_a, "A", "B")))
    experiments
  }
  coded <- code_corpus(corpus, p, "text", .runner = fake)
  list(coded = coded, gold = gold, truth = truth)
}

test_that("gold_correct arithmetic matches the difference estimator", {
  fx <- gold_correct_fixture()
  res <- gold_correct(fx$coded, fx$gold)
  tab <- res$table
  a <- tab[tab$category == "A", ]
  b <- tab[tab$category == "B", ]

  s2 <- stats::var(c(rep(1, 4), rep(0, 16)))
  se <- sqrt((1 - 20 / 40) * s2 / 20)

  expect_equal(a$share_naive, 12 / 40)
  expect_equal(a$share_corrected, 0.5)
  expect_equal(a$se, se)
  expect_equal(b$share_naive, 28 / 40)
  expect_equal(b$share_corrected, 0.5)
  expect_equal(b$se, se)
  expect_equal(res$n_corpus, 40L)
  expect_equal(res$n_audit, 20L)
  expect_equal(res$accuracy_audit, 16 / 20)
  expect_output(print(res), "corrected")
})

test_that("gold_correct moves the estimate toward the corpus truth", {
  fx <- gold_correct_fixture()
  res <- gold_correct(fx$coded, fx$gold)
  a <- res$table[res$table$category == "A", ]
  truth_a <- mean(fx$truth == "A")
  expect_lt(abs(a$share_corrected - truth_a), abs(a$share_naive - truth_a))
})

test_that("gold_correct ledgers test-split use only when sealed", {
  sealed <- gold_correct_fixture(seal_holdout = TRUE)
  expect_equal(nrow(gold_ledger(sealed$gold)), 0L)
  gold_correct(sealed$coded, sealed$gold)
  expect_equal(nrow(gold_ledger(sealed$gold)), 1L)
  expect_equal(gold_ledger(sealed$gold)$accuracy, 16 / 20)

  unsealed <- gold_correct_fixture(seal_holdout = FALSE)
  gold_correct(unsealed$coded, unsealed$gold)
  expect_equal(nrow(gold_ledger(unsealed$gold)), 0L)
})

test_that("gold_correct aborts when no gold units match the corpus", {
  fx <- gold_correct_fixture(unmatched = TRUE)
  expect_warning(
    expect_error(gold_correct(fx$coded, fx$gold), "audited units must be part"),
    "did not match")
})

test_that("gold_correct counts NA corpus labels and excluded audit pairs", {
  fx <- gold_correct_fixture(parse_failure = TRUE)
  expect_warning(res <- gold_correct(fx$coded, fx$gold), "NA corpus label")
  expect_equal(res$n_parse_failures, 1L)
  expect_equal(res$n_audit_parse_failures, 1L)
  expect_equal(res$n_corpus, 39L)
  expect_equal(res$n_audit, 20L)
})

test_that("categories with zero corpus share still appear", {
  fx <- gold_correct_fixture()
  res <- gold_correct(fx$coded, fx$gold)
  c_row <- res$table[res$table$category == "C", ]
  expect_equal(nrow(c_row), 1L)
  expect_equal(c_row$share_naive, 0)
  expect_equal(c_row$share_corrected, 0)
  expect_equal(c_row$se, 0)
})

test_that("code_corpus stores the text column and codebook labels", {
  fx <- gold_correct_fixture()
  expect_identical(fx$coded$text, "text")
  expect_identical(fx$coded$label, "label")
  expect_identical(fx$coded$labels, c("A", "B", "C"))
})

test_that("code_corpus carries a .text_hash linkage column", {
  fx <- gold_correct_fixture()
  expect_true(".text_hash" %in% names(fx$coded$data))
  expect_identical(nrow(fx$coded$data), length(fx$coded$data$.text_hash))
})

# A fixture with duplicated corpus text where the duplicates carry DIFFERENT
# gold labels. Linking by text alone would collapse them to the first match.
dup_text_fixture <- function(use_id = FALSE) {
  cb <- codebook("stance", "one text",
    list(cb_category("A", "Category A."),
         cb_category("B", "Category B.")))
  p <- protocol_lock(protocol(cb, LLMR::llm_config("groq", "any-model"),
                              label = "dup"))

  # Two corpus rows share identical text "ambiguous" but are truly distinct
  # units; the rest are unique.
  texts <- c("ambiguous", "ambiguous", sprintf("unit%02d", 3:20))
  ids <- sprintf("id%02d", seq_along(texts))
  corpus <- data.frame(text = texts, uid = ids, stringsAsFactors = FALSE)

  # Gold audits both duplicate rows, giving them different gold labels.
  gold_rows <- c(1, 2, 5:14)
  gold_df <- data.frame(
    text = texts[gold_rows],
    uid = ids[gold_rows],
    label = c("A", "B", rep("A", 5), rep("B", 5)),
    stringsAsFactors = FALSE)

  gold <- suppressWarnings(if (use_id) {
    gold_set(gold_df, text = "text", label = "label",
             split = c(test = 1), id = "uid")
  } else {
    gold_set(gold_df, text = "text", label = "label", split = c(test = 1))
  })

  fake <- function(experiments, ...) {
    experiments$response_text <- "A"
    experiments
  }
  coded <- if (use_id) {
    code_corpus(corpus, p, "text", id = "uid", .runner = fake)
  } else {
    code_corpus(corpus, p, "text", .runner = fake)
  }
  list(coded = coded, gold = gold)
}

test_that("duplicated corpus text without an id is refused, not silently first-matched", {
  fx <- dup_text_fixture(use_id = FALSE)
  expect_error(
    gold_correct(fx$coded, fx$gold),
    "duplicated in the corpus"
  )
})

test_that("an explicit shared id disambiguates duplicated text", {
  fx <- dup_text_fixture(use_id = TRUE)
  res <- suppressWarnings(gold_correct(fx$coded, fx$gold))
  expect_s3_class(res, "gold_correction")
  expect_identical(res$link_by, "id")
  # both duplicate units were audited (12 audit pairs total)
  expect_equal(res$n_audit, 12L)
})

test_that("id linkage survives reordered corpus rows", {
  fx <- dup_text_fixture(use_id = TRUE)
  shuffled <- fx$coded
  shuffled$data <- fx$coded$data[
    rev(seq_len(nrow(fx$coded$data))), , drop = FALSE
  ]
  res <- suppressWarnings(gold_correct(shuffled, fx$gold))
  expect_identical(res$link_by, "id")
  expect_equal(res$n_audit, 12L)
})

test_that("duplicated corpus ids are refused, not silently first-matched (regression)", {
  # gold_set() enforces unique ids on the gold side; the coded corpus can still
  # carry duplicates, and the id-link path must refuse them exactly as the
  # text-hash path refuses duplicated text.
  fx <- dup_text_fixture(use_id = TRUE)
  dup <- fx$coded
  dup$data <- rbind(fx$coded$data, fx$coded$data[1, , drop = FALSE])
  expect_error(
    gold_correct(dup, fx$gold),
    "duplicated in the coded corpus"
  )
})
