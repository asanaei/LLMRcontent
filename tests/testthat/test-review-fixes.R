# Regression tests for the 0.2.1 corrections: aggregation as an explicit
# measurement rule, parser and label validation, data-preservation refusals,
# the declared correction design, deterministic intervals, and the two-root
# archive seal.

.rf_cb <- function() {
  codebook("t", "one text",
           list(cb_category("apple", "A."), cb_category("banana", "B.")))
}
.rf_cfg <- function() LLMR::llm_config("groq", "demo", temperature = 0)
.rf_runner <- function(replies_by_rep) {
  force(replies_by_rep)
  i <- 0L
  function(experiments, ...) {
    i <<- i + 1L
    experiments$response_text <- replies_by_rep[[i]]
    experiments
  }
}

test_that("a replicate tie is NA with a flag, never a table-order pick", {
  p <- protocol_lock(protocol(.rf_cb(), .rf_cfg(), replicates = 2))
  r <- code_corpus(data.frame(text = "u1"), p, "text",
                   .runner = .rf_runner(list("banana", "apple")))
  expect_identical(r$data$label, NA_character_)
  expect_true(r$data$tied)
  expect_equal(r$data$label_share, 0.5)
})

test_that("label_share counts all replicates; the parsed share is separate", {
  p <- protocol_lock(protocol(.rf_cb(), .rf_cfg(), replicates = 10))
  r <- code_corpus(data.frame(text = "u1"), p, "text",
                   .runner = .rf_runner(as.list(c("apple", rep("junk", 9)))))
  expect_identical(r$data$label, "apple")
  expect_equal(r$data$label_share, 0.1)
  expect_equal(r$data$label_share_parsed, 1)
  expect_equal(r$data$parse_failures, 9L)
})

test_that("a parser value matching no codebook label is a parse failure", {
  p <- protocol_lock(protocol(.rf_cb(), .rf_cfg(),
                              parser = function(text, labels) "pineapple"))
  r <- code_corpus(data.frame(text = "u1"), p, "text",
                   .runner = .rf_runner(list("anything")))
  expect_identical(r$data$label, NA_character_)
  expect_equal(r$data$parse_failures, 1L)
})

test_that("a parser returning two values errors", {
  p <- protocol_lock(protocol(.rf_cb(), .rf_cfg(),
                              parser = function(text, labels) c("apple", "banana")))
  expect_error(
    code_corpus(data.frame(text = "u1"), p, "text",
                .runner = .rf_runner(list("x"))),
    "exactly one value")
})

test_that("labels colliding under normalization are refused", {
  expect_error(
    codebook("t", "u", list(cb_category("Yes", "y"),
                            cb_category("yes", "n"))),
    "normalization")
  expect_error(
    codebook("t", "u", list(cb_category("a b", "y"),
                            cb_category("A  B", "n"))),
    "normalization")
})

test_that("code_corpus refuses to overwrite existing result columns", {
  p <- protocol_lock(protocol(.rf_cb(), .rf_cfg()))
  expect_error(
    code_corpus(data.frame(text = "u", label = "mine"), p, "text",
                .runner = .rf_runner(list("apple"))),
    "already contains")
  expect_error(
    code_corpus(data.frame(text = "u", label_share = 1), p, "text",
                .runner = .rf_runner(list("apple"))),
    "already contains")
})

test_that("missing text is refused across the entry points", {
  p <- protocol_lock(protocol(.rf_cb(), .rf_cfg()))
  expect_error(
    code_corpus(data.frame(text = c("a", NA)), p, "text",
                .runner = .rf_runner(list(c("apple", "apple")))),
    "NA")
  expect_error(
    gold_set(data.frame(text = c("a", NA), label = c("apple", "banana")),
             "text", "label", split = c(test = 1)),
    "NA")
  expect_error(
    audit_plan(data.frame(text = NA_character_), "text",
               function(d) 1, c("a", "b"), "{text}"),
    "NA")
})

test_that("audit_plan refuses data that already carry a label column", {
  expect_error(
    audit_plan(data.frame(text = "a", label = "x"), "text",
               function(d) 1, c("a", "b"), "{text}"),
    "label")
})

test_that("gold_correct requires a declared sampling design", {
  p <- protocol_lock(protocol(.rf_cb(), .rf_cfg()))
  g <- gold_set(data.frame(text = c("a", "b"), label = c("apple", "banana")),
                "text", "label", split = c(test = 1))
  coded <- code_corpus(data.frame(text = c("a", "b")), p, "text",
                       .runner = .rf_runner(list(c("apple", "apple"))))
  expect_error(gold_correct(coded, g), "declared sampling design")
  expect_s3_class(gold_correct(coded, g, design = "srs"), "gold_correction")
})

test_that("duplicate gold texts cannot double-count one corpus row", {
  p <- protocol_lock(protocol(.rf_cb(), .rf_cfg()))
  g <- suppressWarnings(gold_set(
    data.frame(text = c("dup", "dup", "other"),
               label = c("apple", "apple", "banana")),
    "text", "label", split = c(test = 1)))
  coded <- code_corpus(data.frame(text = c("dup", "other")), p, "text",
                       .runner = .rf_runner(list(c("banana", "banana"))))
  expect_error(
    suppressWarnings(gold_correct(coded, g, design = "srs")),
    "already matched")
})

test_that("accuracy intervals are deterministic and never a point at 1", {
  pred <- c(rep("a", 30), rep("b", 10))
  gold <- rep("a", 40)
  expect_identical(.acc_ci(pred, gold), .acc_ci(pred, gold))
  perfect <- .acc_ci(gold, gold)
  expect_lt(perfect[1], 1)
  expect_equal(perfect[2], 1)
})

test_that("report refuses a protocol that does not match the validation", {
  p <- protocol_lock(protocol(.rf_cb(), .rf_cfg()))
  set.seed(110)
  g <- gold_set(
    data.frame(text = paste("u", 1:40),
               label = rep(c("apple", "banana"), 20)),
    "text", "label", split = c(dev = 0.5, test = 0.5))
  v <- validate_protocol(p, g, .runner = function(e, ...) {
    e$response_text <- rep(c("apple", "banana"), length.out = nrow(e))
    e
  })
  other <- protocol_lock(protocol(.rf_cb(), .rf_cfg(), replicates = 2))
  expect_error(LLMR::report(v, gold = g, protocol = other), "does not match")
  expect_s3_class(LLMR::report(v, gold = g, protocol = p), "coding_report")
})

.rf_log <- function() {
  log <- tempfile(fileext = ".jsonl")
  writeLines(c(
    paste0('{"ts":"2026-06-01T10:00:01+0000","schema_version":"1.0",',
           '"kind":"call","provider":"groq","model":"m1","request":{"q":1},',
           '"usage":{"sent":5,"rec":2},"response_id":"r-1","text":"reply1"}'),
    paste0('{"ts":"2026-06-01T10:00:02+0000","schema_version":"1.0",',
           '"kind":"call","provider":"groq","model":"m1","request":{"q":2},',
           '"usage":{"sent":5,"rec":2},"response_id":"r-2","text":"reply2"}')),
    log)
  log
}

test_that("an emptied or truncated record list breaks the shape check", {
  a <- archive_seal(archive_build(.rf_log()))
  gutted <- a
  gutted$records <- list()
  chk <- archive_check(gutted)
  expect_false(chk$shape_ok)
  expect_false(chk$records_ok)
  expect_false(chk$intact)
  short <- a
  short$records <- a$records[1]
  expect_false(archive_check(short)$intact)
})

test_that("manifest metadata sit inside the seal", {
  a <- archive_seal(archive_build(.rf_log()))
  tampered <- a
  tampered$manifest$model <- rep("swapped", nrow(tampered$manifest))
  chk <- archive_check(tampered)
  expect_true(chk$records_ok)
  expect_false(chk$root_ok)
  expect_false(chk$intact)
  expect_error(archive_drift(tampered, fraction = 1,
                             .runner = function(x, ...) x),
               "fails archive_check")
})

test_that("the content root is machine-stable and the seal root is not", {
  log <- .rf_log()
  a <- archive_seal(archive_build(log))
  Sys.sleep(1)
  b <- archive_seal(archive_build(log))
  expect_identical(a$seal$content_root, b$seal$content_root)
  expect_false(identical(a$seal$root, b$seal$root))
})

test_that("redaction still verifies under the two-root seal", {
  a <- archive_seal(archive_build(.rf_log()))
  r <- archive_redact(a)
  chk <- archive_check(r)
  expect_true(chk$intact)
  expect_true(chk$root_ok)
  expect_true(chk$public_root_ok)
  expect_identical(r$seal$root, a$seal$root)
  expect_identical(r$seal$content_root, a$seal$content_root)
})
