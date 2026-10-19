# Corpus identifiers and split specifications are checked before any call.

test_that("code_corpus refuses missing or repeated ids before calling", {
  p <- protocol_lock(protocol(fix_codebook(), fix_config()))
  never <- function(experiments, ...) stop("runner should not be called")
  corpus <- data.frame(doc = c("a07", "a08", "a07", "a09", "a08"),
                       text = c("great", "bad", "fine", "awful", "lovely"))
  expect_error(code_corpus(corpus, p, "text", id = "doc", .runner = never),
               "2 id(s) repeat ('a07', 'a08')", fixed = TRUE)
  corpus$doc <- c("a01", NA, "a03", NA, "a05")
  expect_error(code_corpus(corpus, p, "text", id = "doc", .runner = never),
               "in `corpus`: 2 row(s) have no id", fixed = TRUE)
  corpus$doc <- c("007", "07", "7", "0007", "70")
  coded <- code_corpus(corpus, p, "text", id = "doc",
                       .runner = fake_runner_perfect)
  expect_identical(coded$data$doc, corpus$doc)
  # Without `id`, repeated values in other columns are not checked.
  corpus$doc <- "same"
  expect_s3_class(code_corpus(corpus, p, "text", .runner = fake_runner_perfect),
                  "coded_corpus")
})

test_that("gold_set names the offending ids", {
  d <- data.frame(case = c(1, 2, 2, NA), text = letters[1:4],
                  label = c("x", "y", "x", "y"))
  expect_error(gold_set(d, "text", "label", id = "case"),
               "in `data`: 1 row(s) have no id; 1 id(s) repeat ('2')",
               fixed = TRUE)
})

test_that("split specifications are refused unless they can be allocated", {
  d <- data.frame(text = paste("unit", 1:10), label = rep(c("x", "y"), 5))
  expect_error(gold_set(d, "text", "label", split = c(dev = 1.5, test = -0.5)),
               "finite and nonnegative: test = -0.5", fixed = TRUE)
  expect_error(gold_set(d, "text", "label", split = c(dev = NA, test = 1)),
               "finite and nonnegative: dev = NA", fixed = TRUE)
  expect_error(gold_set(d, "text", "label", split = c(dev = 0.5, dev = 0.5)),
               "'dev' appear(s) more than once", fixed = TRUE)
  expect_error(gold_set(d, "text", "label",
                        split = stats::setNames(c(0.5, 0.5), c("", "test"))),
               "`split` entries 1 have no name", fixed = TRUE)
  expect_error(gold_set(d, "text", "label", split = c(dev = 0.5, test = 0.4)),
               "these sum to 0.9", fixed = TRUE)
  expect_error(gold_set(d, "text", "label", split = c(0.5, 0.5)),
               "named vector of proportions", fixed = TRUE)
  expect_error(gold_set(d, "text", "label", split = c(dev = "0.5", test = "0.5")),
               "named vector of proportions", fixed = TRUE)
})

test_that("valid split specifications allocate as before", {
  d <- data.frame(text = paste("unit", 1:20), label = rep(c("x", "y"), 10))
  set.seed(110)
  g <- suppressWarnings(gold_set(d, "text", "label",
                                 split = c(dev = 0.5, test = 0.5, spare = 0)))
  expect_identical(as.integer(table(factor(g$split, c("dev", "test", "spare")))),
                   c(10L, 10L, 0L))
  set.seed(110)
  a <- suppressWarnings(gold_set(d, "text", "label"))
  set.seed(110)
  b <- suppressWarnings(gold_set(d, "text", "label"))
  expect_identical(a$split, b$split)
})

test_that("an empty requested split reports the allocation by label", {
  d <- data.frame(text = paste("unit", 1:8), label = rep(c("x", "y"), 4))
  g <- suppressWarnings(gold_set(d, "text", "label",
                                 split = c(dev = 0.95, test = 0.05)))
  expect_false("test" %in% g$split)
  err <- tryCatch(gold_split(g, "test"), error = function(e) e)
  expect_match(conditionMessage(err),
               "No split named 'test' in this gold set.", fixed = TRUE)
  expect_match(conditionMessage(err), "dev: x 4, y 4", fixed = TRUE)
  expect_identical(nrow(gold_split(g, "dev")), 8L)
})
