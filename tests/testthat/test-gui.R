# The optional Shiny GUI's glue: demo responders, the column-mapped API wrappers,
# app assembly, the deps guard, and the install helper. The Shiny machinery is a
# Suggests concern, so the assembly tests skip when the GUI packages are absent.

test_that("coder demo responder labels by keyword over codebook labels", {
  resp <- LLMRcontent:::coder_demo_responder(NULL)  # default labels: policy/community/other
  expect_equal(resp("a new public policy on voting rights"), "policy")
  expect_equal(resp("my family and neighbors helped"), "community")
})

test_that("demo_labels_from_codebook falls back to defaults without a codebook", {
  labs <- LLMRcontent:::demo_labels_from_codebook(NULL)
  expect_true(length(labs) >= 2)
  expect_true("policy" %in% labs)
})

test_that("valid demo responder routes by keyword and survives empty labels", {
  resp <- LLMRcontent:::valid_demo_responder(c("conservative", "progressive"))
  expect_equal(resp("we should cut taxes and deregulate"), "conservative")
  expect_equal(resp("fund schools and expand the safety net"), "progressive")
  expect_equal(resp(NULL), "progressive")          # empty text -> last label
  # an all-blank label vector falls back to a/b rather than erroring
  fallback <- LLMRcontent:::valid_demo_responder(c("", ""))
  expect_equal(fallback("cut taxes"), "a")
  expect_equal(fallback("anything else"), "b")
})

test_that("call_gold_set_mapped maps columns into a sealed gold set", {
  df <- data.frame(
    body = c(paste("policy unit", 1:6), paste("community unit", 1:6)),
    cat  = rep(c("policy", "community"), each = 6),
    stringsAsFactors = FALSE)
  g <- suppressWarnings(LLMRcontent:::call_gold_set_mapped(df, "body", "cat",
        split = c(dev = 0.5, test = 0.5), stratify = TRUE, seal_test = TRUE))
  expect_s3_class(g, "gold_set")
  expect_true(isTRUE(g$sealed))
})

test_that("the GUI assembles when its suggested packages are present", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")
  skip_if_not_installed("LLMR.shiny")
  expect_s3_class(LLMRcontent:::.content_gui_ui(), "bslib_page")
  expect_true(is.function(LLMRcontent:::.content_gui_server))
})

test_that(".content_gui_require errors helpfully when a GUI package is missing", {
  need <- c("shiny", "bslib", "DT", "LLMR.shiny")
  if (all(vapply(need, requireNamespace, logical(1), quietly = TRUE))) {
    expect_true(isTRUE(LLMRcontent:::.content_gui_require()))
  } else {
    expect_error(LLMRcontent:::.content_gui_require(),
                 "GUI needs these packages")
  }
})

test_that("install_gui_deps routes CRAN deps vs the LLMR.shiny special case", {
  # Mock install.packages so nothing is actually installed, and force every
  # have() check to report "missing" so the install branch runs for each dep.
  installed <- character(0)
  testthat::local_mocked_bindings(
    install.packages = function(pkgs, ...) installed <<- c(installed, pkgs),
    .package = "utils")
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) FALSE,
    .package = "base")
  status <- install_gui_deps(quiet = TRUE)
  # CRAN deps were attempted; LLMR.shiny reported unavailable (FALSE), not an error.
  expect_true(all(c("shiny", "bslib", "DT") %in% installed))
  expect_false(status[["LLMR.shiny"]])
  expect_named(status, c("shiny", "bslib", "DT", "LLMR.shiny"))
})
