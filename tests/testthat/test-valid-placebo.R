# Placebo tests run entirely offline through the .runner seam.

fake_placebo_runner <- function(experiments, ...) {
  msg <- vapply(experiments$messages, `[[`, "", "user")
  experiments$response_text <- ifelse(grepl("signal", msg),
                                      "conservative", "progressive")
  experiments
}

make_placebo_audit <- function(estimator, n = 20L) {
  half_n <- n %/% 2L
  dat <- data.frame(
    text = c(rep("signal text", half_n), rep("ordinary text", half_n)),
    half = rep(c("first", "second"), each = half_n),
    stringsAsFactors = FALSE)
  plan <- audit_plan(
    data = dat, text = "text", estimator = estimator,
    labels = c("conservative", "progressive"),
    prompt = "One of {labels}: {text}\nLabel:")
  plan <- audit_add_models(plan,
    list(toy = LLMR::llm_config("groq", "any-model", temperature = 0)))
  audit_run(plan, .runner = fake_placebo_runner)
}

test_that("label permutation flags marginal estimators as degenerate", {
  audit <- make_placebo_audit(function(d) {
    mean(d$label == "conservative", na.rm = TRUE)
  })
  set.seed(110)
  pt <- audit_placebo(audit, reps = 25L)
  expect_s3_class(pt, "audit_placebo")
  expect_true(all(pt$cells$degenerate %in% TRUE))
  expect_true(all(is.na(pt$cells$p)))
  expect_output(print(pt), "degenerate")
})

test_that("label permutation gives a small p for an association estimator", {
  audit <- make_placebo_audit(function(d) {
    mean(d$label[d$half == "first"] == "conservative", na.rm = TRUE) -
      mean(d$label[d$half == "second"] == "conservative", na.rm = TRUE)
  })
  set.seed(110)
  pt <- audit_placebo(audit, reps = 199L)
  expect_false(pt$cells$degenerate[1])
  expect_equal(pt$cells$estimate[1], 1)
  expect_lt(pt$cells$p[1], .05)
  expect_equal(pt$cells$n_perm_failed[1], 0L)
  expect_true(pt$cells$null_lo[1] <= pt$cells$null_hi[1])
})

test_that("estimator errors inside permutations are counted, not fatal", {
  audit <- make_placebo_audit(function(d) {
    if (identical(d$label[1], "progressive")) stop("boom")
    mean(d$label[d$half == "first"] == "conservative", na.rm = TRUE) -
      mean(d$label[d$half == "second"] == "conservative", na.rm = TRUE)
  })
  set.seed(110)
  pt <- audit_placebo(audit, reps = 50L)
  expect_gt(pt$cells$n_perm_failed[1], 0L)
  expect_equal(pt$cells$n_perm_ok[1] + pt$cells$n_perm_failed[1], 50L)
})

test_that("irrelevant_text returns real and placebo estimates per cell", {
  audit <- make_placebo_audit(function(d) {
    mean(d$label == "conservative", na.rm = TRUE)
  }, n = 6L)
  pt <- audit_placebo(audit, type = "irrelevant_text",
                      texts = c("The forecast is cloudy.", "Winds are calm."),
                      .runner = fake_placebo_runner)
  expect_s3_class(pt, "audit_placebo")
  expect_equal(nrow(pt$cells), nrow(audit$cells))
  expect_equal(pt$cells$estimate, audit$cells$estimate)
  expect_equal(pt$cells$estimate_placebo, 0)
  expect_equal(pt$cells$parse_failures_placebo, 0L)
  expect_true(is.na(pt$reps))
  expect_output(print(pt), "descriptive")

  cells <- tibble::as_tibble(pt)
  expect_s3_class(cells, "tbl_df")
  expect_equal(cells, pt$cells)
})

test_that("audit_placebo validates its inputs", {
  expect_error(audit_placebo(data.frame()), "audit_run")
  audit <- make_placebo_audit(function(d) {
    mean(d$label == "conservative", na.rm = TRUE)
  }, n = 4L)
  expect_error(audit_placebo(audit, type = "irrelevant_text"),
               "`texts` is required")
  expect_error(audit_placebo(audit, reps = 0), "positive whole number")
})
