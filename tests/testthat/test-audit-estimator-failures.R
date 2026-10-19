# An audit cell's estimate is one finite number or an NA with a recorded
# reason; estimator failures are never coerced into a plausible value.

audit_with <- function(estimator) {
  plan <- audit_plan(data.frame(text = c("a", "b", "c")), "text", estimator,
                     c("yes", "no"), "{text}")
  plan <- audit_add_models(plan,
    list(demo = LLMR::llm_config("groq", "demo", temperature = 0)))
  runner <- function(x, ...) transform(x, response_text = c("yes", "no", "yes"))
  audit_run(plan, .runner = runner)
}

test_that("a valid scalar estimator is unchanged", {
  a <- audit_with(function(d) mean(d$label == "yes"))
  expect_equal(a$cells$estimate, 2 / 3)
  expect_identical(a$cells$estimate_status, "ok")
  expect_true(is.na(a$cells$estimate_note))
})

test_that("an estimator error is recorded and warned about", {
  expect_warning(a <- audit_with(function(d) stop("no such column")),
                 "estimator failed in 1 of 1")
  expect_true(is.na(a$cells$estimate))
  expect_identical(a$cells$estimate_status, "error")
  expect_match(a$cells$estimate_note, "no such column")
})

test_that("vectors, empty results, and infinite values are invalid, not truncated", {
  for (f in list(function(d) c(0.2, 0.8), function(d) numeric(0),
                 function(d) Inf, function(d) "0.5")) {
    expect_warning(a <- audit_with(f), "estimator failed")
    expect_true(is.na(a$cells$estimate))
    expect_identical(a$cells$estimate_status, "invalid")
  }
})

test_that("an NA estimate is recorded as missing", {
  a <- audit_with(function(d) NA_real_)
  expect_true(is.na(a$cells$estimate))
  expect_identical(a$cells$estimate_status, "missing")
  expect_identical(audit_stability(a)$status, "no_valid_estimates")
})

test_that("an estimator error message with braces does not break the warning", {
  expect_warning(audit_with(function(d) stop("bad {field} value")), "\\{field\\}")
})

test_that("failed permutation estimates are counted, explained, and warned about", {
  calls <- 0L
  est <- function(d) {
    calls <<- calls + 1L
    if (calls > 1L) stop("permutation broke")
    mean(d$label == "yes")
  }
  a <- audit_with(est)
  expect_warning(p <- audit_placebo(a, type = "label_permutation", reps = 5L),
                 "5 permutation estimate")
  expect_identical(p$cells$n_perm_failed, 5L)
  expect_match(p$cells$perm_note, "permutation broke")
})

test_that("each cell records how many parsed labels the estimator received", {
  plan <- audit_plan(data.frame(text = c("a", "b", "c", "d")), "text",
                     function(d) mean(d$label == "yes", na.rm = TRUE),
                     c("yes", "no"), "{text}")
  plan <- audit_add_models(plan, list(
    one = LLMR::llm_config("groq", "one", temperature = 0),
    two = LLMR::llm_config("groq", "two", temperature = 0)))
  runner <- function(x, ...) {
    model <- vapply(x$config, function(cfg) cfg$model, "")
    x$response_text <- ifelse(model == "two" & x$unit_id > 1L, "???", "yes")
    x
  }
  a <- audit_run(plan, .runner = runner)
  expect_identical(a$cells$n_labels, c(4L, 1L))
  expect_identical(a$cells$parse_failures, c(0L, 3L))
  expect_identical(a$cells$estimate, c(1, 1))
})

test_that("permuted estimates that are not one finite number are excluded", {
  returns <- list(vector = c(0.2, 0.8), empty = numeric(0), missing = NA_real_,
                  infinite = Inf, text = "0.5")
  notes <- c(vector = "length 2", empty = "length 0", missing = "NA or NaN",
             infinite = "infinite", text = "character of length 1")
  for (kind in names(returns)) {
    calls <- 0L
    est <- function(d) {
      calls <<- calls + 1L
      if (calls == 1L) return(mean(d$label == "yes"))
      returns[[kind]]
    }
    a <- audit_with(est)
    expect_identical(a$cells$estimate_status, "ok")
    expect_warning(p <- audit_placebo(a, type = "label_permutation", reps = 4L),
                   "4 permutation estimate", info = kind)
    expect_identical(p$cells$n_perm_failed, 4L, info = kind)
    expect_identical(p$cells$n_perm_ok, 0L, info = kind)
    expect_true(is.na(p$cells$p), info = kind)
    expect_true(is.na(p$cells$null_lo), info = kind)
    expect_match(p$cells$perm_note, notes[[kind]], fixed = TRUE, info = kind)
  }
})

test_that("a valid permutation estimator enters the null unchanged", {
  set.seed(110)
  a <- audit_with(function(d) mean(d$label == "yes"))
  expect_no_warning(p <- audit_placebo(a, type = "label_permutation", reps = 6L))
  expect_identical(p$cells$n_perm_ok, 6L)
  expect_identical(p$cells$n_perm_failed, 0L)
  expect_true(is.na(p$cells$perm_note))
  expect_true(p$cells$degenerate)
})
