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
