# The Studio's menu estimands return NA when the conclusion is undefined, so
# the audit records a missing estimate instead of a manufactured 0 or 1.

test_that("the mode estimand is undefined without parsed labels or with a tie", {
  mode_is <- make_estimator("mode_is", target = "yes")
  expect_identical(mode_is(data.frame(label = c("yes", "yes", "no"))), 1)
  expect_identical(mode_is(data.frame(label = c("no", "no", "yes"))), 0)
  expect_identical(mode_is(data.frame(label = c(NA_character_, NA_character_))), NA_real_)
  expect_identical(mode_is(data.frame(label = c("yes", "no"))), NA_real_)
  expect_identical(mode_is(data.frame(label = c("no", "yes"))), NA_real_)
})
