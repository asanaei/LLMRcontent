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

studio_audit_shared <- function() {
  list(
    mode = shiny::reactive("demo"), provider = shiny::reactive("groq"),
    model = shiny::reactive(""), temperature = shiny::reactive(0),
    max_tokens = shiny::reactive(128L), reasoning_effort = shiny::reactive("low"),
    can_run = shiny::reactive(TRUE), key = shiny::reactive(list()),
    set_plan = function(...) NULL, add_usage = function(...) NULL)
}

test_that("the Studio says a zero reference has no sign to flip", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")
  skip_if_not_installed("LLMR.shiny", "0.1.3")
  shiny::testServer(
    LLMRcontent:::mod_valid_server,
    args = list(shared = studio_audit_shared(), active = shiny::reactive("valid")),
    {
      # The demo responder labels two of the six demo texts conservative, so
      # the most common label is progressive in every cell.
      session$setInputs(load_demo = 1)
      session$setInputs(text_col = "text", labels = "conservative, progressive",
                        estimand = "mode_is", target = "conservative",
                        prompt = "Classify as {labels}.\n\n{text}",
                        orders = "as_given", temps = "0", add_paraphrase = TRUE)
      run_audit(confirmed = TRUE)
      session$flushReact()
      expect_identical(audit()$cells$estimate, c(0, 0))
      status <- paste(as.character(output$audit_status), collapse = "")
      expect_match(status, "The reference estimate is 0, which has no sign",
                   fixed = TRUE)
      expect_match(status, "most-common-label estimand is 0 or 1", fixed = TRUE)
      expect_false(grepl("No cell in this grid changes", status, fixed = TRUE))

      session$setInputs(estimand = "share")
      run_audit(confirmed = TRUE)
      session$flushReact()
      status <- paste(as.character(output$audit_status), collapse = "")
      expect_false(grepl("has no sign", status, fixed = TRUE))
      expect_match(status, "Across 2 cells", fixed = TRUE)
    }
  )
})

test_that("the Studio headline counts only the cells with an estimate", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")
  skip_if_not_installed("LLMR.shiny", "0.1.3")
  plan <- audit_plan(
    data.frame(text = c("one", "two", "three")), "text",
    function(d) if (all(d$label == "no")) stop("no variation") else
      mean(d$label == "yes") - 0.5,
    c("yes", "no"), "{text}")
  plan <- audit_add_models(plan, list(
    a = LLMR::llm_config("groq", "a", temperature = 0),
    b = LLMR::llm_config("groq", "b", temperature = 0),
    c = LLMR::llm_config("groq", "c", temperature = 0)))
  runner <- function(experiments, ...) {
    model <- vapply(experiments$config, function(x) x$model, "")
    experiments$response_text <- ifelse(model == "b", "no",
                                 ifelse(model == "a", "yes", "no"))
    experiments$response_text[model == "c" & experiments$unit_id == 1L] <- "yes"
    experiments
  }
  result <- suppressWarnings(audit_run(plan, .runner = runner))
  expect_identical(result$cells$estimate_status, c("ok", "error", "ok"))

  shiny::testServer(
    LLMRcontent:::mod_valid_server,
    args = list(shared = studio_audit_shared(), active = shiny::reactive("valid")),
    {
      audit(result)
      # The specification curve leaves out the failed cell, and ggplot2 warns.
      suppressWarnings(session$flushReact())
      status <- paste(as.character(output$audit_status), collapse = "")
      expect_match(status, "Across the 2 of 3 cells with an estimate",
                   fixed = TRUE)
      expect_match(status, "estimates range from -0.167 to 0.500", fixed = TRUE)
    }
  )
})
