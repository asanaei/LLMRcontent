# The Studio audit's temperature field is refused, not repaired, when a token
# is not a number or falls outside the provider's range.

test_that("valid temperature lists parse unchanged", {
  parse <- LLMRcontent:::.valid_parse_temperatures
  expect_identical(parse("0")$values, 0)
  expect_identical(parse("0, 0.7, 1")$values, c(0, 0.7, 1))
  expect_identical(parse(" 0.5 ,1.0")$values, c(0.5, 1))
  expect_identical(parse("0, 1,")$values, c(0, 1))
  expect_identical(parse(".5, 2, 1e-1")$values, c(0.5, 2, 0.1))
  expect_identical(parse("1", provider = "anthropic")$values, 1)
  expect_null(parse("0, 0.7, 1")$problem)
})

test_that("a token that is not a number is named and refuses the grid", {
  parse <- LLMRcontent:::.valid_parse_temperatures
  out <- parse("0, 0.7x, 1")
  expect_identical(out$values, numeric())
  expect_identical(out$problem, "'0.7x' is not a number")
  expect_identical(parse("0 0.7")$problem, "'0 0.7' is not a number")
  expect_identical(parse("0;1")$problem, "'0;1' is not a number")
  # as.numeric() would read these as numbers or as missing values.
  expect_identical(parse("0x1")$problem, "'0x1' is not a number")
  expect_identical(parse("0, NaN")$problem, "'NaN' is not a number")
  expect_identical(parse("Inf")$problem, "'Inf' is not a number")
  expect_identical(parse("NA, 0")$problem, "'NA' is not a number")
  expect_identical(parse("")$problem, "no temperature was entered")
  expect_identical(parse(" , ")$values, numeric())
})

test_that("a temperature outside the provider's range is named", {
  parse <- LLMRcontent:::.valid_parse_temperatures
  out <- parse("0,0,7")
  expect_identical(out$values, numeric())
  expect_identical(out$problem, "'7' is outside that range")
  expect_identical(out$range, "from 0 to 2")
  expect_identical(parse("0, -1, 5")$problem, "'-1' is outside that range")
  expect_identical(parse("2.01")$problem, "'2.01' is outside that range")
  expect_identical(parse("1.5")$values, 1.5)
  out <- parse("0, 1.5", provider = "anthropic")
  expect_identical(out$problem, "'1.5' is outside that range")
  expect_match(out$range, "from 0 to 1, the range for Anthropic models",
               fixed = TRUE)
})

test_that("the audit button names a refused temperature and the run stops", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")
  skip_if_not_installed("LLMR.shiny", "0.1.3")
  provider <- shiny::reactiveVal("groq")
  shared <- list(
    mode = shiny::reactive("demo"),
    provider = provider,
    model = shiny::reactive(""),
    temperature = shiny::reactive(0),
    max_tokens = shiny::reactive(128L),
    reasoning_effort = shiny::reactive("low"),
    can_run = shiny::reactive(TRUE),
    key = shiny::reactive(list()),
    set_plan = function(...) NULL,
    add_usage = function(...) NULL
  )
  shiny::testServer(
    LLMRcontent:::mod_valid_server,
    args = list(shared = shared, active = shiny::reactive("valid")),
    {
      data_raw(data.frame(text = c("cut taxes", "fund schools")))
      session$setInputs(text_col = "text", labels = "conservative, progressive",
                        estimand = "share", target = "conservative",
                        prompt = "Classify as {labels}.\n\n{text}",
                        orders = "as_given", temps = "0, 0.7x, 1",
                        add_paraphrase = FALSE)
      session$flushReact()
      action <- paste(as.character(output$run_audit_action), collapse = "\n")
      expect_match(action, "disabled", fixed = TRUE)
      expect_match(action, "'0.7x' is not a number", fixed = TRUE)

      run_audit(confirmed = TRUE)
      expect_null(audit())
      expect_match(paste(as.character(run_error()), collapse = ""),
                   "'0.7x' is not a number", fixed = TRUE)

      session$setInputs(temps = "0,0,7")
      session$flushReact()
      action <- paste(as.character(output$run_audit_action), collapse = "\n")
      expect_match(action, "'7' is outside that range", fixed = TRUE)

      session$setInputs(temps = "0, 1.5")
      session$flushReact()
      action <- paste(as.character(output$run_audit_action), collapse = "\n")
      expect_false(grepl("disabled", action, fixed = TRUE))
      provider("anthropic")
      session$flushReact()
      action <- paste(as.character(output$run_audit_action), collapse = "\n")
      expect_match(action, "the range for Anthropic models; '1.5' is outside",
                   fixed = TRUE)

      provider("groq")
      session$setInputs(temps = "0, 0.7")
      session$flushReact()
      run_audit(confirmed = TRUE)
      expect_s3_class(audit(), "audit")
      expect_setequal(unique(audit()$cells$temperature), c(0, 0.7))
    }
  )
})

test_that("the sidebar temperature is held to the same range", {
  parse <- LLMRcontent:::.valid_parse_temperatures
  expect_identical(parse("0", "groq", sidebar = 1.5)$values, 0)
  out <- parse("0", "anthropic", sidebar = 1.5)
  expect_identical(out$values, numeric())
  expect_identical(out$problem,
                   "the sidebar temperature, 1.5, is outside that range")
  expect_identical(parse("0", "groq", sidebar = NA_real_)$problem,
                   "the sidebar temperature is not a number")
  expect_identical(parse("0", "groq", sidebar = 3)$problem,
                   "the sidebar temperature, 3, is outside that range")
  expect_null(parse("0", "groq", sidebar = NULL)$problem)
})
