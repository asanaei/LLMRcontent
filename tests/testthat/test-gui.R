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
        split = c(dev = 0.5, test = 0.5), stratify = TRUE,
        seal_holdout = TRUE))
  expect_s3_class(g, "gold_set")
  expect_true(isTRUE(g$sealed))
})

test_that("the coder bundle writes a flat CSV and a generic report", {
  skip_if_not_installed("LLMR.shiny")
  gold <- fix_gold(8)
  protocol <- protocol_lock(protocol(fix_codebook(), fix_config(),
                                     label = "bundle"))
  validation <- validate_protocol(protocol, gold,
                                  .runner = fake_runner_perfect)
  coded <- code_corpus(data.frame(text = gold$data$text), protocol, "text",
                       .runner = fake_runner_perfect)
  path <- tempfile(fileext = ".zip")
  out_dir <- tempfile("bundle-")
  dir.create(out_dir)
  on.exit(unlink(c(path, out_dir), recursive = TRUE), add = TRUE)

  expect_invisible(LLMRcontent:::bundle_coder_artifacts(
    coded, validation, gold, protocol, path
  ))
  utils::unzip(path, exdir = out_dir)

  exported <- utils::read.csv(file.path(out_dir, "coded.csv"))
  expect_equal(nrow(exported), nrow(coded$data))
  expect_identical(exported$label, coded$data$label)
  expect_match(paste(readLines(file.path(out_dir, "summary.txt")),
                     collapse = "\n"),
               "methods report from LLMR::report", fixed = TRUE)
})

test_that("the GUI assembles when its suggested packages are present", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")
  skip_if_not_installed("LLMR.shiny")
  expect_s3_class(LLMRcontent:::.content_gui_ui(), "bslib_page")
  expect_true(is.function(LLMRcontent:::.content_gui_server))
})

test_that("the demo audit records result rows without API calls", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("LLMR.shiny")
  usage_seen <- new.env(parent = emptyenv())
  usage_seen$value <- NULL
  usage_seen$plans <- integer()
  shared <- list(
    mode = shiny::reactive("demo"),
    provider = shiny::reactive("groq"),
    model = shiny::reactive(""),
    can_run = shiny::reactive(TRUE),
    key = shiny::reactive(list()),
    set_plan = function(calls, label = "Next run") {
      usage_seen$plans <- c(usage_seen$plans, as.integer(calls))
    },
    add_usage = function(tokens) usage_seen$value <- tokens
  )

  shiny::testServer(
    LLMRcontent:::mod_valid_server,
    args = list(shared = shared, active = shiny::reactive("valid")),
    {
      session$setInputs(
        labels = "conservative, progressive",
        estimand = "share",
        target = "conservative",
        prompt = "Classify as {labels}.\n\n{text}\n\nLabel:",
        orders = c("as_given", "reversed"),
        temps = "0, 0.7",
        add_paraphrase = TRUE,
        load_demo = 1
      )
      session$flushReact()
      session$setInputs(text_col = "text", run_audit = 1)
      session$flushReact()
      expect_s3_class(audit(), "audit")
      expect_true(LLMR.shiny::is_demo_result(audit()))
    }
  )

  expect_identical(usage_seen$value$result_rows, 48L)
  expect_null(usage_seen$value$calls)
  expect_false(any(usage_seen$plans > 0L))
})

test_that(".content_gui_require gives ordinary installation instructions", {
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) FALSE,
    .package = "base"
  )
  expect_error(LLMRcontent:::.content_gui_require(), "install\\.packages")
})
