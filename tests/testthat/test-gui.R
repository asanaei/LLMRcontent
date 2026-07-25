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

test_that("the coder bundle marks demo components separately", {
  skip_if_not_installed("LLMR.shiny")
  gold <- fix_gold(8)
  protocol <- protocol_lock(protocol(
    fix_codebook(), fix_config(), label = "mixed-bundle"
  ))
  validation <- validate_protocol(
    protocol, gold, .runner = fake_runner_perfect
  )
  coded <- code_corpus(
    data.frame(text = gold$data$text), protocol, "text",
    .runner = fake_runner_perfect
  )
  path <- tempfile(fileext = ".zip")
  out_dir <- tempfile("mixed-bundle-")
  dir.create(out_dir)
  on.exit(unlink(c(path, out_dir), recursive = TRUE), add = TRUE)

  expect_invisible(LLMRcontent:::bundle_coder_artifacts(
    coded, validation, gold, protocol, path,
    coded_demo = FALSE, validation_demo = TRUE
  ))
  utils::unzip(path, exdir = out_dir)

  exported <- utils::read.csv(file.path(out_dir, "coded.csv"))
  methods <- paste(
    readLines(file.path(out_dir, "methods.txt")), collapse = "\n"
  )
  summary <- paste(
    readLines(file.path(out_dir, "summary.txt")), collapse = "\n"
  )
  expect_false("demo_notice" %in% names(exported))
  expect_match(methods, LLMR.shiny::demo_notice(), fixed = TRUE)
  expect_match(
    summary,
    paste("methods.txt:", LLMR.shiny::demo_notice()),
    fixed = TRUE
  )
  expect_false(grepl(
    paste("coded.csv:", LLMR.shiny::demo_notice()),
    summary,
    fixed = TRUE
  ))
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
    temperature = shiny::reactive(0.7),
    max_tokens = shiny::reactive(128L),
    reasoning_effort = shiny::reactive("low"),
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
      expect_s3_class(placebo(), "audit_placebo")
      config <- audit()$plan$models[[1]]
      expect_equal(config$model_params$temperature, 0.7)
      expect_equal(config$model_params$max_tokens, 128L)
      expect_equal(config$model_params$reasoning_effort, "low")
      unit_table <- LLMRcontent:::.content_audit_units_table(audit())
      expect_true(all(c("text", "cell", "unit_id") %in% names(unit_table)))
      expect_true(all(nzchar(unit_table$text)))
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

test_that("the content UI uses non-fillable pages and shared generation controls", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")
  skip_if_not_installed("LLMR.shiny")

  ui_source <- paste(deparse(body(LLMRcontent:::.content_gui_ui)), collapse = "\n")
  expect_match(ui_source, "fillable = FALSE", fixed = TRUE)
  module_source <- paste(
    deparse(body(LLMRcontent:::mod_coder_server)),
    deparse(body(LLMRcontent:::mod_valid_server)),
    deparse(body(LLMRcontent:::mod_archive_server)),
    collapse = "\n"
  )
  expect_match(module_source, "text_block_output", fixed = TRUE)
  expect_false(grepl("verbatimTextOutput", module_source, fixed = TRUE))
  expect_match(
    module_source,
    "coded_demo = is_demo_result(coded())",
    fixed = TRUE
  )
  expect_match(
    module_source,
    "validation_demo = is_demo_result(validation())",
    fixed = TRUE
  )
  expect_false(grepl(
    'identical(shared$mode(), "demo") || is_demo_result(coded())',
    module_source,
    fixed = TRUE
  ))

  shared <- list(
    mode = function() "demo",
    provider = function() "groq",
    model = function() "demo",
    can_run = function() TRUE,
    key = function() list()
  )
  config_html <- as.character(
    LLMRcontent:::coder_config_ui(shiny::NS("coder"), shared)
  )
  expect_match(config_html, "coder-protocol_replicates", fixed = TRUE)
  expect_match(config_html, "Coding prompt template", fixed = TRUE)
  expect_match(config_html, "Use this codebook:", fixed = TRUE)
  expect_match(config_html, "{codebook}", fixed = TRUE)
  expect_match(config_html, "{text}", fixed = TRUE)
  expect_false(grepl("coder-temperature", config_html, fixed = TRUE))

  gold_html <- as.character(
    LLMRcontent:::coder_gold_ui(shiny::NS("coder"))
  )
  expect_match(gold_html, "llmr-text-block", fixed = TRUE)
  expect_match(gold_html, "circle-info", fixed = TRUE)
})

test_that("coding steps use one open drawer and keep run actions outside", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")
  skip_if_not_installed("LLMR.shiny")

  ns <- shiny::NS("coder")
  steps_html <- paste(
    as.character(LLMRcontent:::coder_steps_ui(ns)),
    collapse = "\n"
  )
  expect_true(all(vapply(
    1:7,
    function(value) {
      grepl(
        sprintf('data-value="%d"', value),
        steps_html,
        fixed = TRUE
      )
    },
    logical(1)
  )))
  expect_equal(
    lengths(regmatches(
      steps_html,
      gregexpr('aria-expanded="true"', steps_html, fixed = TRUE)
    )),
    1L
  )
  expect_false(grepl("coder-run_tune", steps_html, fixed = TRUE))
  expect_false(grepl("coder-run_validate", steps_html, fixed = TRUE))
  expect_false(grepl("coder-run_code_corpus", steps_html, fixed = TRUE))

  shared <- list(
    mode = function() "demo",
    can_run = function() TRUE
  )
  tune_actions <- as.character(
    LLMRcontent:::coder_step_action_ui(ns, 4L, shared)
  )
  validation_actions <- as.character(
    LLMRcontent:::coder_step_action_ui(ns, 5L, shared)
  )
  corpus_actions <- as.character(
    LLMRcontent:::coder_step_action_ui(ns, 6L, shared)
  )
  expect_match(tune_actions, "coder-run_tune", fixed = TRUE)
  expect_match(validation_actions, "coder-run_validate", fixed = TRUE)
  expect_match(corpus_actions, "coder-corpus_run_action", fixed = TRUE)

  server_code <- paste(
    deparse(body(LLMRcontent:::mod_coder_server)),
    collapse = "\n"
  )
  expect_match(
    server_code,
    'accordion_panel_set("coding_steps"',
    fixed = TRUE
  )
  expect_false(grepl(
    'accordion_panel_set(session$ns("coding_steps")',
    server_code,
    fixed = TRUE
  ))
})

test_that("codebook wording starts visible and editable", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")
  skip_if_not_installed("LLMR.shiny")

  shared <- list(
    mode = shiny::reactive("demo"),
    provider = shiny::reactive("groq"),
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
    LLMRcontent:::mod_coder_server,
    args = list(shared = shared, active = shiny::reactive("home")),
    {
      session$flushReact()
      editor_html <- paste(
        as.character(output$category_editor),
        collapse = "\n"
      )
      expect_match(editor_html, "cat_definition_1", fixed = TRUE)
      expect_match(
        editor_html,
        "Mentions formal rules, rights, government, or public policy.",
        fixed = TRUE
      )
      expect_match(editor_html, "cat_examples_2", fixed = TRUE)
      expect_true(all(nzchar(categories()$definition)))
    }
  )
})

test_that("display tables round doubles, widen text, and retain ids at right", {
  skip_if_not_installed("DT")
  skip_if_not_installed("LLMR.shiny")

  input <- data.frame(
    response_id = c("r1", "r2"),
    text = c("A full sentence remains readable.", "<script>x</script>"),
    estimate = c(0.1234567, 12.34567),
    n = 1:2,
    stringsAsFactors = FALSE
  )
  original <- input$estimate
  prepared <- LLMRcontent:::.content_prepare_table(
    input,
    text = "text",
    digits = 3
  )

  expect_identical(tail(names(prepared$data), 1), "response_id")
  expect_equal(prepared$data$estimate, signif(original, 3))
  expect_type(prepared$data$n, "integer")
  expect_equal(input$estimate, original)

  widget <- LLMRcontent:::.content_datatable(
    input,
    text = "text",
    digits = 3
  )
  expect_true(widget$x$options$autoWidth)
  text_def <- Filter(
    function(definition) identical(definition$width, "60%"),
    widget$x$options$columnDefs
  )[[1]]
  expect_match(as.character(text_def$render), "render.text", fixed = TRUE)
  expect_match(
    as.character(text_def$createdCell),
    "white-space':'normal",
    fixed = TRUE
  )
})

test_that("GUI timing summaries use only recorded runner durations", {
  calls <- data.frame(
    unit_id = c(1L, 1L, 2L),
    response_id = c("a", "b", "c"),
    duration = c(0.2, 0.3, 0.4)
  )
  timing <- LLMRcontent:::finish_call_timing(
    list(calls),
    proc.time()[["elapsed"]]
  )
  expect_equal(sum(timing$calls$.duration_seconds), 0.9)
  expect_match(LLMRcontent:::timing_summary_text(timing), "3 calls")
  by_unit <- LLMRcontent:::timing_by_unit(timing)
  expect_equal(by_unit$duration_seconds, c(0.5, 0.4))

  expect_null(LLMRcontent:::finish_call_timing(
    list(data.frame(unit_id = 1L)),
    proc.time()[["elapsed"]]
  ))
})

test_that("Archive can check carried coded-call records and retain its artifact", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")
  skip_if_not_installed("LLMR.shiny")

  shared <- list(provider = shiny::reactive("groq"))
  artifacts <- shiny::reactiveValues(
    coded = NULL,
    coded_calls = NULL,
    archive = NULL
  )
  carried <- data.frame(response_id = c("r-1", "missing"))

  shiny::testServer(
    LLMRcontent:::mod_archive_server,
    args = list(
      shared = shared,
      artifacts = artifacts,
      coded = function() structure(
        list(data = data.frame(label = "positive")),
        class = "coded_corpus"
      ),
      coded_calls = function() carried
    ),
    {
      session$setInputs(load_demo = 1)
      session$flushReact()
      session$setInputs(build = 1, use_coded_calls = TRUE)
      session$flushReact()

      expect_s3_class(archive(), "archive")
      expect_s3_class(artifacts$archive, "archive")
      expect_equal(check_result()$n_results, 2L)
      expect_equal(check_result()$n_matched, 1L)

      session$setInputs(seal = 1)
      session$flushReact()
      expect_true(current_archive()$sealed)

      session$setInputs(redact = 1)
      session$flushReact()
      expect_true(current_archive()$redacted)
      expect_s3_class(replay_result(), "error")
    }
  )
})

test_that("only unredacted archives are offered for replay", {
  replayable <- structure(
    list(redacted = FALSE),
    class = "archive"
  )
  redacted <- structure(
    list(redacted = TRUE),
    class = "archive"
  )

  expect_true(LLMRcontent:::.content_archive_replay_available(replayable))
  expect_false(LLMRcontent:::.content_archive_replay_available(redacted))
  expect_false(LLMRcontent:::.content_archive_replay_available(NULL))
})

test_that("gold split scale previews do not truncate large splits", {
  gold <- structure(
    list(
      data = data.frame(text = seq_len(1200L)),
      split = c(rep("dev", 900L), rep("test", 300L))
    ),
    class = "gold_set"
  )

  expect_equal(LLMRcontent:::.content_gold_split_size(gold, "dev"), 900L)
  expect_equal(LLMRcontent:::.content_gold_split_size(gold, "test"), 300L)
})

test_that("ggplot2 is optional at GUI launch", {
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) {
      !identical(package, "ggplot2")
    },
    .package = "base"
  )
  expect_no_error(LLMRcontent:::.content_gui_require())
})
