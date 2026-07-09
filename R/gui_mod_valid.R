# mod_valid.R ------------------------------------------------------------------
# The LLMRcontent robustness-audit workflow: declare an estimand over LLM labels,
# build a grid of measurement choices (models, prompt paraphrases, label-order
# and temperature perturbations), run it, and read off whether the conclusion is
# stable or fragile. The estimand is the one thing a non-coder cannot type, so it
# is chosen from a menu that maps to a real estimator closure.

# Substrate helpers are lazy forwarders in gui_aliases.R (see mod_coder).

# Pre-built estimands. Each returns a function (data-with-$label) -> numeric, so
# a non-coder picks the conclusion's shape without writing R. The target label
# is filled in from the UI for the share estimands.
valid_estimators <- function() {
  list(
    "Share of a target label"      = "share",
    "Difference in two label shares" = "diff",
    "Most common label is the target" = "mode_is"
  )
}

make_estimator <- function(kind, target, other = NULL) {
  switch(
    kind,
    share = function(d) mean(d$label == target, na.rm = TRUE),
    diff  = function(d) mean(d$label == target, na.rm = TRUE) -
                        mean(d$label == other, na.rm = TRUE),
    mode_is = function(d) {
      tab <- sort(table(d$label[!is.na(d$label)]), decreasing = TRUE)
      as.numeric(length(tab) > 0 && names(tab)[1] == target)
    },
    function(d) mean(d$label == target, na.rm = TRUE)
  )
}

# A demo responder that labels by keyword over the declared labels, so the audit
# runs offline.
valid_demo_responder <- function(labels) {
  labs <- labels[nzchar(labels)]
  if (!length(labs)) labs <- c("a", "b")
  function(text) {
    text <- tolower(text %||% "")
    if (grepl("tax|cut|deregulat|conservativ|right", text)) return(labs[[1]])
    labs[[length(labs)]]
  }
}

mod_valid_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("module_ui"))
}

mod_valid_server <- function(id, shared) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    data_raw <- shiny::reactiveVal(NULL)
    audit    <- shiny::reactiveVal(NULL)
    run_error <- shiny::reactiveVal(NULL)

    output$module_ui <- shiny::renderUI({
      if (!pkg_available("LLMRcontent")) return(install_guidance_ui("LLMRcontent"))
      bslib::card(
        bslib::card_header("Measurement-robustness audit"),
        bslib::card_body(
          shiny::uiOutput(ns("run_error")),
          shiny::fluidRow(
            shiny::column(6, shiny::fileInput(ns("data_file"), "Data CSV", accept = ".csv")),
            shiny::column(6, shiny::actionButton(ns("load_demo"), "Load demo data"))
          ),
          shiny::uiOutput(ns("map_ui")),
          shiny::textInput(ns("labels"), "Labels (comma-separated, baseline order)",
                           value = "conservative, progressive"),
          shiny::selectInput(ns("estimand"), "Conclusion (estimand)",
                             choices = valid_estimators()),
          shiny::uiOutput(ns("target_ui")),
          shiny::textAreaInput(ns("prompt"), "Baseline prompt (must contain {text}; may use {labels})",
            value = "Classify the text as one of: {labels}.\n\n{text}\n\nLabel:", rows = 5),
          shiny::tags$hr(),
          shiny::tags$strong("Grid"),
          shiny::fluidRow(
            shiny::column(6, shiny::checkboxGroupInput(ns("orders"), "Label order",
              choices = c("as given" = "as_given", "reversed" = "reversed"),
              selected = c("as_given", "reversed"))),
            shiny::column(6, shiny::textInput(ns("temps"), "Temperatures (comma-separated)", value = "0, 0.7"))
          ),
          shiny::checkboxInput(ns("add_paraphrase"),
            "Add a prompt paraphrase to the grid", value = TRUE),
          if (identical(shared$mode(), "demo")) demo_banner_ui(),
          shiny::actionButton(ns("run_audit"), "Run audit", class = "btn-primary"),
          shiny::tags$hr(),
          shiny::uiOutput(ns("results"))
        )
      )
    })

    output$run_error <- shiny::renderUI(run_error())

    output$map_ui <- shiny::renderUI({
      df <- data_raw(); if (is.null(df)) return(NULL)
      shiny::selectInput(ns("text_col"), "Text column",
                         choices = column_names_for_mapping(df))
    })

    output$target_ui <- shiny::renderUI({
      labs <- parse_labels()
      kind <- input$estimand %||% "share"
      ui <- list(shiny::selectInput(ns("target"), "Target label", choices = labs))
      if (identical(kind, "diff")) {
        ui <- c(ui, list(shiny::selectInput(ns("other"), "Compared-with label",
                                            choices = labs,
                                            selected = labs[min(2, length(labs))])))
      }
      do.call(shiny::tagList, ui)
    })

    parse_labels <- function() {
      x <- trimws(unlist(strsplit(input$labels %||% "", ",", fixed = TRUE)))
      x[nzchar(x)]
    }
    parse_temps <- function() {
      x <- suppressWarnings(as.numeric(trimws(unlist(strsplit(input$temps %||% "0", ",")))))
      x[!is.na(x)]
    }

    # A malformed upload must show a banner, not kill the session.
    shiny::observeEvent(input$data_file, {
      df <- tryCatch(read_csv_upload(input$data_file), error = function(e) {
        run_error(.valid_warn(paste("Could not read the CSV:", conditionMessage(e))))
        NULL
      })
      if (is.null(df)) return()
      run_error(NULL)
      data_raw(df)
      audit(NULL)
    })
    shiny::observeEvent(input$load_demo, {
      data_raw(data.frame(
        text = c("cut taxes and deregulate", "fund public schools fully",
                 "shrink the government", "expand social programs",
                 "lower business rates", "protect workers' rights"),
        stringsAsFactors = FALSE))
      audit(NULL)
    })

    shiny::observeEvent(input$run_audit, {
      run_error(NULL)
      shiny::req(data_raw(), input$text_col)
      labs <- parse_labels()
      if (length(labs) < 2) { run_error(.valid_warn("Enter at least two labels.")); return() }
      if (!grepl("{text}", input$prompt, fixed = TRUE)) {
        run_error(.valid_warn("The prompt must contain {text}.")); return()
      }
      if (identical(shared$mode(), "live") && !shared$can_run()) {
        run_error(live_run_blocker_ui(shared$key())); return()
      }

      est <- make_estimator(input$estimand %||% "share",
                            target = input$target %||% labs[1],
                            other = input$other %||% labs[min(2, length(labs))])

      res <- safe_llmr_call({
        plan <- LLMRcontent::audit_plan(
          data = data_raw(), text = input$text_col,
          estimator = est, labels = labs, prompt = input$prompt)
        cfg <- build_llm_config(shared$provider(), shared$model(), temperature = 0)
        plan <- LLMRcontent::audit_add_models(plan, stats::setNames(list(cfg), shared$model()))
        if (isTRUE(input$add_paraphrase)) {
          plan <- LLMRcontent::audit_add_prompts(plan,
            paraphrase = paste0("Which label fits best: {labels}?\n\nText: {text}\n\nAnswer:"))
        }
        plan <- LLMRcontent::audit_add_perturbations(plan,
          label_order = input$orders %||% "as_given",
          temperature = parse_temps())
        runner <- build_runner(shared$mode(), valid_demo_responder(labs))
        LLMRcontent::audit_run(plan, .runner = runner)
      }, shared$provider())

      if (!res$ok) { run_error(res$ui); return() }
      audit(res$value)
      # Usage accounting must never crash a successful run; count defensively.
      n_calls <- tryCatch(nrow(LLMRcontent::audit_units(res$value)),
                          error = function(e) NA_integer_)
      shared$add_usage(list(calls = n_calls))
    })

    output$results <- shiny::renderUI({
      if (is.null(audit())) return(NULL)
      shiny::tagList(
        shiny::tags$h5("Stability"),
        shiny::verbatimTextOutput(ns("stability")),
        shiny::tags$h5("Fragility"),
        shiny::verbatimTextOutput(ns("fragility")),
        shiny::tags$h5("Specification curve"),
        shiny::plotOutput(ns("curve"), height = 300),
        shiny::tags$h5("Report"),
        shiny::verbatimTextOutput(ns("report"))
      )
    })

    output$stability <- shiny::renderPrint({ shiny::req(audit()); print(LLMRcontent::audit_stability(audit())) })
    output$fragility <- shiny::renderPrint({ shiny::req(audit()); print(LLMRcontent::audit_fragility(audit())) })
    output$curve <- shiny::renderPlot({
      shiny::req(audit())
      LLMRcontent::audit_curve(audit(), plot = TRUE)
    })
    output$report <- shiny::renderText({ shiny::req(audit()); report_text(LLMRcontent::audit_report(audit())) })
  })
}

.valid_warn <- function(msg) {
  bslib::card(class = "border-warning", bslib::card_body(msg))
}
