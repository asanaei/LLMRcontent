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

valid_progress_runner <- function(runner) {
  force(runner)
  function(experiments, ...) {
    n <- NROW(experiments)
    out <- vector("list", n)
    for (i in seq_len(n)) {
      out[[i]] <- runner(experiments[i, , drop = FALSE], ...)
      shiny::incProgress(
        1 / n,
        detail = sprintf("%d of %d calls completed", i, n)
      )
    }
    do.call(rbind, out)
  }
}

mod_valid_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("module_ui"))
}

mod_valid_server <- function(id, shared, active = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    data_raw <- shiny::reactiveVal(NULL)
    audit    <- shiny::reactiveVal(NULL)
    run_error <- shiny::reactiveVal(NULL)

    warn_user <- function(message) {
      run_error(.valid_warn(message))
      shiny::showNotification(message, type = "warning", session = session)
      invisible(FALSE)
    }

    output$module_ui <- shiny::renderUI({
      if (!pkg_available("LLMRcontent")) return(install_guidance_ui("LLMRcontent"))
      bslib::card(
        bslib::card_header("Robustness audit"),
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

    planned_calls <- shiny::reactive({
      if (is.null(data_raw())) return(0L)
      prompts <- 1L + as.integer(isTRUE(input$add_paraphrase))
      orders <- unique(c("as_given", input$orders %||% character()))
      temperatures <- unique(c(0, parse_temps()))
      as.integer(
        NROW(data_raw()) *
          length(orders) *
          length(temperatures) *
          prompts
      )
    })

    shiny::observe({
      if (!is.null(active) && !identical(active(), "valid")) return()
      calls <- if (identical(shared$mode(), "live")) planned_calls() else 0L
      shared$set_plan(
        calls,
        sprintf(
          paste0(
            "Robustness audit; %d expected audit-unit rows; retries excluded; ",
            "Live runs above %d calls require confirmation"
          ),
          calls, .content_large_run_threshold
        )
      )
    })

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

    run_audit <- function(confirmed = FALSE) {
      run_error(NULL)
      if (is.null(data_raw())) {
        warn_user("Choose a data CSV or load the demo data before running the audit.")
        return()
      }
      text_col <- input$text_col %||% ""
      if (!nzchar(text_col) || !text_col %in% names(data_raw())) {
        warn_user("Select a text column before running the audit.")
        return()
      }
      labs <- parse_labels()
      if (length(labs) < 2) {
        warn_user("Enter at least two labels.")
        return()
      }
      if (!grepl("{text}", input$prompt %||% "", fixed = TRUE)) {
        warn_user("The prompt must contain {text}.")
        return()
      }
      if (!length(input$orders %||% character())) {
        warn_user("Select at least one label order.")
        return()
      }
      if (!length(parse_temps())) {
        warn_user("Enter at least one numeric temperature.")
        return()
      }
      if (identical(shared$mode(), "live") &&
          !nzchar(trimws(shared$model() %||% ""))) {
        warn_user("Enter a model in the sidebar before running the audit.")
        return()
      }
      if (identical(shared$mode(), "live") && !shared$can_run()) {
        run_error(live_run_blocker_ui(shared$key()))
        shiny::showNotification(
          "Set the provider API key before running the audit.",
          type = "warning",
          session = session
        )
        return()
      }

      est <- make_estimator(input$estimand %||% "share",
                            target = input$target %||% labs[1],
                            other = input$other %||% labs[min(2, length(labs))])
      planned <- planned_calls()
      if (identical(shared$mode(), "live")) {
        shared$set_plan(
          planned,
          sprintf(
            paste0(
              "Robustness audit; %d expected audit-unit rows; retries excluded; ",
              "Live runs above %d calls require confirmation"
            ),
            planned, .content_large_run_threshold
          )
        )
        if (!isTRUE(confirmed) &&
            planned > .content_large_run_threshold) {
          .content_large_run_modal(
            ns,
            "confirm_audit",
            "The robustness audit",
            planned,
            sprintf("%d expected audit-unit rows", planned)
          )
          return()
        }
      }
      runner <- build_runner(shared$mode(), valid_demo_responder(labs))
      if (identical(shared$mode(), "live")) {
        runner <- valid_progress_runner(runner)
      }
      model_name <- if (identical(shared$mode(), "demo")) {
        "Deterministic demo"
      } else {
        shared$model()
      }

      res <- safe_llmr_call(
        shiny::withProgress(message = "Running robustness audit", value = 0, {
          shiny::setProgress(
            value = 0,
            detail = if (identical(shared$mode(), "live")) {
              sprintf("0 of %d calls completed", planned)
            } else {
              "Running the deterministic demo"
            }
          )
          plan <- LLMRcontent::audit_plan(
            data = data_raw(), text = text_col,
            estimator = est, labels = labs, prompt = input$prompt)
          cfg <- build_llm_config(shared$provider(), shared$model(), temperature = 0)
          plan <- LLMRcontent::audit_add_models(
            plan,
            stats::setNames(list(cfg), model_name)
          )
          if (isTRUE(input$add_paraphrase)) {
            plan <- LLMRcontent::audit_add_prompts(
              plan,
              paraphrase = paste0(
                "Which label fits best: {labels}?\n\n",
                "Text: {text}\n\nAnswer:"
              )
            )
          }
          plan <- LLMRcontent::audit_add_perturbations(
            plan,
            label_order = input$orders,
            temperature = parse_temps()
          )
          out <- LLMRcontent::audit_run(plan, .runner = runner)
          if (identical(shared$mode(), "demo")) {
            shiny::incProgress(1, detail = "Demo audit completed")
          }
          out
        }),
        shared$provider()
      )

      if (!res$ok) { run_error(res$ui); return() }
      out <- if (identical(shared$mode(), "demo")) {
        annotate_demo_result(res$value)
      } else {
        res$value
      }
      audit(out)
      # Usage accounting must never crash a successful run; count defensively.
      n_calls <- tryCatch(nrow(LLMRcontent::audit_units(out)),
                          error = function(e) NA_integer_)
      if (identical(shared$mode(), "demo")) {
        shared$add_usage(list(result_rows = n_calls))
      } else {
        shared$add_usage(list(calls = n_calls))
      }
    }

    shiny::observeEvent(input$run_audit, {
      run_audit()
    })

    shiny::observeEvent(input$confirm_audit, {
      shiny::removeModal()
      run_audit(confirmed = TRUE)
    })

    output$results <- shiny::renderUI({
      shiny::validate(
        shiny::need(!is.null(data_raw()),
                    "Choose a data CSV or load the demo data to prepare an audit."),
        shiny::need(!is.null(audit()),
                    "Run the audit to see stability and fragility results.")
      )
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
    output$report <- shiny::renderText({ shiny::req(audit()); report_text(LLMR::report(audit())) })
  })
}

.valid_warn <- function(msg) {
  bslib::card(class = "border-warning", bslib::card_body(msg))
}
