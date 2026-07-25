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

.content_audit_units_table <- function(value) {
  units <- as.data.frame(LLMRcontent::audit_units(value))
  plan <- value$plan
  if (!is.null(plan$data) && !is.null(plan$text) &&
      plan$text %in% names(plan$data) && "unit_id" %in% names(units)) {
    units$text <- as.character(plan$data[[plan$text]][units$unit_id])
  }
  units
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
    placebo  <- shiny::reactiveVal(NULL)
    audit_timing <- shiny::reactiveVal(NULL)
    run_error <- shiny::reactiveVal(NULL)

    warn_user <- function(message) {
      run_error(.valid_warn(message))
      shiny::showNotification(message, type = "warning", session = session)
      invisible(FALSE)
    }

    output$module_ui <- shiny::renderUI({
      if (!pkg_available("LLMRcontent")) return(install_guidance_ui("LLMRcontent"))
      if (!pkg_available("LLMR")) return(install_guidance_ui("LLMR"))
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
            shiny::column(6, shiny::checkboxGroupInput(
              ns("orders"),
              shiny::tagList(
                "Label order ",
                help_tip(
                  "The order arm tests whether rearranging the displayed labels changes the result."
                )
              ),
              choices = c("as given" = "as_given", "reversed" = "reversed"),
              selected = c("as_given", "reversed")
            )),
            shiny::column(
              6,
              shiny::textInput(
                ns("temps"),
                shiny::tagList(
                  "Temperatures (comma-separated) ",
                  help_tip(
                    "Each listed temperature defines a sampling arm in the audit grid."
                  )
                ),
                value = "0, 0.7",
                placeholder = "For example: 0, 0.7"
              )
            )
          ),
          shiny::checkboxInput(ns("add_paraphrase"),
            "Add a prompt paraphrase to the grid", value = TRUE),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::numericInput(
                ns("placebo_reps"),
                "Label-permutation placebo repetitions",
                value = 200, min = 1, step = 1
              )
            ),
            shiny::column(
              6,
              shiny::numericInput(
                ns("placebo_seed"),
                "Placebo seed",
                value = 110, min = 1, step = 1
              )
            )
          ),
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
                         choices = column_names_for_mapping(df),
                         selected = column_names_for_mapping(df)[[1]])
    })

    output$target_ui <- shiny::renderUI({
      labs <- parse_labels()
      if (!length(labs)) {
        return(shiny::tags$p(
          class = "text-muted",
          "Enter at least two labels to choose the estimand labels."
        ))
      }
      kind <- input$estimand %||% "share"
      ui <- list(shiny::selectInput(
        ns("target"), "Target label", choices = labs,
        selected = labs[[1]]
      ))
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
      temperatures <- unique(c(
        0,
        shared$temperature() %||% numeric(),
        parse_temps()
      ))
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
      placebo(NULL)
      audit_timing(NULL)
    })
    shiny::observeEvent(input$load_demo, {
      data_raw(data.frame(
        text = c("cut taxes and deregulate", "fund public schools fully",
                 "shrink the government", "expand social programs",
                 "lower business rates", "protect workers' rights"),
        stringsAsFactors = FALSE))
      audit(NULL)
      placebo(NULL)
      audit_timing(NULL)
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
      placebo_reps <- suppressWarnings(
        as.integer(input$placebo_reps %||% 200L)
      )
      placebo_seed <- suppressWarnings(
        as.integer(input$placebo_seed %||% 110L)
      )
      if (is.na(placebo_reps) || placebo_reps < 1L) {
        warn_user("Enter a positive whole number of placebo repetitions.")
        return()
      }
      if (is.na(placebo_seed)) {
        warn_user("Enter a whole-number placebo seed.")
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
      call_batches <- list()
      timed_runner <- function(experiments, ...) {
        result <- runner(experiments, ...)
        call_batches[[length(call_batches) + 1L]] <<- result
        result
      }
      started <- proc.time()[["elapsed"]]
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
          cfg <- build_llm_config(
            shared$provider(),
            shared$model(),
            temperature = shared$temperature(),
            max_tokens = shared$max_tokens(),
            reasoning_effort = shared$reasoning_effort()
          )
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
            temperature = unique(c(
              shared$temperature() %||% numeric(),
              parse_temps()
            ))
          )
          out <- LLMRcontent::audit_run(plan, .runner = timed_runner)
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
      audit_timing(finish_call_timing(call_batches, started))
      set.seed(placebo_seed)
      placebo_result <- safe_llmr_call(
        LLMRcontent::audit_placebo(
          out,
          type = "label_permutation",
          reps = placebo_reps
        ),
        shared$provider()
      )
      if (placebo_result$ok) {
        placebo(placebo_result$value)
      } else {
        placebo(NULL)
        run_error(placebo_result$ui)
      }
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
        shiny::uiOutput(ns("audit_status")),
        shiny::tags$h5("Diagnostics"),
        DT::DTOutput(ns("diagnostics")),
        shiny::tags$h5("Stability"),
        DT::DTOutput(ns("stability_table")),
        shiny::tags$h5("Fragility"),
        DT::DTOutput(ns("fragility_table")),
        shiny::tags$h5("Audit cells"),
        DT::DTOutput(ns("audit_cells_table")),
        shiny::tags$h5("Audited units"),
        DT::DTOutput(ns("audit_units_table")),
        shiny::tags$h5("Label-permutation placebo"),
        shiny::uiOutput(ns("placebo_status")),
        DT::DTOutput(ns("placebo_table")),
        shiny::tags$h5("Specification curve"),
        shiny::uiOutput(ns("curve_ui")),
        shiny::uiOutput(ns("audit_timing_ui")),
        shiny::tags$h5("Report"),
        text_block_output(ns("report"), height = "20rem"),
        bslib::accordion(
          bslib::accordion_panel(
            "Technical details",
            shiny::tags$h5("Audit console summary"),
            text_block_output(ns("audit_raw"), height = "10rem"),
            shiny::tags$h5("Stability console output"),
            text_block_output(ns("stability"), height = "12rem"),
            shiny::tags$h5("Fragility console output"),
            text_block_output(ns("fragility"), height = "10rem"),
            shiny::tags$h5("Placebo console output"),
            text_block_output(ns("placebo_raw"), height = "16rem")
          ),
          open = FALSE
        )
      )
    })

    output$audit_status <- shiny::renderUI({
      value <- audit()
      if (is.null(value)) return(NULL)
      stability <- LLMRcontent::audit_stability(value)
      fragility <- LLMRcontent::audit_fragility(value)
      if (!identical(stability$status, "ok")) {
        return(shiny::tags$p(
          class = "text-warning",
          paste("The audit summary status is", stability$status, ".")
        ))
      }
      fragility_text <- if (identical(fragility$status, "reference_failed")) {
        "The reference estimate failed, so fragility is undefined."
      } else if (is.infinite(fragility$fragility)) {
        "No cell in this grid changes the reference sign."
      } else {
        sprintf(
          "Changing %d grid dimensions is sufficient to change the reference sign.",
          fragility$fragility
        )
      }
      shiny::tags$p(
        class = "fw-semibold",
        sprintf(
          paste0(
            "Across %d cells, estimates range from %.3f to %.3f and ",
            "%.0f%% share the reference sign. %s"
          ),
          stability$n_cells,
          stability$min,
          stability$max,
          100 * stability$sign_agreement,
          fragility_text
        )
      )
    })

    output$diagnostics <- DT::renderDT({
      shiny::req(audit())
      .content_datatable(
        diagnostics_table(audit()),
        digits = 3,
        caption = "Combined audit diagnostics",
        rownames = FALSE,
        options = list(scrollX = TRUE, dom = "t")
      )
    })

    output$stability_table <- DT::renderDT({
      shiny::req(audit())
      .content_datatable(
        LLMRcontent::audit_stability(audit()),
        digits = 3,
        rownames = FALSE,
        options = list(scrollX = TRUE, dom = "t")
      )
    })

    output$fragility_table <- DT::renderDT({
      shiny::req(audit())
      value <- LLMRcontent::audit_fragility(audit())
      table <- data.frame(
        fragility = if (is.infinite(value$fragility)) {
          "Inf"
        } else {
          as.character(value$fragility)
        },
        status = value$status,
        reference_cell = value$reference,
        reference_estimate = value$reference_estimate,
        flipping_cells = paste(value$flipping_cells, collapse = ", "),
        stringsAsFactors = FALSE
      )
      .content_datatable(
        table,
        digits = 3,
        rownames = FALSE,
        options = list(dom = "t")
      )
    })

    output$audit_cells_table <- DT::renderDT({
      shiny::req(audit())
      .content_datatable(
        tibble::as_tibble(audit()),
        digits = 3,
        caption = "Estimate and diagnostics for each audit cell",
        rownames = FALSE,
        options = list(pageLength = 8)
      )
    })

    output$audit_units_table <- DT::renderDT({
      shiny::req(audit())
      .content_datatable(
        .content_audit_units_table(audit()),
        text = "text",
        digits = 3,
        caption = "Unit-level text and labels used by the audit cells",
        rownames = FALSE,
        options = list(pageLength = 8)
      )
    })

    output$placebo_status <- shiny::renderUI({
      value <- placebo()
      if (is.null(value)) return(NULL)
      degenerate <- sum(value$cells$degenerate %in% TRUE)
      if (degenerate > 0L) {
        return(shiny::tags$p(
          class = "text-body-secondary",
          sprintf(
            paste0(
              "%d of %d cells are permutation-invariant because this ",
              "estimand uses only the label marginal; the placebo is ",
              "uninformative for those cells."
            ),
            degenerate,
            nrow(value$cells)
          )
        ))
      }
      shiny::tags$p(
        class = "text-body-secondary",
        sprintf(
          "Computed %d label permutations for each of %d audit cells.",
          value$reps,
          nrow(value$cells)
        )
      )
    })

    output$placebo_table <- DT::renderDT({
      shiny::req(placebo())
      .content_datatable(
        tibble::as_tibble(placebo()),
        digits = 3,
        rownames = FALSE,
        options = list(scrollX = TRUE, pageLength = 8)
      )
    })

    output$curve_ui <- shiny::renderUI({
      if (!pkg_available("ggplot2")) return(install_guidance_ui("ggplot2"))
      shiny::plotOutput(ns("curve"), height = 300)
    })

    output$curve <- shiny::renderPlot({
      shiny::req(audit(), pkg_available("ggplot2"))
      LLMRcontent::audit_curve(audit(), plot = TRUE)
    })

    output$audit_timing_ui <- shiny::renderUI({
      timing <- audit_timing()
      if (is.null(timing)) return(NULL)
      shiny::tagList(
        shiny::tags$p(
          class = "text-body-secondary",
          timing_summary_text(timing)
        ),
        if (!pkg_available("ggplot2")) {
          install_guidance_ui("ggplot2")
        } else {
          shiny::plotOutput(ns("audit_timing_plot"), height = 260)
        }
      )
    })

    output$audit_timing_plot <- shiny::renderPlot({
      timing <- timing_by_unit(audit_timing())
      shiny::req(timing, pkg_available("ggplot2"))
      ggplot2::ggplot(
        timing,
        ggplot2::aes(
          x = timing$unit_id,
          y = timing$duration_seconds
        )
      ) +
        ggplot2::geom_col(fill = "#6C757D") +
        ggplot2::labs(
          title = "Recorded call time by audited unit",
          x = "Unit",
          y = "Seconds"
        ) +
        ggplot2::theme_minimal()
    })

    output$report <- shiny::renderText({
      shiny::req(audit())
      report_text(audit())
    })
    output$audit_raw <- shiny::renderPrint({
      shiny::req(audit())
      print(audit())
    })
    output$stability <- shiny::renderPrint({
      shiny::req(audit())
      print(LLMRcontent::audit_stability(audit()))
    })
    output$fragility <- shiny::renderPrint({
      shiny::req(audit())
      print(LLMRcontent::audit_fragility(audit()))
    })
    output$placebo_raw <- shiny::renderPrint({
      shiny::req(placebo())
      print(placebo())
    })
  })
}

.valid_warn <- function(msg) {
  bslib::card(class = "border-warning", bslib::card_body(msg))
}
