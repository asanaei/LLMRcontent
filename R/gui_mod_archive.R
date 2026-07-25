# mod_archive.R ----------------------------------------------------------------
# The LLMRcontent archive workflow: turn an LLMR audit log into a content-addressed
# archive, seal it under a root hash, check integrity, and inspect the manifest
# and verifiability horizon. Replay is demonstrated on the archived responses.
# This module reads a log file rather than making model calls, so it runs the
# same online or offline.

# Substrate helpers are lazy forwarders in gui_aliases.R (see mod_coder).

# A tiny two-record JSONL log, so the workflow has something to chew on offline.
archive_demo_log <- function() {
  path <- tempfile(fileext = ".jsonl")
  writeLines(c(
    paste0('{"ts":"2026-06-01T10:00:01+0000","schema_version":"1.0",',
           '"kind":"call","provider":"groq","model":"openai/gpt-oss-20b",',
           '"model_version":"openai/gpt-oss-20b-2026-06-01","status":200,',
           '"request":{"messages":[{"role":"user","content":"Label: positive?"}],',
           '"temperature":0},"usage":{"sent":5,"rec":1},',
           '"response_id":"r-1","text":"positive"}'),
    paste0('{"ts":"2026-06-01T10:00:02+0000","schema_version":"1.0",',
           '"kind":"call","provider":"openai","model":"gpt-4o-mini",',
           '"model_version":"gpt-4o-mini-2026-06-01","status":200,',
           '"request":{"messages":[{"role":"user","content":"Label: negative?"}],',
           '"temperature":0},"usage":{"sent":6,"rec":1},',
           '"response_id":"r-2","text":"negative"}')
  ), path)
  path
}

mod_archive_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("module_ui"))
}

mod_archive_server <- function(id, shared, artifacts = NULL,
                               coded = NULL, coded_calls = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    if (is.null(artifacts)) {
      artifacts <- shiny::reactiveValues(
        coded = NULL,
        coded_calls = NULL,
        archive = NULL
      )
    }
    if (is.null(coded)) coded <- shiny::reactive(artifacts$coded)
    if (is.null(coded_calls)) {
      coded_calls <- shiny::reactive(artifacts$coded_calls)
    }
    ns <- session$ns
    log_path       <- shiny::reactiveVal(NULL)
    uploaded_calls <- shiny::reactiveVal(NULL)
    archive        <- shiny::reactiveVal(NULL)
    sealed         <- shiny::reactiveVal(NULL)
    redacted       <- shiny::reactiveVal(NULL)
    run_error      <- shiny::reactiveVal(NULL)

    warn_user <- function(message) {
      run_error(.arch_warn(message))
      shiny::showNotification(message, type = "warning", session = session)
      invisible(FALSE)
    }

    current_archive <- shiny::reactive({
      redacted() %||% sealed() %||% archive()
    })

    reset_archive <- function(path) {
      log_path(path)
      archive(NULL)
      sealed(NULL)
      redacted(NULL)
      artifacts$archive <- NULL
    }

    carried_results <- shiny::reactive({
      calls <- coded_calls()
      if (is.data.frame(calls) && "response_id" %in% names(calls)) {
        return(calls)
      }
      value <- coded()
      if (is.null(value)) return(NULL)
      data <- tryCatch(
        as.data.frame(tibble::as_tibble(value)),
        error = function(e) NULL
      )
      if (is.data.frame(data) && "response_id" %in% names(data)) data else NULL
    })

    check_results <- shiny::reactive({
      if (isTRUE(input$use_coded_calls) && !is.null(carried_results())) {
        return(carried_results())
      }
      uploaded_calls()
    })

    check_result <- shiny::reactive({
      value <- current_archive()
      if (is.null(value)) return(NULL)
      tryCatch(
        LLMRcontent::archive_check(value, results = check_results()),
        error = function(e) e
      )
    })

    horizon_result <- shiny::reactive({
      value <- current_archive()
      if (is.null(value)) return(NULL)
      tryCatch(verifiability_horizon(value), error = function(e) e)
    })

    replay_result <- shiny::reactive({
      value <- current_archive()
      if (is.null(value)) return(NULL)
      tryCatch(LLMRcontent::archive_replay(value), error = function(e) e)
    })

    output$module_ui <- shiny::renderUI({
      if (!pkg_available("LLMRcontent")) return(install_guidance_ui("LLMRcontent"))
      # archive_build() reads the log through LLMR::llm_log_read(); preflight LLMR
      # too so a missing core shows guidance instead of crashing on use.
      if (!pkg_available("LLMR")) return(install_guidance_ui("LLMR"))
      bslib::card(
        bslib::card_header("Archive"),
        bslib::card_body(
          shiny::uiOutput(ns("run_error")),
          shiny::tags$h4("Verifiable replication archive"),
          shiny::tags$p(
            paste(
              "Choose an LLM audit log or use the bundled demo log.",
              "Building the archive reads recorded calls and does not make API calls."
            )
          ),
          shiny::fluidRow(
            shiny::column(6, shiny::fileInput(ns("log_file"), "LLM Audit Log", accept = ".jsonl")),
            shiny::column(6, shiny::actionButton(ns("load_demo"), "Use demo log"))
          ),
          shiny::tags$p(
            class = "form-text",
            "The audit log is a JSON Lines (.jsonl) file recorded by LLMR."
          ),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::fileInput(
                ns("results_file"),
                "Optional results CSV",
                accept = ".csv",
                placeholder = "CSV containing response_id"
              )
            ),
            shiny::column(6, shiny::uiOutput(ns("carry_ui")))
          ),
          shiny::tags$p(
            class = "form-text",
            "A results CSV must contain response_id for completeness checking."
          ),
          shiny::uiOutput(ns("build_action")),
          shiny::tags$hr(),
          shiny::uiOutput(ns("results"))
        )
      )
    })

    output$run_error <- shiny::renderUI(run_error())

    output$build_action <- shiny::renderUI({
      reason <- if (is.null(log_path())) {
        "Archive building is disabled until a log file is selected."
      } else {
        NULL
      }
      .content_action_control(
        ns("build"),
        "Build archive",
        reason = reason
      )
    })

    output$carry_ui <- shiny::renderUI({
      if (!is.null(carried_results())) {
        return(shiny::checkboxInput(
          ns("use_coded_calls"),
          "Use call records from the coded corpus held in Coding",
          value = TRUE
        ))
      }
      if (is.null(coded())) return(NULL)
      bslib::card(
        class = "border-info",
        bslib::card_body(
          paste(
            "A coded corpus is held in Coding, but no associated response_id",
            "records are available for a completeness check."
          )
        )
      )
    })

    shiny::observeEvent(input$log_file, {
      reset_archive(input$log_file$datapath)
    })
    shiny::observeEvent(input$load_demo, {
      reset_archive(archive_demo_log())
    })

    shiny::observeEvent(input$results_file, {
      result <- tryCatch(
        read_csv_upload(input$results_file),
        error = function(e) {
          warn_user(paste("Could not read the results CSV:", conditionMessage(e)))
          NULL
        }
      )
      if (is.null(result)) return()
      if (!"response_id" %in% names(result)) {
        uploaded_calls(NULL)
        warn_user("The results CSV must contain a response_id column.")
        return()
      }
      uploaded_calls(result)
      run_error(NULL)
    })

    shiny::observeEvent(input$build, {
      run_error(NULL)
      if (is.null(log_path())) {
        warn_user("Choose a log file or use the demo log before building the archive.")
        return()
      }
      res <- safe_llmr_call(
        shiny::withProgress(message = "Building archive", value = 0, {
          shiny::setProgress(value = 0, detail = "Reading the audit log")
          out <- LLMRcontent::archive_build(log_path())
          shiny::incProgress(1, detail = "Archive built")
          out
        }),
        shared$provider()
      )
      if (!res$ok) { run_error(res$ui); return() }
      archive(res$value)
      sealed(NULL)
      redacted(NULL)
      artifacts$archive <- res$value
    })

    shiny::observeEvent(input$seal, {
      shiny::req(current_archive())
      res <- safe_llmr_call(
        LLMRcontent::archive_seal(current_archive()),
        shared$provider()
      )
      if (!res$ok) { run_error(res$ui); return() }
      sealed(res$value)
      redacted(NULL)
      artifacts$archive <- res$value
    })

    shiny::observeEvent(input$redact, {
      shiny::req(current_archive())
      res <- safe_llmr_call(
        LLMRcontent::archive_redact(current_archive()),
        shared$provider()
      )
      if (!res$ok) { run_error(res$ui); return() }
      redacted(res$value)
      artifacts$archive <- res$value
    })

    output$results <- shiny::renderUI({
      shiny::validate(
        shiny::need(
          !is.null(archive()),
          "Choose a log file or use the demo log, then build the archive."
        )
      )
      shiny::tagList(
        shiny::uiOutput(ns("archive_status")),
        DT::DTOutput(ns("diagnostics")),
        shiny::uiOutput(ns("archive_actions")),
        shiny::tags$h5("Manifest"),
        DT::DTOutput(ns("manifest")),
        shiny::tags$h5("Integrity check"),
        shiny::uiOutput(ns("check_status")),
        DT::DTOutput(ns("check_table")),
        shiny::tags$h5("Verifiability horizon"),
        DT::DTOutput(ns("horizon_table")),
        shiny::tags$h5("Offline replay"),
        shiny::uiOutput(ns("replay_status")),
        shiny::uiOutput(ns("archive_timing_ui")),
        shiny::tags$h5("Archive report"),
        text_block_output(ns("report"), height = "20rem"),
        bslib::accordion(
          bslib::accordion_panel(
            "Technical details",
            shiny::tags$h5("Archive console summary"),
            text_block_output(ns("summary"), height = "10rem"),
            shiny::tags$h5("Integrity console summary"),
            text_block_output(ns("check"), height = "10rem"),
            shiny::tags$h5("Horizon console summary"),
            text_block_output(ns("horizon"), height = "12rem"),
            shiny::tags$h5("Replay console summary"),
            text_block_output(ns("replay"), height = "10rem")
          ),
          open = FALSE
        )
      )
    })

    output$archive_status <- shiny::renderUI({
      value <- current_archive()
      if (is.null(value)) return(NULL)
      state <- if (isTRUE(value$redacted)) {
        "SEALED and REDACTED"
      } else if (isTRUE(value$sealed)) {
        "SEALED"
      } else {
        "UNSEALED"
      }
      shiny::tags$p(
        class = "fw-semibold",
        sprintf(
          "%s archive with %d recorded calls.",
          state,
          nrow(value$manifest)
        )
      )
    })

    output$diagnostics <- DT::renderDT({
      value <- current_archive()
      shiny::req(value)
      .content_datatable(
        diagnostics_table(value),
        digits = 3,
        caption = "Archive diagnostics",
        rownames = FALSE,
        options = list(scrollX = TRUE, dom = "t")
      )
    })

    output$archive_actions <- shiny::renderUI({
      value <- current_archive()
      if (is.null(value)) return(NULL)
      shiny::tags$div(
        class = "d-flex flex-wrap align-items-center gap-2 mb-3",
        if (!isTRUE(value$sealed)) {
          shiny::actionButton(ns("seal"), "Seal archive", class = "btn-primary")
        },
        if (!isTRUE(value$sealed)) {
          help_tip(
            "Sealing binds the ordered records and environment metadata to one root hash."
          )
        },
        if (isTRUE(value$sealed) && !isTRUE(value$redacted)) {
          shiny::actionButton(
            ns("redact"), "Redact content", class = "btn-outline-secondary"
          )
        },
        if (isTRUE(value$sealed) && !isTRUE(value$redacted)) {
          help_tip(
            "Redaction removes request and response text while retaining a verifiable public hash tree."
          )
        },
        if (isTRUE(value$sealed)) {
          shiny::tags$span(
            class = "text-success",
            "Sealed under a root hash."
          )
        }
      )
    })

    output$manifest <- DT::renderDT({
      shiny::req(current_archive())
      .content_datatable(
        current_archive()$manifest,
        digits = 3,
        rownames = FALSE,
        options = list(pageLength = 5)
      )
    })

    output$check_status <- shiny::renderUI({
      result <- check_result()
      if (is.null(result)) return(NULL)
      if (inherits(result, "error")) {
        return(.arch_warn(paste("Could not check the archive:", conditionMessage(result))))
      }
      completeness <- if (!is.na(result$n_results)) {
        sprintf(
          " Completeness: %d of %d result rows matched.",
          result$n_matched,
          result$n_results
        )
      } else {
        " No results were supplied for completeness checking."
      }
      shiny::tags$p(
        class = if (isTRUE(result$intact)) "text-success" else "text-danger",
        paste0(
          if (isTRUE(result$intact)) {
            "Archive integrity is intact."
          } else {
            "Archive integrity is broken."
          },
          completeness
        )
      )
    })

    output$check_table <- DT::renderDT({
      result <- check_result()
      shiny::req(result, !inherits(result, "error"))
      table <- data.frame(
        records = result$n_records,
        records_ok = result$records_ok,
        root_ok = result$root_ok,
        public_root_ok = result$public_root_ok,
        intact = result$intact,
        results = result$n_results,
        matched = result$n_matched,
        stringsAsFactors = FALSE
      )
      .content_datatable(
        table,
        rownames = FALSE,
        options = list(dom = "t")
      )
    })

    output$horizon_table <- DT::renderDT({
      result <- horizon_result()
      shiny::req(result, !inherits(result, "error"))
      .content_datatable(
        result,
        digits = 3,
        rownames = FALSE,
        options = list(scrollX = TRUE, dom = "t")
      )
    })

    output$replay_status <- shiny::renderUI({
      result <- replay_result()
      if (is.null(result)) return(NULL)
      if (inherits(result, "error")) {
        return(shiny::tags$p(
          class = "text-body-secondary",
          paste("Offline replay unavailable:", conditionMessage(result))
        ))
      }
      shiny::tags$p(
        class = "text-success",
        sprintf(
          "Offline replay is available for %d records over %d distinct requests.",
          attr(result, "n_replayable") %||% 0L,
          attr(result, "n_keys") %||% 0L
        )
      )
    })

    output$archive_timing_ui <- shiny::renderUI({
      timing <- archive_timing(current_archive())
      if (is.null(timing)) return(NULL)
      shiny::tagList(
        shiny::tags$p(
          class = "text-body-secondary",
          sprintf(
            paste0(
              "Recorded call time %.2f seconds across %d calls; ",
              "median %.2f seconds per call."
            ),
            sum(timing$duration_seconds),
            nrow(timing),
            stats::median(timing$duration_seconds)
          )
        ),
        if (!pkg_available("ggplot2")) {
          install_guidance_ui("ggplot2")
        } else {
          shiny::plotOutput(ns("archive_timing_plot"), height = 260)
        }
      )
    })

    output$archive_timing_plot <- shiny::renderPlot({
      timing <- archive_timing(current_archive())
      shiny::req(timing, pkg_available("ggplot2"))
      ggplot2::ggplot(
        timing,
        ggplot2::aes(
          x = timing$call,
          y = timing$duration_seconds
        )
      ) +
        ggplot2::geom_col(fill = "#6C757D") +
        ggplot2::labs(
          title = "Recorded duration by archived call",
          x = "Call",
          y = "Seconds"
        ) +
        ggplot2::theme_minimal()
    })

    output$report <- shiny::renderText({
      shiny::req(current_archive())
      report_text(current_archive())
    })

    output$summary <- shiny::renderPrint({
      shiny::req(current_archive())
      print(current_archive())
    })

    output$check <- shiny::renderPrint({
      result <- check_result()
      shiny::req(result)
      if (inherits(result, "error")) {
        cat("Could not compute:", conditionMessage(result), "\n")
      } else {
        print(result)
      }
    })

    output$horizon <- shiny::renderPrint({
      result <- horizon_result()
      shiny::req(result)
      if (inherits(result, "error")) {
        cat("Could not compute:", conditionMessage(result), "\n")
      } else {
        print(result)
      }
    })

    output$replay <- shiny::renderPrint({
      result <- replay_result()
      shiny::req(result)
      if (inherits(result, "error")) {
        cat("Could not compute:", conditionMessage(result), "\n")
      } else {
        print(result)
      }
    })
  })
}

.arch_warn <- function(msg) {
  bslib::card(class = "border-warning", bslib::card_body(msg))
}
