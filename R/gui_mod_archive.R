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

mod_archive_server <- function(id, shared) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    log_path  <- shiny::reactiveVal(NULL)
    archive   <- shiny::reactiveVal(NULL)
    sealed    <- shiny::reactiveVal(NULL)
    run_error <- shiny::reactiveVal(NULL)

    output$module_ui <- shiny::renderUI({
      if (!pkg_available("LLMRcontent")) return(install_guidance_ui("LLMRcontent"))
      # archive_build() reads the log through LLMR::llm_log_read(); preflight LLMR
      # too so a missing core shows guidance instead of crashing on use.
      if (!pkg_available("LLMR")) return(install_guidance_ui("LLMR"))
      bslib::card(
        bslib::card_header("Verifiable replication archive"),
        bslib::card_body(
          shiny::uiOutput(ns("run_error")),
          shiny::fluidRow(
            shiny::column(6, shiny::fileInput(ns("log_file"), "LLMR audit log (.jsonl)", accept = ".jsonl")),
            shiny::column(6, shiny::actionButton(ns("load_demo"), "Use demo log"))
          ),
          shiny::actionButton(ns("build"), "Build archive", class = "btn-primary"),
          shiny::tags$hr(),
          shiny::uiOutput(ns("results"))
        )
      )
    })

    output$run_error <- shiny::renderUI(run_error())

    shiny::observeEvent(input$log_file, { log_path(input$log_file$datapath); archive(NULL); sealed(NULL) })
    shiny::observeEvent(input$load_demo, { log_path(archive_demo_log()); archive(NULL); sealed(NULL) })

    shiny::observeEvent(input$build, {
      run_error(NULL)
      if (is.null(log_path())) { run_error(.arch_warn("Choose a log file or use the demo log.")); return() }
      res <- safe_llmr_call(LLMRcontent::archive_build(log_path()), shared$provider())
      if (!res$ok) { run_error(res$ui); return() }
      archive(res$value); sealed(NULL)
    })

    shiny::observeEvent(input$seal, {
      shiny::req(archive())
      res <- safe_llmr_call(LLMRcontent::archive_seal(archive()), shared$provider())
      if (!res$ok) { run_error(res$ui); return() }
      sealed(res$value)
    })

    output$results <- shiny::renderUI({
      if (is.null(archive())) return(NULL)
      shiny::tagList(
        shiny::verbatimTextOutput(ns("summary")),
        shiny::actionButton(ns("seal"), "Seal archive", class = "btn-primary"),
        shiny::uiOutput(ns("seal_status")),
        shiny::tags$h5("Manifest"),
        DT::DTOutput(ns("manifest")),
        shiny::tags$h5("Integrity check"),
        shiny::verbatimTextOutput(ns("check")),
        shiny::tags$h5("Verifiability horizon"),
        shiny::verbatimTextOutput(ns("horizon")),
        shiny::tags$h5("Offline replay"),
        shiny::verbatimTextOutput(ns("replay"))
      )
    })

    output$summary <- shiny::renderPrint({ shiny::req(archive()); print(archive()) })

    output$seal_status <- shiny::renderUI({
      if (is.null(sealed())) return(NULL)
      shiny::tags$p(class = "text-success", "Sealed under a root hash.")
    })

    output$manifest <- DT::renderDT({
      shiny::req(archive())
      DT::datatable(as_display_table(archive()$manifest),
                    options = list(scrollX = TRUE, pageLength = 5))
    })

    # Archive inspection prints; a malformed archive shows the error in the
    # panel rather than crashing the output.
    safe_print <- function(expr) {
      tryCatch(print(expr),
               error = function(e) cat("Could not compute:", conditionMessage(e), "\n"))
    }

    output$check <- shiny::renderPrint({
      shiny::req(archive())
      a <- sealed() %||% archive()
      safe_print(LLMRcontent::archive_check(a))
    })

    output$horizon <- shiny::renderPrint({
      shiny::req(archive())
      safe_print(LLMRcontent::verifiability_horizon(archive()))
    })

    output$replay <- shiny::renderPrint({
      shiny::req(archive())
      safe_print(LLMRcontent::archive_replay(archive()))
    })
  })
}

.arch_warn <- function(msg) {
  bslib::card(class = "border-warning", bslib::card_body(msg))
}
