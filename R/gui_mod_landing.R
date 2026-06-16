# mod_landing.R ----------------------------------------------------------------
# The home cards: one per package, greyed out and labeled "Install needed" when
# the package is absent, otherwise an "Open" button that selects its nav panel.

mod_landing_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("cards"))
}

mod_landing_server <- function(id, on_open) {
  shiny::moduleServer(id, function(input, output, session) {
    output$cards <- shiny::renderUI({
      workflows <- list(
        list(
          key = "coder",
          package = "LLMRcontent",
          title = "Coding",
          purpose = "Build, tune, validate, and apply LLM coding protocols."
        ),
        list(
          key = "valid",
          package = "LLMRcontent",
          title = "Robustness audit",
          purpose = "Audit whether a measured conclusion survives reasonable measurement choices."
        ),
        list(
          key = "archive",
          package = "LLMRcontent",
          title = "Archive",
          purpose = "Build, seal, inspect, and replay a verifiable record of LLM calls."
        )
      )

      bslib::layout_column_wrap(
        width = "280px",
        lapply(workflows, function(w) {
          available <- LLMR.shiny::pkg_available(w$package)
          bslib::card(
            class = if (available) "" else "bg-light text-muted",
            bslib::card_header(w$title),
            bslib::card_body(
              shiny::tags$p(w$purpose),
              if (!available) shiny::tags$p("Install needed."),
              shiny::actionButton(
                session$ns(paste0("open_", w$key)),
                "Open",
                class = if (available) "btn-primary" else "btn-outline-secondary"
              )
            )
          )
        })
      )
    })

    shiny::observeEvent(input$open_coder, on_open("coder"))
    shiny::observeEvent(input$open_valid, on_open("valid"))
    shiny::observeEvent(input$open_archive, on_open("archive"))
  })
}
