# gui_app_server.R -------------------------------------------------------------
# Top-level server: LLMR.shiny::shell_context() wires the sidebar (provider/model/
# mode sync, key and usage tiles, usage tracking) and returns the shared reactive
# bundle the workflow modules consume.

.content_gui_server <- function(input, output, session) {
  shared <- LLMR.shiny::shell_context(input, output, session)
  artifacts <- shiny::reactiveValues(
    coded = NULL,
    coded_calls = NULL,
    archive = NULL
  )
  active_nav <- shiny::reactive(input$main_nav)

  mod_landing_server("landing", function(target) {
    bslib::nav_select("main_nav", selected = target, session = session)
  })

  coder_state <- mod_coder_server("coder", shared, active_nav, artifacts)
  mod_valid_server("valid", shared, active_nav)
  mod_archive_server(
    "archive",
    shared,
    artifacts,
    coded = coder_state$coded,
    coded_calls = coder_state$coded_calls
  )

  shiny::observe({
    nav <- input$main_nav
    if (is.null(nav) || nav %in% c("home", "archive")) {
      shared$set_plan(0L)
    }
  })
}
