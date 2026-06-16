# gui_app_server.R -------------------------------------------------------------
# Top-level server: LLMR.shiny::shell_context() wires the sidebar (provider/model/
# mode sync, key + cost tiles, usage tracking) and returns the shared reactive
# bundle the workflow modules consume.

.content_gui_server <- function(input, output, session) {
  shared <- LLMR.shiny::shell_context(input, output, session)

  mod_landing_server("landing", function(target) {
    bslib::nav_select("main_nav", selected = target, session = session)
  })

  mod_coder_server("coder", shared)
  mod_valid_server("valid", shared)
  mod_archive_server("archive", shared)
}
