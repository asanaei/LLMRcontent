# gui_app_ui.R -----------------------------------------------------------------
# The app shell: a navbar with the shared LLMR.shiny sidebar (provider, model,
# mode, key and usage tiles) and one nav panel per LLMRcontent workflow -- coding,
# robustness audit, and archive. The silicon-respondent packages (LLMRpanel,
# FocusGroup) have their own GUIs.

.content_gui_ui <- function() {
  bslib::page_navbar(
    title = "LLMRcontent",
    id = "main_nav",
    selected = "home",
    fillable = TRUE,
    theme = LLMR.shiny::llmr_theme("content"),
    sidebar = LLMR.shiny::shell_sidebar(),
    bslib::nav_panel(
      "Home",
      value = "home",
      mod_landing_ui("landing")
    ),
    bslib::nav_panel(
      "Coding",
      value = "coder",
      mod_coder_ui("coder")
    ),
    bslib::nav_panel(
      "Robustness audit",
      value = "valid",
      mod_valid_ui("valid")
    ),
    bslib::nav_panel(
      "Archive",
      value = "archive",
      mod_archive_ui("archive")
    )
  )
}
