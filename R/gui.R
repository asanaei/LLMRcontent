# gui.R ------------------------------------------------------------------------
# An optional Shiny GUI for LLMRcontent, launched with run_content_studio(). It
# fronts the three LLMRcontent workflows -- coding, robustness audit, and archive
# -- as tabs over the shared LLMR.shiny substrate (provider/model sidebar, key
# and cost tiles, offline demo mode).
#
# shiny, bslib, DT, ggplot2, and LLMR.shiny are Suggests, not Imports: a non-GUI user
# installs none of them and the analysis package stays lean. Every call into
# those packages is fully qualified (or forwarded through the lazy helpers in
# gui_aliases.R), and the launcher guards on all four.

#' Launch the LLMRcontent Shiny GUI
#'
#' A point-and-click front end for the three LLMRcontent workflows: coding
#' (codebook, sealed gold set, protocol tuning, locked validation, corpus
#' coding, gold correction), measurement-robustness audits, and replication
#' archives, each as a tab. The app wraps the package API rather than
#' reimplementing it: the package defines behavior, the GUI defines
#' presentation.
#'
#' The GUI is optional. It needs the suggested packages shiny, bslib, DT,
#' ggplot2, and LLMR.shiny. Install them with
#' `install.packages(c("shiny", "bslib", "DT", "ggplot2", "LLMR.shiny"))`.
#' Live runs read provider API keys from environment variables only, never
#' pasted into the app; a deterministic demo mode runs offline.
#'
#' @param ... Passed to [shiny::runApp()] (e.g. `port`, `launch.browser`).
#' @return Invisibly, the value of [shiny::runApp()]; called for the side effect
#'   of starting the app.
#' @examples
#' if (interactive() &&
#'     all(vapply(c("shiny", "bslib", "DT", "ggplot2", "LLMR.shiny"),
#'                requireNamespace, logical(1), quietly = TRUE))) {
#'   run_content_studio()
#' }
#' @export
run_content_studio <- function(...) {
  .content_gui_require()
  app <- shiny::shinyApp(ui = .content_gui_ui(), server = .content_gui_server)
  invisible(shiny::runApp(app, ...))
}

.content_gui_require <- function() {
  need <- c("shiny", "bslib", "DT", "ggplot2", "LLMR.shiny")
  missing <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop("The LLMRcontent GUI needs these packages: ",
         paste(missing, collapse = ", "),
         ". Install them with install.packages(c(",
         paste(sprintf("\"%s\"", missing), collapse = ", "),
         ")), then retry.",
         call. = FALSE)
  }
  invisible(TRUE)
}
