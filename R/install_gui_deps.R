# install_gui_deps.R -----------------------------------------------------------
# A one-call installer for the optional GUI stack, so users do not have to read
# the launcher's "install these first" message and assemble the call by hand.

#' Install the LLMRcontent GUI's suggested packages
#'
#' The Shiny GUI ([run_content_studio()]) needs four suggested packages: shiny,
#' bslib, DT, and LLMR.shiny. This installs the ones that are missing. The first
#' three come from CRAN; LLMR.shiny is the family's shared GUI substrate and may
#' not be on CRAN yet, in which case this points you to its GitHub source rather
#' than failing.
#'
#' @param upgrade If `FALSE` (default), packages already installed are left
#'   alone; if `TRUE`, CRAN packages are reinstalled to their latest versions.
#' @param quiet If `TRUE`, suppress the per-package progress messages.
#' @return Invisibly, a named logical vector: `TRUE` where the package is
#'   available after the call, `FALSE` where it could not be installed (e.g.
#'   LLMR.shiny when it is not on CRAN and not already installed).
#' @examples
#' if (interactive()) {
#'   install_gui_deps()
#' }
#' @export
install_gui_deps <- function(upgrade = FALSE, quiet = FALSE) {
  say <- function(...) if (!quiet) message(...)
  have <- function(p) requireNamespace(p, quietly = TRUE)

  cran_pkgs <- c("shiny", "bslib", "DT")
  status <- stats::setNames(logical(0), character(0))

  for (p in cran_pkgs) {
    if (have(p) && !upgrade) {
      say(sprintf("%s: already installed.", p))
      status[p] <- TRUE
      next
    }
    say(sprintf("%s: installing from CRAN ...", p))
    try(utils::install.packages(p), silent = TRUE)
    status[p] <- have(p)
  }

  # LLMR.shiny: install from CRAN if available there; otherwise do not hard-fail
  # -- tell the user where to get it. Once it is on CRAN this path just works.
  if (have("LLMR.shiny") && !upgrade) {
    say("LLMR.shiny: already installed.")
    status["LLMR.shiny"] <- TRUE
  } else {
    say("LLMR.shiny: attempting CRAN install ...")
    suppressWarnings(try(utils::install.packages("LLMR.shiny"), silent = TRUE))
    if (have("LLMR.shiny")) {
      status["LLMR.shiny"] <- TRUE
    } else {
      status["LLMR.shiny"] <- FALSE
      say("LLMR.shiny is not available from your repositories yet. ",
          "Install it from GitHub with:\n",
          "    remotes::install_github(\"asanaei/LLMR.shiny\")")
    }
  }

  if (all(status)) {
    say("All GUI packages are available. Launch the GUI with run_content_studio().")
  } else {
    missing <- names(status)[!status]
    say("Still missing: ", paste(missing, collapse = ", "),
        ". See the note above for LLMR.shiny.")
  }
  invisible(status)
}
