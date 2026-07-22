# gui_aliases.R ----------------------------------------------------------------
# The GUI modules call the LLMR.shiny substrate by short names. LLMR.shiny is a
# Suggests, so it must not be referenced at package load time; these helpers
# defer the `LLMR.shiny::` lookup until the GUI actually runs (which
# is exactly when the launcher's guard has confirmed LLMR.shiny is installed).
# This keeps the module call sites terse without an unconditional Suggests
# dependency at build/check time.

# The GUI modules use %||% from rlang (an Import, brought in via 00_package.R);
# no local redefinition, so the operator means the same thing package-wide.

pkg_available            <- function(package) requireNamespace(package, quietly = TRUE)
install_guidance_ui      <- function(...) LLMR.shiny::install_guidance_ui(...)
# safe_llmr_call() captures its first argument lazily with
# eval.parent(substitute(expr)); a `...` forwarder would re-home that
# expression in this file's namespace and every module-local reference in it
# would fail ("could not find function ..."). Forward by named promise so the
# expression keeps its caller's environment.
safe_llmr_call           <- function(expr, provider = NULL) {
  LLMR.shiny::safe_llmr_call(expr, provider)
}
live_run_blocker_ui      <- function(...) LLMR.shiny::live_run_blocker_ui(...)
build_runner             <- function(...) LLMR.shiny::build_runner(...)
build_llm_config         <- function(...) LLMR.shiny::build_llm_config(...)
annotate_demo_result     <- function(...) LLMR.shiny::annotate_demo_result(...)
is_demo_result           <- function(...) LLMR.shiny::is_demo_result(...)
demo_banner_ui           <- function(...) LLMR.shiny::demo_banner_ui(...)
extract_token_counts     <- function(...) LLMR.shiny::extract_token_counts(...)
as_display_table         <- function(...) LLMR.shiny::as_display_table(...)
read_csv_upload          <- function(...) LLMR.shiny::read_csv_upload(...)
read_csv_path            <- function(...) LLMR.shiny::read_csv_path(...)
column_names_for_mapping <- function(...) LLMR.shiny::column_names_for_mapping(...)
report_text              <- function(...) LLMR.shiny::report_text(...)
