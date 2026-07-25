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
guess_column             <- function(...) LLMR.shiny::guess_column(...)
report_text              <- function(...) LLMR.shiny::report_text(...)
diagnostics_table         <- function(...) LLMR.shiny::diagnostics_table(...)
text_block_output         <- function(...) LLMR.shiny::text_block_output(...)
help_tip                  <- function(...) LLMR.shiny::help_tip(...)

.content_action_control <- function(input_id, label, reason = NULL,
                                    class = "btn-primary") {
  shiny::tags$div(
    class = "d-flex flex-column align-items-start gap-1",
    shiny::actionButton(
      input_id,
      label,
      class = class,
      disabled = !is.null(reason)
    ),
    if (!is.null(reason)) {
      shiny::tags$p(class = "form-text mb-0", reason)
    }
  )
}

.content_id_columns <- function(data) {
  columns <- names(data)
  columns[
    grepl("(^id$|_id$|^id_|_hash$|^hash$)", columns, ignore.case = TRUE) |
      tolower(columns) %in% c("cell", "call", "record")
  ]
}

.content_text_columns <- function(data) {
  columns <- names(data)
  ids <- .content_id_columns(data)
  setdiff(
    columns[
      grepl(
        "(^|_)(text|content|message|prompt|definition|example|response)(_|$)",
        columns,
        ignore.case = TRUE
      )
    ],
    ids
  )
}

.content_prepare_table <- function(data, text = NULL, ids = NULL,
                                   digits = 3L) {
  data <- as.data.frame(
    as_display_table(data, digits = digits),
    check.names = FALSE
  )
  text <- intersect(text %||% .content_text_columns(data), names(data))
  ids <- setdiff(
    intersect(ids %||% .content_id_columns(data), names(data)),
    text
  )
  if (length(ids)) {
    data <- data[c(setdiff(names(data), ids), ids)]
  }
  list(data = data, text = text, ids = ids)
}

.content_datatable <- function(data, text = NULL, ids = NULL, digits = 3L,
                               options = list(), rownames = FALSE, ...) {
  prepared <- .content_prepare_table(
    data,
    text = text,
    ids = ids,
    digits = digits
  )
  text_targets <- match(prepared$text, names(prepared$data), nomatch = 0L)
  text_targets <- text_targets[text_targets > 0L] - 1L
  id_targets <- match(prepared$ids, names(prepared$data), nomatch = 0L)
  id_targets <- id_targets[id_targets > 0L] - 1L

  column_defs <- list()
  if (length(text_targets)) {
    column_defs[[length(column_defs) + 1L]] <- list(
      targets = text_targets,
      width = if (length(text_targets) == 1L) "60%" else "38%",
      render = DT::JS("$.fn.dataTable.render.text()"),
      createdCell = DT::JS(
        paste0(
          "function(td) {",
          "$(td).css({'white-space':'normal',",
          "'overflow-wrap':'break-word','word-break':'normal'});",
          "}"
        )
      )
    )
  }
  if (length(id_targets)) {
    column_defs[[length(column_defs) + 1L]] <- list(
      targets = id_targets,
      width = "9%"
    )
  }

  existing_defs <- options$columnDefs %||% list()
  options$columnDefs <- c(existing_defs, column_defs)
  options <- utils::modifyList(
    list(autoWidth = TRUE, scrollX = TRUE, pageLength = 5),
    options
  )

  DT::datatable(
    prepared$data,
    options = options,
    rownames = rownames,
    ...
  )
}

# Live runs above this planned API-call count require explicit confirmation.
.content_large_run_threshold <- 100L

.content_large_run_modal <- function(ns, confirm_id, task, calls, result_label) {
  shiny::showModal(shiny::modalDialog(
    title = "Confirm Large Live Run",
    shiny::tags$p(sprintf(
      "%s will make %d planned API calls and produce %s.",
      task, calls, result_label
    )),
    shiny::tags$p("Retries are excluded and may add API calls."),
    easyClose = TRUE,
    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(
        ns(confirm_id),
        sprintf("Run %d Calls", calls),
        class = "btn-primary"
      )
    )
  ))
}
