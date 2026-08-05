# mod_coder.R ------------------------------------------------------------------
# The LLMRcontent coding workflow as a seven-step stepper: codebook, gold set, config,
# tune, validate, code corpus, download. Each step wraps the LLMRcontent coding API and
# routes failures through safe_llmr_call() so a missing key or package shows a
# banner rather than crashing the app.

# Substrate helpers (pkg_available, safe_llmr_call, build_*, read_csv_*, etc.)
# are defined as lazy forwarders in gui_aliases.R, so the call sites below stay
# short without referencing the LLMR.shiny Suggests at package load.

.content_archive_replay_available <- function(value) {
  inherits(value, "archive") && !isTRUE(value$redacted)
}

.content_gold_split_size <- function(gold, split) {
  if (is.null(gold)) return(0L)
  out <- tryCatch(
    LLMRcontent::gold_split(gold, split = split),
    error = function(e) NULL
  )
  if (is.null(out)) return(0L)
  NROW(out)
}

mod_coder_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("module_ui"))
}

mod_coder_server <- function(id, shared, active = NULL, artifacts = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    if (is.null(artifacts)) {
      artifacts <- shiny::reactiveValues(
        coded = NULL,
        coded_calls = NULL,
        archive = NULL
      )
    }
    archive_replay_available <- shiny::reactive({
      .content_archive_replay_available(artifacts$archive)
    })
    step <- shiny::reactiveVal(1L)

    categories <- shiny::reactiveVal(data.frame(
      label = c("policy", "community"),
      definition = c(
        "Mentions formal rules, rights, government, or public policy.",
        "Mentions family, friends, neighbors, groups, or community life."
      ),
      include = c("law\nrights\npublic programs", "family\nfriends\nneighbors"),
      exclude = c("", ""),
      examples = c("The city council changed the rule.", "My neighbors helped after the storm."),
      counterexamples = c("I cooked dinner.", "The tax agency changed a form."),
      stringsAsFactors = FALSE
    ))

    codebook <- shiny::reactiveVal(NULL)
    gold_raw <- shiny::reactiveVal(NULL)
    gold <- shiny::reactiveVal(NULL)
    protocols <- shiny::reactiveVal(NULL)
    tuning <- shiny::reactiveVal(NULL)
    locked_protocol <- shiny::reactiveVal(NULL)
    validation <- shiny::reactiveVal(NULL)
    corpus_raw <- shiny::reactiveVal(NULL)
    coded <- shiny::reactiveVal(NULL)
    corrected <- shiny::reactiveVal(NULL)
    tuning_timing <- shiny::reactiveVal(NULL)
    validation_timing <- shiny::reactiveVal(NULL)
    coding_timing <- shiny::reactiveVal(NULL)
    run_error <- shiny::reactiveVal(NULL)
    correction_warnings <- shiny::reactiveVal(character())

    reset_coded_state <- function() {
      coded(NULL)
      corrected(NULL)
      coding_timing(NULL)
      correction_warnings(character())
      artifacts$coded <- NULL
      artifacts$coded_calls <- NULL
    }

    reset_validation_state <- function() {
      validation(NULL)
      validation_timing(NULL)
      reset_coded_state()
    }

    reset_lock_state <- function() {
      locked_protocol(NULL)
      shiny::updateCheckboxInput(
        session,
        "confirm_ledger",
        value = FALSE
      )
      reset_validation_state()
    }

    reset_protocol_state <- function() {
      protocols(NULL)
      tuning(NULL)
      tuning_timing(NULL)
      reset_lock_state()
    }

    validation_matches_lock <- shiny::reactive({
      value <- validation()
      locked <- locked_protocol()
      !is.null(value) && !is.null(locked) &&
        identical(value$protocol_hash, locked$hash)
    })

    warn_user <- function(message) {
      run_error(
        bslib::card(
          class = "border-warning",
          bslib::card_body(message)
        )
      )
      shiny::showNotification(message, type = "warning", session = session)
      invisible(FALSE)
    }

    add_result_usage <- function(result, fallback_calls) {
      counts <- extract_token_counts(result, fallback_calls = fallback_calls)
      if (identical(shared$mode(), "demo")) {
        if (!is.null(counts$calls)) {
          counts$result_rows <- as.integer(counts$calls)
        }
        counts$calls <- NULL
        counts$sent <- 0L
        counts$received <- 0L
        counts$total <- 0L
      }
      shared$add_usage(counts)
    }

    plan_label <- function(task, calls) {
      sprintf(
        paste0(
          "%s; %d expected model responses; retries excluded; ",
          "Live runs above %d calls require confirmation"
        ),
        task, calls, .content_large_run_threshold
      )
    }

    output$module_ui <- shiny::renderUI({
      if (!pkg_available("LLMRcontent")) {
        return(install_guidance_ui("LLMRcontent", "LLMRcontent"))
      }

      bslib::card(
        bslib::card_header(shiny::uiOutput(session$ns("step_header"))),
        bslib::card_body(
          shiny::uiOutput(session$ns("run_error")),
          shiny::uiOutput(session$ns("step_body")),
          shiny::uiOutput(session$ns("step_action"))
        )
      )
    })

    output$step_header <- shiny::renderUI({
      labels <- c(
        "1 Codebook",
        "2 Gold",
        "3 Config",
        "4 Tune",
        "5 Validate",
        "6 Code",
        "7 Download"
      )
      current <- step()
      shiny::tags$div(
        shiny::tags$h4(class = "mb-2", "Coding"),
        shiny::tags$div(
          class = "d-flex flex-wrap gap-2",
          lapply(seq_along(labels), function(i) {
            shiny::tags$span(
              class = paste(
                "badge",
                if (i == current) "text-bg-primary" else if (i < current) "text-bg-success" else "text-bg-secondary"
              ),
              labels[[i]]
            )
          })
        )
      )
    })

    output$run_error <- shiny::renderUI({
      run_error()
    })

    output$corpus_run_context <- shiny::renderUI({
      replay_selected <- archive_replay_available() &&
        isTRUE(input$use_built_archive)
      shiny::tagList(
        if (identical(shared$mode(), "live") &&
            !shared$can_run() && !replay_selected) {
          live_run_blocker_ui(shared$key())
        },
        if (identical(shared$mode(), "demo") && !replay_selected) {
          demo_banner_ui()
        }
      )
    })

    output$corpus_run_action <- shiny::renderUI({
      replay_selected <- archive_replay_available() &&
        isTRUE(input$use_built_archive)
      text_col <- input$corpus_text_col %||% ""
      reason <- if (is.null(locked_protocol())) {
        "Corpus coding is disabled until a protocol is locked."
      } else if (!validation_matches_lock()) {
        "Corpus coding is disabled until the locked protocol is validated."
      } else if (is.null(corpus_raw())) {
        "Corpus coding is disabled until a corpus is loaded."
      } else if (!nzchar(text_col) ||
                 !text_col %in% names(corpus_raw())) {
        "Corpus coding is disabled until a text column is selected."
      } else if (identical(shared$mode(), "live") &&
                 !shared$can_run() && !replay_selected) {
        "Corpus coding is disabled until the provider API key is available."
      } else {
        NULL
      }
      .content_action_control(
        session$ns("run_code_corpus"),
        "Code corpus",
        class = "btn-primary",
        reason = reason
      )
    })

    output$step_body <- shiny::renderUI({
      coder_steps_ui(session$ns)
    })

    output$step_action <- shiny::renderUI({
      coder_step_action_ui(session$ns, step(), shared)
    })

    shiny::observeEvent(step(), {
      bslib::accordion_panel_set(
        "coding_steps",
        values = as.character(step()),
        session = session
      )
    }, ignoreInit = TRUE)

    output$protocol_context <- shiny::renderUI({
      key_ui <- if (identical(shared$mode(), "live") && !shared$can_run()) {
        live_run_blocker_ui(shared$key())
      } else {
        NULL
      }
      config_summary <- if (identical(shared$mode(), "demo")) {
        shiny::tags$p(
          class = "text-muted",
          "Bundled deterministic demo. No model, API key, or API calls."
        )
      } else {
        shiny::tags$dl(
          class = "row",
          shiny::tags$dt(class = "col-sm-2", "Provider"),
          shiny::tags$dd(class = "col-sm-10", shared$provider()),
          shiny::tags$dt(class = "col-sm-2", "Model"),
          shiny::tags$dd(class = "col-sm-10", shared$model()),
          shiny::tags$dt(class = "col-sm-2", "Mode"),
          shiny::tags$dd(class = "col-sm-10", "Live")
        )
      }
      shiny::tagList(key_ui, config_summary)
    })

    output$tune_context <- shiny::renderUI({
      shiny::tagList(
        if (identical(shared$mode(), "live") && !shared$can_run()) {
          live_run_blocker_ui(shared$key())
        },
        if (identical(shared$mode(), "demo")) demo_banner_ui()
      )
    })

    output$validate_context <- shiny::renderUI({
      shiny::tagList(
        if (identical(shared$mode(), "live") && !shared$can_run()) {
          live_run_blocker_ui(shared$key())
        },
        if (identical(shared$mode(), "demo")) demo_banner_ui()
      )
    })

    output$protocol_candidates_status <- shiny::renderUI({
      shiny::tags$p(
        paste0("Protocol candidates: ", length(protocols() %||% list()))
      )
    })

    output$archive_replay_choice <- shiny::renderUI({
      if (!archive_replay_available()) return(NULL)
      bslib::card(
        class = "border-info",
        bslib::card_body(
          shiny::checkboxInput(
            session$ns("use_built_archive"),
            shiny::tagList(
              "Use the archive held in the Archive tab for offline replay ",
              help_tip(
                paste(
                  "Replay succeeds only when the protocol configuration and",
                  "rendered requests match archived calls."
                )
              )
            ),
            value = FALSE
          )
        )
      )
    })

    collect_categories <- function() {
      df <- categories()
      for (i in seq_len(NROW(df))) {
        df$label[[i]] <- input[[paste0("cat_label_", i)]] %||% df$label[[i]]
        df$definition[[i]] <- input[[paste0("cat_definition_", i)]] %||% df$definition[[i]]
        df$include[[i]] <- input[[paste0("cat_include_", i)]] %||% df$include[[i]]
        df$exclude[[i]] <- input[[paste0("cat_exclude_", i)]] %||% df$exclude[[i]]
        df$examples[[i]] <- input[[paste0("cat_examples_", i)]] %||% df$examples[[i]]
        df$counterexamples[[i]] <- input[[paste0("cat_counterexamples_", i)]] %||% df$counterexamples[[i]]
      }
      df
    }

    split_lines <- function(x) {
      y <- unlist(strsplit(x %||% "", "\n", fixed = TRUE))
      y <- trimws(y)
      y[nzchar(y)]
    }

    # Read-only: collects the current inputs and builds the codebook object.
    # It must not write categories(): draft_codebook() calls it on every
    # keystroke, and a write would re-render the category editor and steal
    # focus. The reactiveVal is persisted in the save/add/remove observers.
    build_codebook <- function() {
      df <- collect_categories()
      labels <- trimws(df$label)
      if (sum(nzchar(labels)) < 2) {
        stop("At least two category labels are required.", call. = FALSE)
      }

      cat_objs <- lapply(seq_len(NROW(df)), function(i) {
        LLMRcontent::cb_category(
          label = df$label[[i]],
          definition = df$definition[[i]],
          include = split_lines(df$include[[i]]),
          exclude = split_lines(df$exclude[[i]]),
          examples = split_lines(df$examples[[i]]),
          counterexamples = split_lines(df$counterexamples[[i]])
        )
      })

      LLMRcontent::codebook(
        name = input$codebook_name %||% "Untitled codebook",
        unit = input$codebook_unit %||% "text unit",
        categories = cat_objs
      )
    }

    output$save_codebook_action <- shiny::renderUI({
      labels <- collect_categories()$label
      reason <- if (length(labels) < 2L || any(!nzchar(trimws(labels)))) {
        "Saving is disabled until every category has a label."
      } else if (anyDuplicated(labels)) {
        "Saving is disabled until category labels are unique."
      } else {
        NULL
      }
      .content_action_control(
        session$ns("save_codebook"),
        "Save and continue",
        reason = reason
      )
    })

    draft_codebook <- shiny::reactive({
      build_codebook()
    })

    output$category_editor <- shiny::renderUI({
      df <- categories()
      shiny::tagList(
        lapply(seq_len(NROW(df)), function(i) {
          bslib::card(
            bslib::card_header(paste("Category", i)),
            bslib::card_body(
              shiny::textInput(
                session$ns(paste0("cat_label_", i)), "Label",
                value = df$label[[i]], placeholder = "Short category label"
              ),
              shiny::textAreaInput(
                session$ns(paste0("cat_definition_", i)), "Definition",
                value = df$definition[[i]], rows = 2,
                placeholder = "State what belongs in this category."
              ),
              shiny::textAreaInput(
                session$ns(paste0("cat_include_", i)), "Include terms",
                value = df$include[[i]], rows = 2,
                placeholder = "Optional; enter one term per line."
              ),
              shiny::textAreaInput(
                session$ns(paste0("cat_exclude_", i)), "Exclude terms",
                value = df$exclude[[i]], rows = 2,
                placeholder = "Optional; enter one term per line."
              ),
              shiny::textAreaInput(
                session$ns(paste0("cat_examples_", i)), "Examples",
                value = df$examples[[i]], rows = 2,
                placeholder = "Optional; enter one example per line."
              ),
              shiny::textAreaInput(
                session$ns(paste0("cat_counterexamples_", i)),
                "Counterexamples",
                value = df$counterexamples[[i]], rows = 2,
                placeholder = "Optional; enter one counterexample per line."
              )
            )
          )
        }),
        shiny::fluidRow(
          shiny::column(4, shiny::actionButton(session$ns("add_category"), "Add category")),
          shiny::column(4, shiny::selectInput(session$ns("remove_category_index"), "Remove", choices = seq_len(NROW(df)))),
          shiny::column(4, shiny::actionButton(session$ns("remove_category"), "Remove selected"))
        )
      )
    })

    output$codebook_preview <- shiny::renderText({
      cb <- draft_codebook()
      LLMRcontent::format_codebook(cb)
    })

    output$codebook_hash <- shiny::renderText({
      cb <- draft_codebook()
      paste("Codebook hash:", LLMRcontent::codebook_hash(cb))
    })

    shiny::observeEvent(input$add_category, {
      df <- collect_categories()
      df <- rbind(
        df,
        data.frame(
          label = "",
          definition = "",
          include = "",
          exclude = "",
          examples = "",
          counterexamples = "",
          stringsAsFactors = FALSE
        )
      )
      categories(df)
    })

    shiny::observeEvent(input$remove_category, {
      df <- collect_categories()
      if (NROW(df) <= 2) {
        run_error(
          bslib::card(
            class = "border-warning",
            bslib::card_body("At least two categories are required.")
          )
        )
        return()
      }
      idx <- as.integer(input$remove_category_index %||% NROW(df))
      categories(df[-idx, , drop = FALSE])
    })

    shiny::observeEvent(input$save_codebook, {
      categories(collect_categories())
      res <- safe_llmr_call(build_codebook(), shared$provider())
      if (!res$ok) {
        run_error(res$ui)
        return()
      }
      run_error(NULL)
      reset_protocol_state()
      codebook(res$value)
      step(2L)
    })

    # A malformed upload must show a banner, not kill the session.
    read_upload_safely <- function(file) {
      tryCatch(read_csv_upload(file), error = function(e) {
        run_error(
          bslib::card(
            class = "border-warning",
            bslib::card_body(paste("Could not read the CSV:", conditionMessage(e)))
          )
        )
        NULL
      })
    }

    shiny::observeEvent(input$gold_file, {
      df <- read_upload_safely(input$gold_file)
      if (is.null(df)) return()
      run_error(NULL)
      gold_raw(df)
      gold(NULL)
      reset_protocol_state()
      step(2L)
    })

    shiny::observeEvent(input$load_demo_gold, {
      path <- system.file("extdata", "demo_gold.csv", package = "LLMRcontent")
      gold_raw(read_csv_path(path))
      gold(NULL)
      reset_protocol_state()
      step(2L)
    })

    output$gold_map_ui <- shiny::renderUI({
      df <- gold_raw()
      if (is.null(df)) {
        return(shiny::tags$p(
          class = "text-muted",
          "Upload a gold CSV or load the demo gold data to map its columns."
        ))
      }

      cols <- column_names_for_mapping(df)
      text_col <- guess_column(cols, "text")
      label_col <- guess_column(cols, "label", exclude = text_col)
      shiny::tagList(
        shiny::selectInput(
          session$ns("gold_text_col"),
          "Text column",
          choices = cols,
          selected = text_col
        ),
        shiny::selectInput(
          session$ns("gold_label_col"),
          "Label column",
          choices = cols,
          selected = label_col
        ),
        shiny::selectInput(
          session$ns("gold_id_col"),
          shiny::tagList(
            "Stable id column (optional) ",
            help_tip(paste(
              "An id shared with the corpus lets the correction link audited",
              "units even when texts repeat; without one, duplicate texts",
              "are refused rather than matched arbitrarily."))
          ),
          choices = c("(none)" = "", cols),
          selected = guess_column(cols, "id",
                                  exclude = c(text_col, label_col)) %||% ""
        )
      )
    })

    output$gold_preview <- DT::renderDT({
      df <- gold_raw()
      shiny::validate(
        shiny::need(
          !is.null(df),
          "Upload a gold CSV or load the demo gold data to preview it."
        )
      )
      .content_datatable(
        utils::head(df, 20),
        text = input$gold_text_col %||% names(df)[[1]],
        rownames = FALSE,
        options = list(pageLength = 5)
      )
    })

    gold_size_result <- shiny::reactive({
      shiny::req(pkg_available("LLMRcontent"))
      LLMRcontent::gold_size(
        expected_agreement = input$expected_agreement %||% 0.8,
        ci_width = input$ci_width %||% 0.1
      )
    })

    output$gold_size_status <- shiny::renderUI({
      result <- gold_size_result()
      holdout_frac <- 1 - (input$dev_split %||% 60) / 100
      total <- if (holdout_frac > 0) ceiling(result$recommended_size / holdout_frac) else NA
      shiny::tags$p(
        class = "fw-semibold",
        sprintf(
          paste(
            "Recommended size of the evaluation (holdout) split: %d",
            "human-coded units. With the current split, that means about %s",
            "gold units in total."),
          result$recommended_size,
          if (is.na(total)) "-" else format(total)
        )
      )
    })

    output$gold_size_table <- DT::renderDT({
      result <- gold_size_result()
      candidates <- as.data.frame(result$candidates)
      .content_datatable(
        candidates,
        digits = 3,
        caption = "Candidate gold-set sizes",
        rownames = FALSE,
        options = list(dom = "t", pageLength = nrow(candidates))
      )
    })

    output$gold_size_helper <- shiny::renderPrint({
      print(gold_size_result())
    })

    output$gold_status <- shiny::renderUI({
      if (is.null(gold())) return(NULL)

      bslib::card(
        class = "border-success",
        bslib::card_header("SEALED gold set"),
        bslib::card_body(
          "The gold set was created. The test split is reserved for validation."
        )
      )
    })

    shiny::observeEvent(input$create_gold, {
      if (is.null(gold_raw())) {
        warn_user("Upload a gold CSV or load the demo gold data before creating the gold set.")
        return()
      }
      text_col <- input$gold_text_col %||% ""
      label_col <- input$gold_label_col %||% ""
      if (!nzchar(text_col) || !text_col %in% names(gold_raw())) {
        warn_user("Select a text column before creating the gold set.")
        return()
      }
      if (!nzchar(label_col) || !label_col %in% names(gold_raw())) {
        warn_user("Select a label column before creating the gold set.")
        return()
      }
      # A cleared numericInput yields NA; fall back to the default seed.
      seed <- suppressWarnings(as.integer(input$gold_seed %||% 110L))
      if (is.na(seed)) seed <- 110L
      id_col <- input$gold_id_col %||% ""
      if (nzchar(id_col) && id_col %in% c(text_col, label_col)) {
        warn_user("The id column must differ from the text and label columns.")
        return()
      }
      if (nzchar(id_col) && !id_col %in% names(gold_raw())) {
        warn_user("The selected id column is not in the gold data.")
        return()
      }
      set.seed(seed)
      split <- c(dev = input$dev_split / 100, test = 1 - input$dev_split / 100)

      res <- safe_llmr_call(
        call_gold_set_mapped(
          gold_raw(),
          text_col,
          label_col,
          split = split,
          stratify = isTRUE(input$stratify_gold),
          seal_holdout = TRUE,
          id_col = if (nzchar(id_col)) id_col else NULL
        ),
        shared$provider()
      )

      if (!res$ok) {
        run_error(res$ui)
        return()
      }

      run_error(NULL)
      reset_protocol_state()
      gold(res$value)
    })

    shiny::observeEvent(input$continue_gold, {
      if (is.null(gold())) {
        warn_user("Create the sealed gold set before continuing.")
        return()
      }
      step(3L)
    })

    prompt_valid <- shiny::reactive({
      prompt <- input$prompt_template %||% ""
      grepl("{text}", prompt, fixed = TRUE) &&
        grepl("{codebook}", prompt, fixed = TRUE)
    })

    output$prompt_validation <- shiny::renderUI({
      if (prompt_valid()) {
        return(shiny::tags$p(class = "text-success", "Prompt template contains {text} and {codebook}."))
      }
      shiny::tags$p(class = "text-warning", "Prompt template must contain {text} and {codebook}.")
    })

    output$protocol_scale_preview <- shiny::renderUI({
      variants <- input$prompt_variants %||% character()
      replicates <- suppressWarnings(
        as.integer(input$protocol_replicates %||% 1L)
      )
      if (is.na(replicates)) replicates <- 1L
      calls <- length(variants) * replicates * dev_units()
      shiny::tags$p(
        class = "text-body-secondary",
        sprintf(
          paste0(
            "Tuning scale: %d development units x %d candidates x %d ",
            "replicates = %d expected model responses."
          ),
          dev_units(), length(variants), replicates, calls
        )
      )
    })

    shiny::observeEvent(input$build_protocols, {
      if (is.null(codebook())) {
        warn_user("Save a codebook before building protocols.")
        return()
      }
      if (is.null(gold())) {
        warn_user("Create the sealed gold set before building protocols.")
        return()
      }

      if (!prompt_valid()) {
        warn_user("Add both {text} and {codebook} to the prompt template.")
        return()
      }

      if (identical(shared$mode(), "live") &&
          !nzchar(trimws(shared$model() %||% ""))) {
        warn_user("Enter a model in the sidebar before building protocols.")
        return()
      }

      if (identical(shared$mode(), "live") && !shared$can_run()) {
        run_error(live_run_blocker_ui(shared$key()))
        shiny::showNotification(
          "Set the provider API key before building protocols.",
          type = "warning",
          session = session
        )
        return()
      }

      if (identical(shared$mode(), "live") && !pkg_available("LLMR")) {
        run_error(install_guidance_ui("LLMR", "LLMR"))
        return()
      }

      variants <- input$prompt_variants %||% "base"
      res <- safe_llmr_call({
        cfg <- build_llm_config(
          provider = shared$provider(),
          model = shared$model(),
          temperature = shared$temperature(),
          max_tokens = shared$max_tokens(),
          reasoning_effort = shared$reasoning_effort()
        )
        proto_list <- lapply(variants, function(v) {
          LLMRcontent::protocol(
            codebook = codebook(),
            config = cfg,
            prompt = prompt_variant(input$prompt_template, v),
            replicates = as.integer(input$protocol_replicates %||% 1L),
            label = paste0("candidate_", v)
          )
        })
        names(proto_list) <- paste0("candidate_", variants)
        proto_list
      }, shared$provider())

      if (!res$ok) { run_error(res$ui); return() }
      reset_protocol_state()
      protocols(res$value)
      run_error(NULL)
      step(4L)
    })

    dev_units <- shiny::reactive({
      .content_gold_split_size(gold(), "dev")
    })

    test_units <- shiny::reactive({
      .content_gold_split_size(gold(), "test")
    })

    output$create_gold_action <- shiny::renderUI({
      text_col <- input$gold_text_col %||% ""
      label_col <- input$gold_label_col %||% ""
      dev_split <- suppressWarnings(
        as.numeric(input$dev_split %||% 60)
      )
      reason <- if (is.null(gold_raw())) {
        "Gold-set creation is disabled until gold data are loaded."
      } else if (NROW(gold_raw()) < 2L) {
        "Gold-set creation is disabled until at least two rows are loaded."
      } else if (!nzchar(text_col) ||
                 !text_col %in% names(gold_raw())) {
        "Gold-set creation is disabled until a text column is selected."
      } else if (!nzchar(label_col) ||
                 !label_col %in% names(gold_raw())) {
        "Gold-set creation is disabled until a label column is selected."
      } else if (identical(text_col, label_col)) {
        "Gold-set creation is disabled until different text and label columns are selected."
      } else if (anyNA(gold_raw()[[label_col]])) {
        "Gold-set creation is disabled until missing labels are resolved."
      } else if (length(dev_split) != 1L || !is.finite(dev_split) ||
                 dev_split <= 0 || dev_split >= 100) {
        "Gold-set creation is disabled until the development split is between 0 and 100 percent."
      } else {
        NULL
      }
      .content_action_control(
        session$ns("create_gold"),
        "Create sealed gold",
        reason = reason
      )
    })

    output$continue_gold_action <- shiny::renderUI({
      reason <- if (is.null(gold())) {
        "Continue is disabled until the sealed gold set is created."
      } else {
        NULL
      }
      .content_action_control(
        session$ns("continue_gold"),
        "Continue",
        reason = reason,
        class = "btn-outline-primary"
      )
    })

    output$build_protocols_action <- shiny::renderUI({
      replicates <- suppressWarnings(
        as.integer(input$protocol_replicates %||% NA_integer_)
      )
      reason <- if (is.null(codebook())) {
        "Protocol building is disabled until the codebook is saved."
      } else if (is.null(gold())) {
        "Protocol building is disabled until the sealed gold set is created."
      } else if (!prompt_valid()) {
        "Protocol building is disabled until the prompt contains {text} and {codebook}."
      } else if (!length(input$prompt_variants %||% character())) {
        "Protocol building is disabled until a prompt candidate is selected."
      } else if (is.na(replicates) || replicates < 1L) {
        "Protocol building is disabled until a positive replicate count is entered."
      } else if (identical(shared$mode(), "live") &&
                 !nzchar(trimws(shared$model() %||% ""))) {
        "Protocol building is disabled until a model is entered."
      } else if (identical(shared$mode(), "live") &&
                 !shared$can_run()) {
        "Protocol building is disabled until the provider API key is available."
      } else {
        NULL
      }
      .content_action_control(
        session$ns("build_protocols"),
        "Build protocols",
        reason = reason
      )
    })

    output$run_tune_action <- shiny::renderUI({
      reason <- if (is.null(protocols()) || !length(protocols())) {
        "Tuning is disabled until protocol candidates are built."
      } else if (is.null(gold()) || dev_units() < 1L) {
        "Tuning is disabled until the gold set has development units."
      } else if (identical(shared$mode(), "live") &&
                 !shared$can_run()) {
        "Tuning is disabled until the provider API key is available."
      } else {
        NULL
      }
      .content_action_control(
        session$ns("run_tune"),
        "Run tuning",
        reason = reason
      )
    })

    output$continue_tune_action <- shiny::renderUI({
      winner <- input$winner_protocol %||% ""
      reason <- if (is.null(tuning())) {
        "Continue is disabled until tuning is complete."
      } else if (is.null(protocols()) ||
                 !winner %in% names(protocols())) {
        "Continue is disabled until a winning protocol is selected."
      } else {
        NULL
      }
      .content_action_control(
        session$ns("continue_tune"),
        "Continue to lock",
        reason = reason,
        class = "btn-outline-primary"
      )
    })

    output$lock_protocol_action <- shiny::renderUI({
      winner <- input$winner_protocol %||% ""
      reason <- if (is.null(protocols()) || !length(protocols())) {
        "Locking is disabled until protocol candidates are built."
      } else if (!winner %in% names(protocols())) {
        "Locking is disabled until a protocol is selected."
      } else {
        NULL
      }
      .content_action_control(
        session$ns("lock_protocol"),
        "Lock selected protocol",
        reason = reason,
        class = "btn-outline-primary"
      )
    })

    output$run_validate_action <- shiny::renderUI({
      reason <- if (is.null(locked_protocol())) {
        "Validation is disabled until a protocol is locked."
      } else if (is.null(gold())) {
        "Validation is disabled until the sealed gold set is created."
      } else if (test_units() < 1L) {
        "Validation is disabled because the sealed gold set has no test units."
      } else if (!isTRUE(input$confirm_ledger)) {
        "Validation is disabled until the ledger confirmation is checked."
      } else if (identical(shared$mode(), "live") &&
                 !shared$can_run()) {
        "Validation is disabled until the provider API key is available."
      } else {
        NULL
      }
      .content_action_control(
        session$ns("run_validate"),
        "Validate locked protocol",
        reason = reason
      )
    })

    output$continue_validate_action <- shiny::renderUI({
      reason <- if (is.null(validation())) {
        "Continue is disabled until validation is complete."
      } else if (!validation_matches_lock()) {
        "Continue is disabled until the current locked protocol is validated."
      } else {
        NULL
      }
      .content_action_control(
        session$ns("continue_validate"),
        "Continue to corpus coding",
        reason = reason,
        class = "btn-outline-primary"
      )
    })

    output$continue_corpus_action <- shiny::renderUI({
      reason <- if (is.null(coded())) {
        "Continue is disabled until the corpus is coded."
      } else {
        NULL
      }
      .content_action_control(
        session$ns("continue_corpus"),
        "Continue to downloads",
        reason = reason,
        class = "btn-outline-primary"
      )
    })

    run_tune <- function(confirmed = FALSE) {
      if (is.null(protocols())) {
        warn_user("Build the protocol candidates before running tuning.")
        return()
      }
      if (is.null(gold())) {
        warn_user("Create the sealed gold set before running tuning.")
        return()
      }

      if (identical(shared$mode(), "live") && !shared$can_run()) {
        run_error(live_run_blocker_ui(shared$key()))
        shiny::showNotification(
          "Set the provider API key before running tuning.",
          type = "warning",
          session = session
        )
        return()
      }

      reps <- sum(vapply(protocols(), `[[`, integer(1), "replicates"))
      planned <- reps * dev_units()
      if (identical(shared$mode(), "live")) {
        shared$set_plan(planned, plan_label("Tuning", planned))
        if (!isTRUE(confirmed) &&
            planned > .content_large_run_threshold) {
          .content_large_run_modal(
            session$ns,
            "confirm_tune",
            "Protocol tuning",
            planned,
            sprintf("%d expected model responses", planned)
          )
          return()
        }
      }
      runner <- build_runner(shared$mode(), coder_demo_responder(codebook()))
      call_batches <- list()
      timed_runner <- function(experiments, ...) {
        result <- runner(experiments, ...)
        call_batches[[length(call_batches) + 1L]] <<- result
        result
      }
      started <- proc.time()[["elapsed"]]

      res <- safe_llmr_call(
        shiny::withProgress(message = "Tuning protocols", value = 0, {
          shiny::incProgress(0.2)
          out <- LLMRcontent::tune_protocol(
            protocols = protocols(),
            gold = gold(),
            split = "dev",
            .runner = timed_runner
          )
          shiny::incProgress(0.8)
          out
        }),
        shared$provider()
      )

      if (!res$ok) {
        run_error(res$ui)
        return()
      }

      out <- if (identical(shared$mode(), "demo")) annotate_demo_result(res$value) else res$value
      reset_lock_state()
      tuning(out)
      tuning_timing(finish_call_timing(call_batches, started))
      add_result_usage(tibble::as_tibble(out), fallback_calls = planned)
      run_error(NULL)
    }

    shiny::observeEvent(input$run_tune, {
      run_tune()
    })

    shiny::observeEvent(input$confirm_tune, {
      shiny::removeModal()
      run_tune(confirmed = TRUE)
    })

    output$tune_table <- DT::renderDT({
      shiny::validate(
        shiny::need(!is.null(tuning()), "Run tuning to compare the protocol candidates.")
      )
      .content_datatable(
        tibble::as_tibble(tuning()),
        digits = 3,
        caption = "Development-split performance used for protocol tuning",
        rownames = FALSE,
        options = list(scrollX = TRUE, pageLength = 5)
      )
    })

    output$tune_timing_status <- shiny::renderUI({
      text <- timing_summary_text(tuning_timing())
      if (is.null(text)) return(NULL)
      shiny::tags$p(class = "text-body-secondary", text)
    })

    output$tune_timing_ui <- shiny::renderUI({
      if (is.null(tuning_timing())) return(NULL)
      shiny::tagList(
        shiny::uiOutput(session$ns("tune_timing_status")),
        if (!pkg_available("ggplot2")) {
          install_guidance_ui("ggplot2")
        } else {
          shiny::plotOutput(session$ns("tune_timing_plot"), height = 260)
        }
      )
    })

    output$tune_timing_plot <- shiny::renderPlot({
      timing <- timing_by_unit(tuning_timing())
      shiny::req(timing, pkg_available("ggplot2"))
      ggplot2::ggplot(
        timing,
        ggplot2::aes(
          x = timing$unit_id,
          y = timing$duration_seconds
        )
      ) +
        ggplot2::geom_col(fill = "#6C757D") +
        ggplot2::labs(
          title = "Recorded call time by development unit",
          x = "Unit",
          y = "Seconds"
        ) +
        ggplot2::theme_minimal()
    })

    output$winner_ui <- shiny::renderUI({
      shiny::req(protocols())
      shiny::selectInput(
        session$ns("winner_protocol"),
        "Winner",
        choices = names(protocols()),
        selected = names(protocols())[[1]]
      )
    })

    shiny::observeEvent(input$continue_tune, {
      if (is.null(tuning())) {
        warn_user("Run tuning before continuing to protocol lock.")
        return()
      }
      if (is.null(input$winner_protocol) || !nzchar(input$winner_protocol)) {
        warn_user("Select a protocol before continuing.")
        return()
      }
      step(5L)
    })

    shiny::observeEvent(input$lock_protocol, {
      if (is.null(protocols())) {
        warn_user("Build protocol candidates before locking one.")
        return()
      }
      if (is.null(input$winner_protocol) ||
          !input$winner_protocol %in% names(protocols())) {
        warn_user("Select a protocol before locking it.")
        return()
      }
      selected <- protocols()[[input$winner_protocol]]
      res <- safe_llmr_call(LLMRcontent::protocol_lock(selected), shared$provider())

      if (!res$ok) {
        run_error(res$ui)
        return()
      }

      reset_validation_state()
      shiny::updateCheckboxInput(
        session,
        "confirm_ledger",
        value = FALSE
      )
      locked_protocol(res$value)
      run_error(NULL)
    })

    output$lock_status <- shiny::renderUI({
      shiny::req(locked_protocol())
      hash <- locked_protocol()$hash %||% "hash unavailable"
      label <- locked_protocol()$label %||% "Selected protocol"
      shiny::tags$p(
        class = "text-success fw-semibold",
        sprintf(
          "Protocol %s is locked. Short hash: %s.",
          label,
          substr(hash, 1L, 12L)
        )
      )
    })

    output$ledger_table <- DT::renderDT({
      shiny::req(gold())
      ledger <- LLMRcontent::gold_ledger(gold())
      .content_datatable(
        ledger,
        digits = 3,
        rownames = FALSE,
        options = list(pageLength = 5)
      )
    })

    run_validate <- function(confirmed = FALSE) {
      if (is.null(locked_protocol())) {
        warn_user("Lock a protocol before validating it.")
        return()
      }
      if (is.null(gold())) {
        warn_user("Create the sealed gold set before validating the protocol.")
        return()
      }

      if (!isTRUE(input$confirm_ledger)) {
        warn_user("Confirm that validation is ledgered before running.")
        return()
      }

      if (identical(shared$mode(), "live") && !shared$can_run()) {
        run_error(live_run_blocker_ui(shared$key()))
        shiny::showNotification(
          "Set the provider API key before validating the protocol.",
          type = "warning",
          session = session
        )
        return()
      }

      planned <- locked_protocol()$replicates * test_units()
      if (identical(shared$mode(), "live")) {
        shared$set_plan(planned, plan_label("Validation", planned))
        if (!isTRUE(confirmed) &&
            planned > .content_large_run_threshold) {
          .content_large_run_modal(
            session$ns,
            "confirm_validate",
            "Protocol validation",
            planned,
            sprintf("%d expected model responses", planned)
          )
          return()
        }
      }
      runner <- build_runner(shared$mode(), coder_demo_responder(codebook()))
      call_batches <- list()
      timed_runner <- function(experiments, ...) {
        result <- runner(experiments, ...)
        call_batches[[length(call_batches) + 1L]] <<- result
        result
      }
      started <- proc.time()[["elapsed"]]

      res <- safe_llmr_call(
        shiny::withProgress(message = "Validating locked protocol", value = 0, {
          shiny::incProgress(0.2)
          out <- LLMRcontent::validate_protocol(
            protocol = locked_protocol(),
            gold = gold(),
            split = "test",
            .runner = timed_runner
          )
          shiny::incProgress(0.8)
          out
        }),
        shared$provider()
      )

      if (!res$ok) {
        run_error(res$ui)
        return()
      }

      out <- if (identical(shared$mode(), "demo")) annotate_demo_result(res$value) else res$value
      validation(out)
      validation_timing(finish_call_timing(call_batches, started))
      add_result_usage(out, fallback_calls = planned)
      run_error(NULL)
    }

    shiny::observeEvent(input$run_validate, {
      run_validate()
    })

    shiny::observeEvent(input$confirm_validate, {
      shiny::removeModal()
      run_validate(confirmed = TRUE)
    })

    output$validation_table <- DT::renderDT({
      shiny::validate(
        shiny::need(
          !is.null(validation()),
          "Validate the locked protocol to see validation results."
        )
      )
      .content_datatable(
        diagnostics_table(validation()),
        digits = 3,
        caption = "Holdout validation summary",
        rownames = FALSE,
        options = list(scrollX = TRUE, dom = "t")
      )
    })

    output$validation_status <- shiny::renderUI({
      value <- validation()
      if (is.null(value)) return(NULL)
      shiny::tags$p(
        class = "fw-semibold",
        sprintf(
          paste0(
            "Holdout accuracy %.3f (95%% CI %.3f to %.3f); ",
            "macro-F1 %.3f; %d parse failures."
          ),
          value$accuracy,
          value$acc_lo,
          value$acc_hi,
          value$macro_f1,
          value$parse_failures
        )
      )
    })

    output$validation_categories <- DT::renderDT({
      value <- validation()
      shiny::req(value)
      .content_datatable(
        value$per_category,
        digits = 3,
        caption = "Validation by category",
        rownames = FALSE,
        options = list(scrollX = TRUE, dom = "t")
      )
    })

    output$validation_confusion <- DT::renderDT({
      value <- validation()
      shiny::req(value)
      table <- as.data.frame.matrix(value$confusion)
      table <- cbind(truth = rownames(table), table, row.names = NULL)
      .content_datatable(
        table,
        caption = "Confusion matrix",
        rownames = FALSE,
        options = list(scrollX = TRUE, dom = "t")
      )
    })

    output$validation_report <- shiny::renderText({
      shiny::req(validation(), gold(), locked_protocol())
      report_text(
        validation(),
        gold = gold(),
        protocol = locked_protocol()
      )
    })

    output$validation_raw <- shiny::renderPrint({
      shiny::req(validation())
      print(validation())
    })

    output$validation_timing_status <- shiny::renderUI({
      text <- timing_summary_text(validation_timing())
      if (is.null(text)) return(NULL)
      shiny::tags$p(class = "text-body-secondary", text)
    })

    output$validation_timing_ui <- shiny::renderUI({
      if (is.null(validation_timing())) return(NULL)
      shiny::tagList(
        shiny::uiOutput(session$ns("validation_timing_status")),
        if (!pkg_available("ggplot2")) {
          install_guidance_ui("ggplot2")
        } else {
          shiny::plotOutput(
            session$ns("validation_timing_plot"),
            height = 260
          )
        }
      )
    })

    output$validation_timing_plot <- shiny::renderPlot({
      timing <- timing_by_unit(validation_timing())
      shiny::req(timing, pkg_available("ggplot2"))
      ggplot2::ggplot(
        timing,
        ggplot2::aes(
          x = timing$unit_id,
          y = timing$duration_seconds
        )
      ) +
        ggplot2::geom_col(fill = "#6C757D") +
        ggplot2::labs(
          title = "Recorded call time by holdout unit",
          x = "Unit",
          y = "Seconds"
        ) +
        ggplot2::theme_minimal()
    })

    output$validation_plot <- shiny::renderPlot({
      shiny::req(requireNamespace("ggplot2", quietly = TRUE))
      df <- data.frame(
        split = c("dev", "test"),
        units = c(dev_units(), test_units())
      )

      ggplot2::ggplot(df, ggplot2::aes(x = split, y = units)) +
        ggplot2::geom_col(fill = "#6C757D") +
        ggplot2::scale_x_discrete(
          labels = c(dev = "Development", test = "Test")
        ) +
        ggplot2::labs(
          title = "Gold-unit counts by split",
          x = "Gold split",
          y = "Number of gold units",
          caption = paste(
            "Bar heights compare development and test gold-unit counts.",
            "They do not show validation accuracy."
          )
        ) +
        ggplot2::theme_minimal()
    })

    output$validation_plot_ui <- shiny::renderUI({
      if (!pkg_available("ggplot2")) return(install_guidance_ui("ggplot2"))
      shiny::plotOutput(session$ns("validation_plot"), height = 300)
    })

    shiny::observeEvent(input$continue_validate, {
      if (!validation_matches_lock()) {
        warn_user("Validate the locked protocol before continuing to corpus coding.")
        return()
      }
      step(6L)
    })

    shiny::observeEvent(input$corpus_file, {
      df <- read_upload_safely(input$corpus_file)
      if (is.null(df)) return()
      run_error(NULL)
      corpus_raw(df)
      coded(NULL)
      corrected(NULL)
      coding_timing(NULL)
      artifacts$coded <- NULL
      artifacts$coded_calls <- NULL
    })

    shiny::observeEvent(input$load_demo_corpus, {
      path <- system.file("extdata", "demo_corpus.csv", package = "LLMRcontent")
      corpus_raw(read_csv_path(path))
      coded(NULL)
      corrected(NULL)
      coding_timing(NULL)
      artifacts$coded <- NULL
      artifacts$coded_calls <- NULL
    })

    output$corpus_map_ui <- shiny::renderUI({
      df <- corpus_raw()
      if (is.null(df)) {
        return(shiny::tags$p(
          class = "text-muted",
          "Upload a corpus CSV or load the demo corpus to select its text column."
        ))
      }

      cols <- column_names_for_mapping(df)
      text_guess <- guess_column(cols, "text")
      shiny::tagList(
        shiny::selectInput(
          session$ns("corpus_text_col"),
          "Text column",
          choices = cols,
          selected = text_guess
        ),
        shiny::selectInput(
          session$ns("corpus_id_col"),
          shiny::tagList(
            "Stable id column (optional) ",
            help_tip(paste(
              "Use the same id column as the gold data so the correction can",
              "link audited units to corpus rows."))
          ),
          choices = c("(none)" = "", cols),
          selected = guess_column(cols, "id", exclude = text_guess) %||% ""
        )
      )
    })

    output$corpus_preview <- DT::renderDT({
      shiny::validate(
        shiny::need(
          !is.null(corpus_raw()),
          "Upload a corpus CSV or load the demo corpus to preview it."
        )
      )
      value <- corpus_raw()
      .content_datatable(
        utils::head(value, 20),
        text = input$corpus_text_col %||% names(value)[[1]],
        rownames = FALSE,
        options = list(pageLength = 5)
      )
    })

    shiny::observe({
      if (!is.null(active) && !identical(active(), "coder")) return()
      if (!identical(shared$mode(), "live")) {
        shared$set_plan(0L)
        return()
      }

      estimate <- switch(
        as.character(step()),
        "3" = {
          variants <- input$prompt_variants %||% character()
          replicates <- suppressWarnings(
            as.integer(input$protocol_replicates %||% 1L)
          )
          if (is.na(replicates)) replicates <- 1L
          list(
            calls = length(variants) * replicates * dev_units(),
            task = "Protocol tuning"
          )
        },
        "4" = {
          if (is.null(protocols())) {
            list(calls = 0L, task = "Tuning")
          } else {
            reps <- sum(vapply(protocols(), `[[`, integer(1), "replicates"))
            list(calls = reps * dev_units(), task = "Tuning")
          }
        },
        "5" = {
          if (is.null(locked_protocol())) {
            list(calls = 0L, task = "Validation")
          } else {
            list(
              calls = locked_protocol()$replicates * test_units(),
              task = "Validation"
            )
          }
        },
        "6" = {
          if (is.null(corpus_raw()) || is.null(locked_protocol())) {
            list(calls = 0L, task = "Corpus coding")
          } else if (archive_replay_available() &&
                     isTRUE(input$use_built_archive)) {
            list(calls = 0L, task = "Offline archive replay")
          } else {
            list(
              calls = NROW(corpus_raw()) * locked_protocol()$replicates,
              task = "Corpus coding"
            )
          }
        },
        list(calls = 0L, task = "Next run")
      )
      calls <- as.integer(estimate$calls)
      shared$set_plan(
        calls,
        plan_label(estimate$task, calls)
      )
    })

    run_code_corpus <- function(confirmed = FALSE) {
      if (is.null(locked_protocol())) {
        warn_user("Lock a protocol before coding the corpus.")
        return()
      }
      if (!validation_matches_lock()) {
        warn_user("Validate the locked protocol before coding the corpus.")
        return()
      }
      if (is.null(corpus_raw())) {
        warn_user("Upload a corpus CSV or load the demo corpus before coding.")
        return()
      }
      text_col <- input$corpus_text_col %||% ""
      if (!nzchar(text_col) || !text_col %in% names(corpus_raw())) {
        warn_user("Select a text column before coding the corpus.")
        return()
      }
      corpus_id_col <- input$corpus_id_col %||% ""
      if (nzchar(corpus_id_col) && identical(corpus_id_col, text_col)) {
        warn_user("The id column must differ from the text column.")
        return()
      }
      if (nzchar(corpus_id_col) && !corpus_id_col %in% names(corpus_raw())) {
        warn_user("The selected id column is not in the corpus.")
        return()
      }

      use_archive <- archive_replay_available() &&
        isTRUE(input$use_built_archive)

      if (!use_archive &&
          identical(shared$mode(), "live") && !shared$can_run()) {
        run_error(live_run_blocker_ui(shared$key()))
        shiny::showNotification(
          "Set the provider API key before coding the corpus.",
          type = "warning",
          session = session
        )
        return()
      }

      reps <- as.integer((locked_protocol() %||% list())$replicates %||% 1L)
      planned <- NROW(corpus_raw()) * reps
      if (identical(shared$mode(), "live") && !use_archive) {
        shared$set_plan(planned, plan_label("Corpus coding", planned))
        if (!isTRUE(confirmed) &&
            planned > .content_large_run_threshold) {
          .content_large_run_modal(
            session$ns,
            "confirm_code_corpus",
            "Corpus coding",
            planned,
            sprintf("%d expected model responses", planned)
          )
          return()
        }
      }
      runner <- if (use_archive) {
        LLMRcontent::archive_replay(artifacts$archive)
      } else {
        build_runner(shared$mode(), coder_demo_responder(codebook()))
      }
      call_batches <- list()
      timed_runner <- function(experiments, ...) {
        result <- runner(experiments, ...)
        call_batches[[length(call_batches) + 1L]] <<- result
        result
      }
      started <- proc.time()[["elapsed"]]
      coding_timing(NULL)

      res <- safe_llmr_call(
        shiny::withProgress(message = "Coding corpus", value = 0, {
          shiny::incProgress(0.2)
          out <- call_code_corpus_mapped(
            corpus = corpus_raw(),
            text_col = text_col,
            protocol = locked_protocol(),
            .runner = timed_runner,
            id_col = if (nzchar(corpus_id_col)) corpus_id_col else NULL
          )
          shiny::incProgress(0.8)
          out
        }),
        shared$provider()
      )

      if (!res$ok) {
        run_error(res$ui)
        return()
      }

      out <- if (identical(shared$mode(), "demo") && !use_archive) {
        annotate_demo_result(res$value)
      } else {
        res$value
      }
      call_results <- bind_call_batches(call_batches)
      coded(out)
      coding_timing(finish_call_timing(call_batches, started))
      artifacts$coded <- out
      artifacts$coded_calls <- call_results
      if (use_archive) {
        shared$add_usage(list(result_rows = NROW(call_results)))
      } else {
        add_result_usage(out, fallback_calls = planned)
      }

      warns <- character()
      corr <- NULL
      if (!isTRUE(input$correction_srs)) {
        warns <- c(warns, paste(
          "Correction skipped: corrected prevalences assume the audited",
          "(holdout) units are a random subsample of this corpus. Tick the",
          "confirmation box above the confidence setting to compute them."))
      } else {
        corr <- tryCatch(
          withCallingHandlers(
            LLMRcontent::gold_correct(out, gold(),
                                      conf = input$correction_conf %||% 0.95,
                                      design = "srs"),
            warning = function(w) {
              warns <<- c(warns, conditionMessage(w))
              invokeRestart("muffleWarning")
            }
          ),
          error = function(e) {
            warns <<- c(warns, paste("Correction unavailable:", conditionMessage(e)))
            NULL
          }
        )
      }

      correction_warnings(warns)
      # The coded corpus and the gold correction are different objects: coded()
      # is the row-per-unit corpus (the download), corrected() is the
      # gold_correction summary (the prevalence table). Keep them separate.
      corrected(corr)
      run_error(NULL)
    }

    shiny::observeEvent(input$run_code_corpus, {
      run_code_corpus()
    })

    shiny::observeEvent(input$confirm_code_corpus, {
      shiny::removeModal()
      run_code_corpus(confirmed = TRUE)
    })

    output$coded_preview <- DT::renderDT({
      shiny::validate(
        shiny::need(!is.null(coded()), "Code the corpus to preview coded rows.")
      )
      value <- coded()
      .content_datatable(
        tibble::as_tibble(value),
        text = value$text,
        digits = 3,
        rownames = FALSE,
        options = list(pageLength = 5)
      )
    })

    output$coded_status <- shiny::renderUI({
      value <- coded()
      if (is.null(value)) return(NULL)
      data <- tibble::as_tibble(value)
      shiny::tags$p(
        class = "fw-semibold",
        sprintf(
          "Coded %d units under protocol %s with %d replicate%s per unit.",
          nrow(data),
          substr(value$protocol_hash, 1, 12),
          locked_protocol()$replicates,
          if (identical(locked_protocol()$replicates, 1L)) "" else "s"
        )
      )
    })

    output$coded_distribution <- DT::renderDT({
      value <- coded()
      shiny::req(value)
      labels <- as.character(tibble::as_tibble(value)$label)
      counts <- as.data.frame(table(label = labels, useNA = "ifany"))
      names(counts)[2] <- "n"
      counts$share <- counts$n / sum(counts$n)
      .content_datatable(
        counts,
        digits = 3,
        caption = "Coded-label distribution",
        rownames = FALSE,
        options = list(dom = "t")
      )
    })

    output$correction_table <- DT::renderDT({
      shiny::validate(
        shiny::need(
          !is.null(corrected()),
          "Code the corpus to estimate gold-corrected category prevalences."
        )
      )
      .content_datatable(
        tibble::as_tibble(corrected()),
        digits = 3,
        caption = "Gold-corrected category prevalences",
        rownames = FALSE,
        options = list(scrollX = TRUE, pageLength = 5)
      )
    })

    output$correction_warnings <- shiny::renderUI({
      warns <- correction_warnings()
      if (length(warns) == 0) return(NULL)

      bslib::card(
        class = "border-warning",
        bslib::card_header("Correction notes"),
        bslib::card_body(shiny::tags$ul(lapply(warns, shiny::tags$li)))
      )
    })

    output$coding_report <- shiny::renderText({
      shiny::req(validation(), gold(), locked_protocol())
      report_text(
        validation(),
        gold = gold(),
        protocol = locked_protocol()
      )
    })

    output$coding_raw <- shiny::renderPrint({
      shiny::req(coded())
      print(coded())
      if (!is.null(corrected())) print(corrected())
    })

    output$coding_timing_status <- shiny::renderUI({
      text <- timing_summary_text(coding_timing())
      if (is.null(text)) return(NULL)
      shiny::tags$p(class = "text-body-secondary", text)
    })

    output$coding_timing_ui <- shiny::renderUI({
      if (is.null(coding_timing())) return(NULL)
      shiny::tagList(
        shiny::uiOutput(session$ns("coding_timing_status")),
        if (!pkg_available("ggplot2")) {
          install_guidance_ui("ggplot2")
        } else {
          shiny::plotOutput(session$ns("coding_timing_plot"), height = 260)
        }
      )
    })

    output$coding_timing_plot <- shiny::renderPlot({
      timing <- timing_by_unit(coding_timing())
      shiny::req(timing, pkg_available("ggplot2"))
      ggplot2::ggplot(
        timing,
        ggplot2::aes(
          x = timing$unit_id,
          y = timing$duration_seconds
        )
      ) +
        ggplot2::geom_col(fill = "#6C757D") +
        ggplot2::labs(
          title = "Recorded call time by corpus unit",
          x = "Unit",
          y = "Seconds"
        ) +
        ggplot2::theme_minimal()
    })

    shiny::observeEvent(input$continue_corpus, {
      if (is.null(coded())) {
        warn_user("Code the corpus before continuing to downloads.")
        return()
      }
      step(7L)
    })

    output$download_summary <- shiny::renderUI({
      shiny::req(coded(), validation(), locked_protocol())
      coded_demo <- is_demo_result(coded())
      validation_demo <- is_demo_result(validation())
      provenance <- if (coded_demo && validation_demo) {
        "The coded corpus and validation report come from deterministic demo runs."
      } else if (coded_demo) {
        "The coded corpus comes from a deterministic demo run; the validation report does not."
      } else if (validation_demo) {
        "The validation report comes from a deterministic demo run; the coded corpus does not."
      } else {
        NULL
      }

      shiny::tagList(
        if (!is.null(provenance)) {
          shiny::tagList(
            demo_banner_ui(),
            shiny::tags$p(class = "text-body-secondary", provenance)
          )
        },
        shiny::tags$ul(
          shiny::tags$li("Coded corpus CSV"),
          shiny::tags$li("Methods text from LLMR::report()"),
          shiny::tags$li("Locked protocol and validation context represented in the methods text")
        ),
        shiny::tags$p("Next: use the methods text with reports, and keep the locked protocol with project records.")
      )
    })

    output$download_report <- shiny::renderText({
      shiny::req(validation(), gold(), locked_protocol())
      report_text(
        validation(),
        gold = gold(),
        protocol = locked_protocol()
      )
    })

    output$download_bundle <- shiny::downloadHandler(
      filename = function() {
        paste0("llmrcoder_artifacts_", Sys.Date(), ".zip")
      },
      content = function(file) {
        bundle_coder_artifacts(
          coded = coded(),
          validation = validation(),
          gold = gold(),
          protocol = locked_protocol(),
          file = file,
          coded_demo = is_demo_result(coded()),
          validation_demo = is_demo_result(validation())
        )
      }
    )

    list(
      coded = shiny::reactive(coded()),
      coded_calls = shiny::reactive(artifacts$coded_calls)
    )
  })
}

coder_steps_ui <- function(ns) {
  bslib::accordion(
    id = ns("coding_steps"),
    open = "1",
    multiple = FALSE,
    bslib::accordion_panel(
      "1 Codebook",
      coder_codebook_ui(ns),
      value = "1"
    ),
    bslib::accordion_panel(
      "2 Gold",
      coder_gold_ui(ns),
      value = "2"
    ),
    bslib::accordion_panel(
      "3 Config",
      coder_config_ui(ns),
      value = "3"
    ),
    bslib::accordion_panel(
      "4 Tune",
      coder_tune_ui(ns),
      value = "4"
    ),
    bslib::accordion_panel(
      "5 Validate",
      coder_validate_ui(ns),
      value = "5"
    ),
    bslib::accordion_panel(
      "6 Code",
      coder_corpus_ui(ns),
      value = "6"
    ),
    bslib::accordion_panel(
      "7 Download",
      coder_download_ui(ns),
      value = "7"
    )
  )
}

coder_step_action_ui <- function(ns, step, shared) {
  controls <- switch(
    as.character(step),
    "1" = shiny::uiOutput(ns("save_codebook_action")),
    "2" = shiny::tagList(
      shiny::uiOutput(ns("create_gold_action")),
      help_tip(
        paste(
          "Sealing reserves the holdout for validation and records every use",
          "in its ledger."
        )
      ),
      shiny::uiOutput(ns("continue_gold_action"))
    ),
    "3" = shiny::uiOutput(ns("build_protocols_action")),
    "4" = shiny::tagList(
      shiny::uiOutput(ns("run_tune_action")),
      shiny::uiOutput(ns("continue_tune_action"))
    ),
    "5" = shiny::tagList(
      shiny::uiOutput(ns("lock_protocol_action")),
      help_tip(
        paste(
          "Locking hashes the codebook, prompt, model settings, parser,",
          "and replicate count."
        )
      ),
      shiny::tags$div(
        class = "border rounded p-2",
        shiny::checkboxInput(
          ns("confirm_ledger"),
          shiny::tagList(
            "Confirm: record this validation in the sealed test-split ledger ",
            help_tip(
              "Each test-split evaluation is appended to the gold-set ledger."
            )
          ),
          value = FALSE
        ),
        shiny::uiOutput(ns("run_validate_action"))
      ),
      shiny::uiOutput(ns("continue_validate_action"))
    ),
    "6" = shiny::tagList(
      shiny::uiOutput(ns("corpus_run_action")),
      shiny::uiOutput(ns("continue_corpus_action"))
    ),
    "7" = shiny::downloadButton(
      ns("download_bundle"), "Download artifacts"
    ),
    NULL
  )
  shiny::tags$div(
    class = paste(
      "d-flex flex-wrap align-items-center gap-2 mt-3 pt-3",
      "border-top"
    ),
    controls
  )
}

coder_codebook_ui <- function(ns) {
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::textInput(
          ns("codebook_name"), "Codebook name", "Demo codebook",
          placeholder = "Descriptive codebook name"
        )
      ),
      shiny::column(
        6,
        shiny::textInput(
          ns("codebook_unit"), "Unit", "text response",
          placeholder = "Unit of analysis"
        )
      )
    ),
    shiny::uiOutput(ns("category_editor")),
    bslib::card(
      bslib::card_header("Preview"),
      bslib::card_body(
        text_block_output(ns("codebook_preview"), height = "20rem"),
        shiny::textOutput(ns("codebook_hash"))
      )
    )
  )
}

coder_gold_ui <- function(ns) {
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(6, shiny::fileInput(ns("gold_file"), "Gold CSV", accept = ".csv")),
      shiny::column(6, shiny::actionButton(ns("load_demo_gold"), "Load demo gold"))
    ),
    shiny::uiOutput(ns("gold_map_ui")),
    shiny::fluidRow(
      shiny::column(
        4,
        shiny::numericInput(
          ns("dev_split"),
          shiny::tagList(
            "Development split percent ",
            help_tip(
              "The development split is used for tuning; the remainder is reserved as the sealed holdout."
            )
          ),
          value = 60, min = 10, max = 90, step = 5
        )
      ),
      shiny::column(
        4,
        shiny::checkboxInput(ns("stratify_gold"), "Stratify", value = TRUE)
      ),
      shiny::column(
        4,
        shiny::numericInput(
          ns("gold_seed"), "Seed", value = 110, min = 1, step = 1
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(6, shiny::numericInput(ns("expected_agreement"), "Expected agreement", value = 0.8, min = 0.1, max = 1, step = 0.05)),
      shiny::column(6, shiny::numericInput(ns("ci_width"), "CI width", value = 0.1, min = 0.01, max = 0.5, step = 0.01))
    ),
    shiny::uiOutput(ns("gold_size_status")),
    DT::DTOutput(ns("gold_size_table")),
    shiny::tags$details(
      class = "mb-3",
      shiny::tags$summary("Technical details"),
      text_block_output(ns("gold_size_helper"), height = "10rem")
    ),
    shiny::uiOutput(ns("gold_status")),
    DT::DTOutput(ns("gold_preview"))
  )
}

coder_config_ui <- function(ns, shared = NULL) {
  shiny::tagList(
    shiny::uiOutput(ns("protocol_context")),
    shiny::textAreaInput(
      ns("prompt_template"),
      "Coding prompt template",
      value = paste(
        "Use this codebook:",
        "{codebook}",
        "",
        "Code this text:",
        "{text}",
        "",
        "Return the single best category label.",
        sep = "\n"
      ),
      rows = 9
    ),
    shiny::uiOutput(ns("prompt_validation")),
    shiny::checkboxGroupInput(
      ns("prompt_variants"),
      "Prompt candidates",
      choices = c("Base" = "base", "Strict label only" = "strict", "Force closest label" = "uncertain"),
      selected = c("base", "strict")
    ),
    shiny::numericInput(
      ns("protocol_replicates"),
      shiny::tagList(
        "Replicates ",
        help_tip(
          "Each unit is coded this many times and reduced to its modal label."
        )
      ),
      value = 1, min = 1, max = 10, step = 1
    ),
    shiny::uiOutput(ns("protocol_scale_preview"))
  )
}

prompt_variant <- function(prompt, variant) {
  switch(
    variant,
    strict = paste(prompt, "Return only a category label. Do not include explanation.", sep = "\n\n"),
    uncertain = paste(prompt, "If the evidence is weak, choose the closest category and keep the response concise.", sep = "\n\n"),
    prompt
  )
}

coder_tune_ui <- function(ns, shared = NULL, protocols = NULL, tuning = NULL,
                          timing = NULL) {
  shiny::tagList(
    shiny::uiOutput(ns("tune_context")),
    shiny::uiOutput(ns("protocol_candidates_status")),
    DT::DTOutput(ns("tune_table")),
    shiny::uiOutput(ns("tune_timing_ui")),
    shiny::uiOutput(ns("winner_ui"))
  )
}

coder_validate_ui <- function(ns, shared = NULL, locked = NULL,
                              validation = NULL, timing = NULL) {
  shiny::tagList(
    shiny::uiOutput(ns("validate_context")),
    shiny::uiOutput(ns("lock_status")),
    shiny::uiOutput(ns("validation_status")),
    DT::DTOutput(ns("validation_table")),
    DT::DTOutput(ns("validation_categories")),
    DT::DTOutput(ns("validation_confusion")),
    shiny::uiOutput(ns("validation_timing_ui")),
    shiny::uiOutput(ns("validation_plot_ui")),
    shiny::tags$h4("Gold ledger"),
    DT::DTOutput(ns("ledger_table")),
    shiny::tags$details(
      class = "mb-3",
      shiny::tags$summary("Technical details"),
      shiny::tags$h5("Methods report"),
      text_block_output(ns("validation_report"), height = "20rem"),
      shiny::tags$h5("Console summary"),
      text_block_output(ns("validation_raw"), height = "10rem")
    )
  )
}

coder_corpus_ui <- function(ns, shared = NULL, corpus = NULL, coded = NULL,
                            timing = NULL, archive_available = FALSE) {
  shiny::tagList(
    shiny::uiOutput(ns("corpus_run_context")),
    shiny::fluidRow(
      shiny::column(6, shiny::fileInput(ns("corpus_file"), "Corpus CSV", accept = ".csv")),
      shiny::column(6, shiny::actionButton(ns("load_demo_corpus"), "Load demo corpus"))
    ),
    shiny::uiOutput(ns("corpus_map_ui")),
    shiny::uiOutput(ns("archive_replay_choice")),
    # Replicates are fixed by the locked protocol, not chosen here, so there is
    # no corpus-replicates control; the cost estimate reads protocol$replicates.
    shiny::checkboxInput(
      ns("correction_srs"),
      shiny::tagList(
        "The audited (holdout) units are a random subsample of this corpus ",
        help_tip(paste(
          "The corrected prevalences use the survey-sampling difference",
          "estimator, which is justified only for a random subsample; a",
          "balanced or convenience audit biases it in an unknowable",
          "direction. Coding proceeds either way; only the correction",
          "waits for this confirmation."))
      ),
      value = FALSE
    ),
    shiny::numericInput(ns("correction_conf"), "Correction confidence", value = 0.95, min = 0.5, max = 0.99, step = 0.01),
    DT::DTOutput(ns("corpus_preview")),
    shiny::uiOutput(ns("correction_warnings")),
    shiny::uiOutput(ns("coded_status")),
    DT::DTOutput(ns("coded_distribution")),
    DT::DTOutput(ns("coded_preview")),
    DT::DTOutput(ns("correction_table")),
    shiny::uiOutput(ns("coding_timing_ui")),
    shiny::tags$details(
      class = "mb-3",
      shiny::tags$summary("Technical details"),
      shiny::tags$h5("Methods report"),
      text_block_output(ns("coding_report"), height = "20rem"),
      shiny::tags$h5("Console summaries"),
      text_block_output(ns("coding_raw"), height = "12rem")
    )
  )
}

coder_download_ui <- function(ns, coded = NULL, validation = NULL) {
  shiny::tagList(
    shiny::uiOutput(ns("download_summary")),
    shiny::tags$h4("Methods report"),
    text_block_output(ns("download_report"), height = "20rem")
  )
}
