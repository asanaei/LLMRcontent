# mod_coder.R ------------------------------------------------------------------
# The LLMRcontent coding workflow as an eight-step stepper: codebook, gold set, config,
# tune, validate, code corpus, download. Each step wraps the LLMRcontent coding API and
# routes failures through safe_llmr_call() so a missing key or package shows a
# banner rather than crashing the app.

# Substrate helpers (pkg_available, safe_llmr_call, build_*, read_csv_*, etc.)
# are defined as lazy forwarders in gui_aliases.R, so the call sites below stay
# short without referencing the LLMR.shiny Suggests at package load.

mod_coder_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("module_ui"))
}

mod_coder_server <- function(id, shared, active = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
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
    run_error <- shiny::reactiveVal(NULL)
    correction_warnings <- shiny::reactiveVal(character())

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
          shiny::uiOutput(session$ns("step_body"))
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
    })

    output$run_error <- shiny::renderUI({
      run_error()
    })

    output$step_body <- shiny::renderUI({
      switch(
        as.character(step()),
        "1" = coder_codebook_ui(session$ns),
        "2" = coder_gold_ui(session$ns),
        "3" = coder_config_ui(session$ns, shared),
        "4" = coder_tune_ui(session$ns, shared, protocols(), tuning()),
        "5" = coder_validate_ui(session$ns, shared, locked_protocol(), validation()),
        "6" = coder_corpus_ui(session$ns, shared, corpus_raw(), coded()),
        "7" = coder_download_ui(session$ns, coded(), validation()),
        coder_codebook_ui(session$ns)
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
              shiny::textInput(session$ns(paste0("cat_label_", i)), "Label", value = df$label[[i]]),
              shiny::textAreaInput(session$ns(paste0("cat_definition_", i)), "Definition", value = df$definition[[i]], rows = 2),
              shiny::textAreaInput(session$ns(paste0("cat_include_", i)), "Include terms", value = df$include[[i]], rows = 2),
              shiny::textAreaInput(session$ns(paste0("cat_exclude_", i)), "Exclude terms", value = df$exclude[[i]], rows = 2),
              shiny::textAreaInput(session$ns(paste0("cat_examples_", i)), "Examples", value = df$examples[[i]], rows = 2),
              shiny::textAreaInput(session$ns(paste0("cat_counterexamples_", i)), "Counterexamples", value = df$counterexamples[[i]], rows = 2)
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
    })

    shiny::observeEvent(input$load_demo_gold, {
      path <- system.file("extdata", "demo_gold.csv", package = "LLMRcontent")
      gold_raw(read_csv_path(path))
      gold(NULL)
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
      shiny::tagList(
        shiny::selectInput(session$ns("gold_text_col"), "Text column", choices = cols, selected = cols[[1]]),
        shiny::selectInput(session$ns("gold_label_col"), "Label column", choices = cols, selected = cols[[min(2, length(cols))]])
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
      DT::datatable(utils::head(df, 20), options = list(scrollX = TRUE, pageLength = 5))
    })

    output$gold_size_helper <- shiny::renderPrint({
      shiny::req(pkg_available("LLMRcontent"))
      LLMRcontent::gold_size(
        expected_agreement = input$expected_agreement %||% 0.8,
        ci_width = input$ci_width %||% 0.1
      )
    })

    output$gold_status <- shiny::renderUI({
      if (is.null(gold())) return(NULL)

      bslib::card(
        class = "border-info",
        bslib::card_header("Sealed gold set"),
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
      set.seed(seed)
      split <- c(dev = input$dev_split / 100, test = 1 - input$dev_split / 100)

      res <- safe_llmr_call(
        call_gold_set_mapped(
          gold_raw(),
          text_col,
          label_col,
          split = split,
          stratify = isTRUE(input$stratify_gold),
          seal_holdout = TRUE
        ),
        shared$provider()
      )

      if (!res$ok) {
        run_error(res$ui)
        return()
      }

      run_error(NULL)
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
          temperature = input$temperature %||% 0
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
      protocols(res$value)
      run_error(NULL)
      step(4L)
    })

    dev_units <- shiny::reactive({
      if (is.null(gold())) return(0L)
      out <- tryCatch(LLMRcontent::gold_split(gold(), split = "dev"), error = function(e) NULL)
      if (is.null(out)) return(0L)
      NROW(as_display_table(out))
    })

    test_units <- shiny::reactive({
      if (is.null(gold())) return(0L)
      out <- tryCatch(LLMRcontent::gold_split(gold(), split = "test"), error = function(e) NULL)
      if (is.null(out)) return(0L)
      NROW(as_display_table(out))
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

      res <- safe_llmr_call(
        shiny::withProgress(message = "Tuning protocols", value = 0, {
          shiny::incProgress(0.2)
          out <- LLMRcontent::tune_protocol(
            protocols = protocols(),
            gold = gold(),
            split = "dev",
            .runner = runner
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
      tuning(out)
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
      DT::datatable(
        as_display_table(tibble::as_tibble(tuning())),
        caption = "Development-split performance used for protocol tuning",
        options = list(scrollX = TRUE, pageLength = 5)
      )
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

      locked_protocol(res$value)
      run_error(NULL)
    })

    output$lock_status <- shiny::renderUI({
      shiny::req(locked_protocol())
      hash <- locked_protocol()$hash %||% "hash unavailable"
      shiny::tags$p(class = "text-success", paste("LOCKED", hash))
    })

    output$ledger_table <- DT::renderDT({
      shiny::req(gold())
      ledger <- LLMRcontent::gold_ledger(gold())
      DT::datatable(as_display_table(ledger), options = list(scrollX = TRUE, pageLength = 5))
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

      res <- safe_llmr_call(
        shiny::withProgress(message = "Validating locked protocol", value = 0, {
          shiny::incProgress(0.2)
          out <- LLMRcontent::validate_protocol(
            protocol = locked_protocol(),
            gold = gold(),
            split = "test",
            .runner = runner
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
      DT::datatable(as_display_table(validation()), options = list(scrollX = TRUE, pageLength = 5))
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

    shiny::observeEvent(input$continue_validate, {
      if (is.null(validation())) {
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
    })

    shiny::observeEvent(input$load_demo_corpus, {
      path <- system.file("extdata", "demo_corpus.csv", package = "LLMRcontent")
      corpus_raw(read_csv_path(path))
      coded(NULL)
      corrected(NULL)
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
      shiny::selectInput(session$ns("corpus_text_col"), "Text column", choices = cols, selected = cols[[1]])
    })

    output$corpus_preview <- DT::renderDT({
      shiny::validate(
        shiny::need(
          !is.null(corpus_raw()),
          "Upload a corpus CSV or load the demo corpus to preview it."
        )
      )
      DT::datatable(utils::head(corpus_raw(), 20), options = list(scrollX = TRUE, pageLength = 5))
    })

    shiny::observe({
      if (!is.null(active) && !identical(active(), "coder")) return()
      if (!identical(shared$mode(), "live")) {
        shared$set_plan(0L)
        return()
      }

      estimate <- switch(
        as.character(step()),
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
        warn_user("Lock and validate a protocol before coding the corpus.")
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

      if (identical(shared$mode(), "live") && !shared$can_run()) {
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
      if (identical(shared$mode(), "live")) {
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
      runner <- build_runner(shared$mode(), coder_demo_responder(codebook()))

      res <- safe_llmr_call(
        shiny::withProgress(message = "Coding corpus", value = 0, {
          shiny::incProgress(0.2)
          out <- call_code_corpus_mapped(
            corpus = corpus_raw(),
            text_col = text_col,
            protocol = locked_protocol(),
            .runner = runner
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
      coded(out)
      add_result_usage(out, fallback_calls = planned)

      warns <- character()
      corr <- tryCatch(
        withCallingHandlers(
          LLMRcontent::gold_correct(out, gold(), conf = input$correction_conf %||% 0.95),
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
      DT::datatable(as_display_table(tibble::as_tibble(coded())),
                    options = list(scrollX = TRUE, pageLength = 5))
    })

    output$correction_table <- DT::renderDT({
      shiny::validate(
        shiny::need(
          !is.null(corrected()),
          "Code the corpus to estimate gold-corrected category prevalences."
        )
      )
      DT::datatable(
        as_display_table(tibble::as_tibble(corrected())),
        caption = "Gold-corrected category prevalences",
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

    shiny::observeEvent(input$continue_corpus, {
      if (is.null(coded())) {
        warn_user("Code the corpus before continuing to downloads.")
        return()
      }
      step(7L)
    })

    output$download_summary <- shiny::renderUI({
      shiny::req(coded(), validation(), locked_protocol())

      shiny::tagList(
        if (identical(shared$mode(), "demo") || is_demo_result(coded())) demo_banner_ui(),
        shiny::tags$ul(
          shiny::tags$li("Coded corpus CSV"),
          shiny::tags$li("Methods text from LLMR::report()"),
          shiny::tags$li("Locked protocol and validation context represented in the methods text")
        ),
        shiny::tags$p("Next: use the methods text with reports, and keep the locked protocol with project records.")
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
          demo = identical(shared$mode(), "demo") || is_demo_result(coded())
        )
      }
    )
  })
}

coder_codebook_ui <- function(ns) {
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(6, shiny::textInput(ns("codebook_name"), "Codebook name", "Demo codebook")),
      shiny::column(6, shiny::textInput(ns("codebook_unit"), "Unit", "text response"))
    ),
    shiny::uiOutput(ns("category_editor")),
    bslib::card(
      bslib::card_header("Preview"),
      bslib::card_body(
        shiny::verbatimTextOutput(ns("codebook_preview")),
        shiny::verbatimTextOutput(ns("codebook_hash"))
      )
    ),
    shiny::actionButton(ns("save_codebook"), "Save and continue", class = "btn-primary")
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
      shiny::column(3, shiny::numericInput(ns("dev_split"), "Dev split percent", value = 60, min = 10, max = 90, step = 5)),
      shiny::column(3, shiny::checkboxInput(ns("stratify_gold"), "Stratify", value = TRUE)),
      shiny::column(3, shiny::numericInput(ns("gold_seed"), "Seed", value = 110, min = 1, step = 1)),
      shiny::column(3, shiny::actionButton(ns("create_gold"), "Create sealed gold", class = "btn-primary"))
    ),
    shiny::fluidRow(
      shiny::column(6, shiny::numericInput(ns("expected_agreement"), "Expected agreement", value = 0.8, min = 0.1, max = 1, step = 0.05)),
      shiny::column(6, shiny::numericInput(ns("ci_width"), "CI width", value = 0.1, min = 0.01, max = 0.5, step = 0.01))
    ),
    shiny::verbatimTextOutput(ns("gold_size_helper")),
    shiny::uiOutput(ns("gold_status")),
    DT::DTOutput(ns("gold_preview")),
    shiny::actionButton(ns("continue_gold"), "Continue", class = "btn-primary")
  )
}

coder_config_ui <- function(ns, shared) {
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

  shiny::tagList(
    key_ui,
    config_summary,
    shiny::textAreaInput(
      ns("prompt_template"),
      "Prompt template",
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
      choices = c("Base" = "base", "Strict label only" = "strict", "Include uncertainty instruction" = "uncertain"),
      selected = c("base", "strict")
    ),
    shiny::fluidRow(
      shiny::column(6, shiny::numericInput(ns("temperature"), "Temperature", value = 0, min = 0, max = 2, step = 0.1)),
      shiny::column(6, shiny::numericInput(ns("protocol_replicates"), "Replicates", value = 1, min = 1, max = 10, step = 1))
    ),
    shiny::actionButton(ns("build_protocols"), "Build protocols", class = "btn-primary")
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

coder_tune_ui <- function(ns, shared, protocols, tuning) {
  disabled <- if (identical(shared$mode(), "live") && !shared$can_run()) "disabled" else NULL

  shiny::tagList(
    if (identical(shared$mode(), "live") && !shared$can_run()) live_run_blocker_ui(shared$key()),
    if (identical(shared$mode(), "demo")) demo_banner_ui(),
    shiny::tags$p(paste0("Protocol candidates: ", length(protocols %||% list()))),
    shiny::actionButton(ns("run_tune"), "Run tuning", class = "btn-primary", disabled = disabled),
    DT::DTOutput(ns("tune_table")),
    shiny::uiOutput(ns("winner_ui")),
    shiny::actionButton(ns("continue_tune"), "Continue to lock", class = "btn-primary")
  )
}

coder_validate_ui <- function(ns, shared, locked, validation) {
  disabled <- if (identical(shared$mode(), "live") && !shared$can_run()) "disabled" else NULL

  shiny::tagList(
    if (identical(shared$mode(), "live") && !shared$can_run()) live_run_blocker_ui(shared$key()),
    if (identical(shared$mode(), "demo")) demo_banner_ui(),
    shiny::actionButton(ns("lock_protocol"), "Lock selected protocol", class = "btn-primary"),
    shiny::uiOutput(ns("lock_status")),
    shiny::checkboxInput(ns("confirm_ledger"), "Validation is ledgered against the sealed test split", value = FALSE),
    shiny::actionButton(ns("run_validate"), "Validate locked protocol", class = "btn-primary", disabled = disabled),
    DT::DTOutput(ns("validation_table")),
    shiny::plotOutput(ns("validation_plot"), height = 300),
    shiny::tags$h4("Gold ledger"),
    DT::DTOutput(ns("ledger_table")),
    shiny::actionButton(ns("continue_validate"), "Continue to corpus coding", class = "btn-primary")
  )
}

coder_corpus_ui <- function(ns, shared, corpus, coded) {
  disabled <- if (identical(shared$mode(), "live") && !shared$can_run()) "disabled" else NULL

  shiny::tagList(
    if (identical(shared$mode(), "live") && !shared$can_run()) live_run_blocker_ui(shared$key()),
    if (identical(shared$mode(), "demo")) demo_banner_ui(),
    shiny::fluidRow(
      shiny::column(6, shiny::fileInput(ns("corpus_file"), "Corpus CSV", accept = ".csv")),
      shiny::column(6, shiny::actionButton(ns("load_demo_corpus"), "Load demo corpus"))
    ),
    shiny::uiOutput(ns("corpus_map_ui")),
    # Replicates are fixed by the locked protocol, not chosen here, so there is
    # no corpus-replicates control; the cost estimate reads protocol$replicates.
    shiny::numericInput(ns("correction_conf"), "Correction confidence", value = 0.95, min = 0.5, max = 0.99, step = 0.01),
    DT::DTOutput(ns("corpus_preview")),
    shiny::actionButton(ns("run_code_corpus"), "Code corpus", class = "btn-primary", disabled = disabled),
    shiny::uiOutput(ns("correction_warnings")),
    DT::DTOutput(ns("coded_preview")),
    DT::DTOutput(ns("correction_table")),
    shiny::actionButton(ns("continue_corpus"), "Continue to downloads", class = "btn-primary")
  )
}

coder_download_ui <- function(ns, coded, validation) {
  shiny::tagList(
    shiny::uiOutput(ns("download_summary")),
    shiny::downloadButton(ns("download_bundle"), "Download artifacts")
  )
}
