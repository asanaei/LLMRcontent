fix_plan <- function(n = 6) {
  audit_plan(
    data = data.frame(text = c("cut taxes", "fund schools", "small state",
                               "public option", "deregulate",
                               "shrink the state")[seq_len(n)]),
    text = "text",
    estimator = function(d) mean(d$label == "conservative") - 0.5,
    labels = c("conservative", "progressive"),
    prompt = "One of {labels}.\n{text}\nLabel:"
  )
}

fix_cfg <- function(model) LLMR::llm_config("groq", model, temperature = 0)

# A runner whose labels depend on the cell's model: model "lean-right" codes
# everything conservative; "balanced" codes by keyword.
fake_runner <- function(experiments, ...) {
  experiments$response_text <- vapply(seq_len(nrow(experiments)), function(i) {
    cfg <- experiments$config[[i]]
    txt <- experiments$messages[[i]][["user"]]
    if (identical(cfg$model, "lean-right")) "conservative"
    else if (grepl("taxes|state|deregulate", txt)) "Conservative"
    else "progressive"
  }, character(1))
  experiments$success <- TRUE
  experiments
}

test_that("plans build, validate, and print their grid", {
  p <- fix_plan()
  expect_s3_class(p, "audit_plan")
  expect_output(print(p), "no models yet")

  p <- audit_add_models(
    p, config = list(a = fix_cfg("balanced"), b = fix_cfg("lean-right")))
  p <- audit_add_prompts(p, terse = "{labels}? {text}")
  p <- audit_add_perturbations(p, label_order = "reversed", temperature = 0.7)
  expect_output(print(p), "2 prompt\\(s\\) x 2 model\\(s\\) x 2 order\\(s\\) x 2 temperature\\(s\\) = 16")

  expect_error(audit_add_models(fix_plan(), list(fix_cfg("x"))), "named")
  expect_error(audit_add_prompts(fix_plan(), bad = "no placeholder"), "\\{text\\}")
  expect_error(audit_add_perturbations(fix_plan(), label_order = "shuffled"), "as_given")
  expect_error(audit_plan(data.frame(text = "a"), "text",
                          function(d) 1, c("x", "y"), "no placeholder"),
               "\\{text\\}")
})

test_that("prompt rendering honors label order and is literal", {
  out <- LLMRcontent:::.render_audit_prompt(
    "Pick from {labels}: {text}", "a {brace} case",
    c("x", "y"), "reversed")
  expect_match(out, "y, x", fixed = TRUE)
  expect_match(out, "a {brace} case", fixed = TRUE)
})

test_that("audit_run recomputes the estimand per cell", {
  p <- audit_add_models(fix_plan(), list(balanced = fix_cfg("balanced"),
                                         right = fix_cfg("lean-right")))
  a <- audit_run(p, .runner = fake_runner)
  expect_s3_class(a, "audit")
  expect_named(a, c("cells", "units", "plan"))
  expect_equal(nrow(a$cells), 2L)
  expect_s3_class(a$cells, "tbl_df")
  expect_s3_class(a$units, "tbl_df")
  expect_s3_class(a$plan, "audit_plan")
  expect_output(print(a), "<audit")
  # balanced codes 4/6 conservative -> 1/6; lean-right 6/6 -> 0.5
  expect_equal(a$cells$estimate[a$cells$model == "balanced"], 4 / 6 - 0.5)
  expect_equal(a$cells$estimate[a$cells$model == "right"], 0.5)
  expect_true(all(a$cells$parse_failures == 0L))
  expect_true(all(is.na(a$units$response_id)))
  expect_error(audit_run(fix_plan(), .runner = fake_runner), "no models")
})

test_that("audit_run refuses runner-reported failures", {
  p <- audit_add_models(fix_plan(), list(balanced = fix_cfg("balanced")))
  failed_runner <- function(experiments, ...) {
    experiments$response_text <- "conservative"
    experiments$success <- rep(TRUE, nrow(experiments))
    experiments$success[c(1, 2)] <- c(FALSE, NA)
    experiments
  }

  expect_error(
    audit_run(p, .runner = failed_runner),
    "reported 2 unsuccessful row\\(s\\)"
  )

  missing_rows <- function(experiments, ...) {
    data.frame(response_text = rep("conservative", nrow(experiments)))
  }
  expect_error(audit_run(p, .runner = missing_rows), "return the experiment rows")

  missing_response <- function(experiments, ...) {
    experiments$response_text <- NA_character_
    experiments
  }
  expect_error(audit_run(p, .runner = missing_response), "missing response")

  duplicated <- function(experiments, ...) {
    experiments$response_text <- "conservative"
    experiments$unit_id[2] <- experiments$unit_id[1]
    experiments
  }
  expect_error(audit_run(p, .runner = duplicated), "row identity")

  reversed <- function(experiments, ...) {
    experiments$response_text <- ifelse(
      grepl("taxes|state|deregulate",
            vapply(experiments$messages, `[[`, "", "user")),
      "conservative", "progressive")
    experiments[rev(seq_len(nrow(experiments))), , drop = FALSE]
  }
  reordered <- audit_run(p, .runner = reversed)
  expect_equal(reordered$cells$estimate, 4 / 6 - 0.5)
})

test_that("audit_stability, audit_fragility, and the curve summarize the grid honestly", {
  p <- fix_plan()
  p <- audit_add_models(p, list(balanced = fix_cfg("balanced"),
                                right = fix_cfg("lean-right")))
  p <- audit_add_perturbations(p, label_order = "reversed")
  a <- audit_run(p, .runner = function(experiments, ...) {
    # reversed label order flips the balanced model's coding -> sign flip
    experiments$response_text <- vapply(seq_len(nrow(experiments)), function(i) {
      cfg <- experiments$config[[i]]
      txt <- experiments$messages[[i]][["user"]]
      rightish <- grepl("taxes|state|deregulate", txt)
      reversed <- grepl("progressive, conservative", txt)
      if (identical(cfg$model, "lean-right")) "conservative"
      else if (xor(rightish, reversed)) "conservative" else "progressive"
    }, character(1))
    experiments$success <- TRUE
    experiments
  })

  s <- audit_stability(a)
  expect_equal(s$n_cells, 4L)
  expect_true(s$min < 0 && s$max > 0)        # the grid contains a sign flip

  f <- audit_fragility(a)
  expect_s3_class(f, "audit_fragility")
  expect_named(f, c("fragility", "flipping_cells", "status", "reference",
                    "reference_estimate"))
  expect_identical(f$fragility, 1L)           # one choice (order) flips it
  expect_true(length(f$flipping_cells) >= 1L)
  expect_type(f$flipping_cells, "integer")
  expect_identical(f$status, "ok")
  expect_identical(f$reference, 1L)
  expect_equal(f$reference_estimate,
               a$cells$estimate[a$cells$cell == 1L])
  expect_output(print(f), "<audit_fragility")

  expect_identical(formals(audit_curve)$plot, FALSE)
  visible_curve <- withVisible(audit_curve(a))
  expect_true(visible_curve$visible)
  curve <- visible_curve$value
  expect_s3_class(curve, "tbl_df")
  expect_false(inherits(curve, "audit"))
  expect_identical(curve$estimate, sort(a$cells$estimate))
  expect_identical(curve$rank, seq_len(nrow(a$cells)))
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    plotted_curve <- (function() {
      path <- tempfile(fileext = ".pdf")
      grDevices::pdf(path)
      on.exit({
        grDevices::dev.off()
        unlink(path)
      })
      withVisible(audit_curve(a, plot = TRUE))
    })()
    expect_true(plotted_curve$visible)
    expect_equal(plotted_curve$value, curve)
  }

  rpt <- LLMR::report(a)
  txt <- paste(unclass(rpt), collapse = "\n")
  expect_match(txt, "4 cells")
  expect_match(txt, "flip the sign")
  expect_match(txt, "not construct validity")
  expect_match(txt, "passed audit")
  expect_output(print(rpt), "ROBUSTNESS AUDIT")
})

test_that("shared generics dispatch for audit objects", {
  p <- audit_add_models(fix_plan(), list(balanced = fix_cfg("balanced"),
                                         right = fix_cfg("lean-right")))
  a <- audit_run(p, .runner = fake_runner)

  d <- LLMR::diagnostics(a)
  expect_s3_class(d, "tbl_df")
  expect_named(d, c("n_cells", "reference_estimate", "sign_agreement",
                    "min", "median", "max", "iqr", "n_failed_cells",
                    "fragility", "fragility_status"))
  expect_equal(d$n_cells, nrow(a$cells))
  expect_equal(d$reference_estimate, 4 / 6 - 0.5)
  expect_true(is.infinite(d$fragility))
  expect_identical(d$fragility_status, "no_flip")

  rpt <- LLMR::report(a)
  expect_s3_class(rpt, "audit_report")
  expect_match(paste(unclass(rpt), collapse = "\n"), "distribution of estimates")

  tbl <- tibble::as_tibble(a)
  expect_s3_class(tbl, "tbl_df")
  expect_false(inherits(tbl, "audit"))
  expect_equal(nrow(tbl), nrow(a$cells))
})

test_that("fragility is Inf when nothing flips", {
  p <- audit_add_models(fix_plan(), list(right = fix_cfg("lean-right")))
  a <- audit_run(p, .runner = fake_runner)
  f <- audit_fragility(a)
  expect_s3_class(f, "audit_fragility")
  expect_true(is.infinite(f$fragility))
  expect_identical(f$flipping_cells, integer(0))
  expect_identical(f$status, "no_flip")
})

test_that("the unit-level trail and cost columns ride along", {
  p <- audit_add_models(fix_plan(), list(balanced = fix_cfg("balanced")))
  a <- audit_run(p, .runner = function(experiments, ...) {
    experiments <- fake_runner(experiments)
    experiments$sent_tokens <- rep(7L, nrow(experiments))
    experiments$rec_tokens <- rep(1L, nrow(experiments))
    experiments$response_id <- paste0("r-", seq_len(nrow(experiments)))
    experiments
  })
  expect_equal(a$cells$tokens, 6L * 8L)
  u <- audit_units(a)
  expect_equal(nrow(u), 6L)
  expect_true(all(c("cell", "unit_id", "label", "response_id") %in% names(u)))
  expect_identical(u$response_id[1], "r-1")
})

test_that("flip rules are pluggable and edge-aware", {
  rule <- sign_flip()
  expect_identical(rule(c(-1, 1, NA), reference = 1), c(TRUE, FALSE, FALSE))
  # a zero reference has no sign: sign_flip flags nothing, by design
  expect_false(any(rule(c(-1, 1), reference = 0)))

  thr <- threshold_flip(at = 0.5)
  expect_identical(thr(c(0.4, 0.6), reference = 0.7), c(TRUE, FALSE))

  p <- fix_plan()
  p <- audit_add_models(p, list(balanced = fix_cfg("balanced"),
                                right = fix_cfg("lean-right")))
  a <- audit_run(p, .runner = fake_runner)
  # under a threshold rule at 0.25, lean-right (0.5) crosses from balanced (1/6)
  f <- audit_fragility(a, flip = threshold_flip(at = 0.25))
  expect_identical(f$fragility, 1L)
})

# A runner whose every reply is unparseable, so every label is NA and every
# cell estimate is NA -- the degenerate "no valid estimates" case.
all_na_runner <- function(experiments, ...) {
  experiments$response_text <- "not one of the labels at all"
  experiments$success <- TRUE
  experiments
}

test_that("audit_stability reports no_valid_estimates instead of Inf/NaN", {
  p <- audit_add_models(fix_plan(), list(a = fix_cfg("balanced")))
  a <- audit_run(p, .runner = all_na_runner)
  expect_no_error(s <- audit_stability(a))
  expect_equal(s$status, "no_valid_estimates")
  expect_true(is.na(s$min) && is.na(s$max) && is.na(s$sign_agreement))
  expect_false(is.infinite(s$min))   # the old bug returned Inf
})

test_that("audit_stability and audit_fragility reject a non-existent reference", {
  p <- audit_add_models(fix_plan(), list(a = fix_cfg("balanced")))
  a <- audit_run(p, .runner = fake_runner)
  expect_error(audit_stability(a, reference = 9999L), "not among")
  expect_error(audit_fragility(a, reference = 9999L), "not among")
})

test_that("diagnostics.audit keeps its documented columns even on an all-NA audit", {
  p <- audit_add_models(fix_plan(), list(a = fix_cfg("balanced")))
  a <- audit_run(p, .runner = all_na_runner)
  d <- LLMR::diagnostics(a)
  expect_named(d, c("n_cells", "reference_estimate", "sign_agreement",
                    "min", "median", "max", "iqr", "n_failed_cells",
                    "fragility", "fragility_status"))
  expect_identical(d$fragility_status, "reference_failed")

  f <- audit_fragility(a)
  expect_s3_class(f, "audit_fragility")
  expect_true(is.infinite(f$fragility))
  expect_identical(f$flipping_cells, integer(0))
  expect_identical(f$status, "reference_failed")
  expect_true(is.na(f$reference_estimate))
  expect_output(print(f), "status reference_failed")
  expect_match(paste(unclass(LLMR::report(a)), collapse = "\n"),
               "reference cell failed")
})

test_that("the ecosystem hash convention is pinned (drift guard vs LLMR)", {
  expect_identical(
    LLMR::llm_hash(list(model = "gpt-oss-20b", temperature = 0)),
    "7c5ffbb0b308f20bf188a3efd962a2895f45ad202307234ee1965d86abc0606c")
})

test_that("audit prompts render every placeholder occurrence (regression)", {
  out <- LLMRcontent:::.render_audit_prompt(
    "{labels} twice {labels}: {text} and {text}", "U", c("a", "b"), "as_given")
  expect_false(grepl("{text}", out, fixed = TRUE))
  expect_false(grepl("{labels}", out, fixed = TRUE))
  expect_length(gregexpr("U", out, fixed = TRUE)[[1]], 2L)
})

test_that("audit experiments carry the raw unit text as a metadata column", {
  plan <- audit_plan(
    data = data.frame(text = c("cut taxes", "fund schools")), text = "text",
    estimator = function(d) mean(d$label == "a", na.rm = TRUE),
    labels = c("a", "b"),
    prompt = "One of: {labels}.\n\n{text}\n\nLabel:")
  plan <- audit_add_models(plan,
    list(m = LLMR::llm_config("groq", "fake-model", temperature = 0)))
  seen <- NULL
  spy <- function(experiments, ...) {
    seen <<- experiments$text
    experiments$response_text <- "a"
    experiments
  }
  invisible(audit_run(plan, .runner = spy))
  expect_false(is.null(seen))
  expect_true(all(seen %in% c("cut taxes", "fund schools")))
})
