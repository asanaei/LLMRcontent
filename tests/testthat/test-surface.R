test_that("the public export surface is exact", {
  namespace_file <- file.path(
    getNamespaceInfo(asNamespace("LLMRcontent"), "path"),
    "NAMESPACE"
  )
  directives <- readLines(namespace_file, warn = FALSE)
  actual <- sub("^export\\(([^)]+)\\)$", "\\1",
                grep("^export\\(", directives, value = TRUE))
  expected <- c(
    "archive_build", "archive_check", "archive_drift", "archive_read",
    "archive_redact", "archive_replay", "archive_seal", "archive_write",
    "audit_add_models", "audit_add_perturbations", "audit_add_prompts",
    "audit_curve", "audit_fragility", "audit_placebo", "audit_plan",
    "audit_run", "audit_stability", "audit_units", "cb_category",
    "code_corpus", "codebook", "codebook_hash", "codebook_labels",
    "coder_agreement", "format_codebook", "gold_correct", "gold_ledger",
    "gold_set", "gold_size", "gold_split", "protocol", "protocol_lock",
    "run_content_studio", "sign_flip", "threshold_flip", "tune_protocol",
    "validate_protocol"
  )

  expect_length(actual, 37L)
  expect_identical(sort(actual), sort(expected))
})

test_that("report helpers remain internal and reset dispatch stays registered", {
  ns <- asNamespace("LLMRcontent")
  internal <- c("archive_appendix", "audit_report", "coding_report",
                "parse_label", "verifiability_horizon")
  exports <- getNamespaceExports("LLMRcontent")

  expect_true(all(vapply(internal, exists, logical(1), envir = ns,
                         inherits = FALSE)))
  expect_false(any(internal %in% exports))
  expect_false("reset" %in% exports)
  expect_true(exists("reset.archive_replayer", envir = ns, inherits = FALSE))
})
