# LLMRcontent 0.2.0

Initial CRAN release.

- `gold_set()` creates development and holdout splits from human-coded data,
  while `protocol_lock()` identifies the instrument used for validation and
  coding. `code_corpus()` returns a structured `coded_corpus`, and
  `gold_correct()` estimates category prevalences and standard errors from
  matched holdout errors. Gold-set planning returns the recommendation and its
  full candidate grid.
- `audit_plan()` defines a grid of prompts, models, label orders, and
  temperatures. `audit_run()` returns the cell table, unit trail, and plan as
  fields of a structured audit object.
  `audit_stability()` and the structured `audit_fragility()` result summarize
  the estimates and conclusion flips. `audit_curve()` draws only when asked.
- `archive_build()` reads LLMR audit logs into archives. Archives can be sealed,
  checked, and replayed with `archive_seal()`, `archive_check()`, and
  `archive_replay()`. `archive_drift()` reissues an explicit fraction or count
  of stored raw records to measure live drift. Archive writes refuse an
  existing directory unless replacement is requested.
- `LLMR::report()` is the reporting entry point for validation, audit, and
  archive objects. `LLMR::diagnostics()` supplies their machine-readable
  summaries.
- `run_content_studio()` provides an optional Shiny interface. Its dependencies
  are listed in `Suggests`.
- The studio now uses the shared generation controls, exposes protocol
  replicates with complete run-size accounting, and presents validation,
  correction, robustness, placebo, diagnostics, and archive results before
  their scrollable console details. Coded-call records can flow into archive
  completeness checks, and a built archive can be used for offline corpus
  replay. Long text outputs no longer collapse inside fillable navigation
  pages. Recorded call durations are summarized when the runner supplies them.
- The coding workflow now places its seven configuration stages in collapsible
  drawers while keeping the action for the active stage outside the drawer.
  Its existing pre-filled coding prompt and category wording remain visible
  and editable. Coded units and audited units use wide, wrapping text columns,
  identifier columns remain available at the right, and audit cells and their
  unit-level trail are shown as separate tables. Studio tables now apply the
  shared display-only rounding to double columns.
