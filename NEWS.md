# LLMRcontent 0.2.1

Corrections from an external methodological review of 0.2.0 (never on CRAN).

* Replicate aggregation is an explicit measurement rule: a tie among
  replicates yields `label = NA` with a `tied` flag instead of an arbitrary
  first pick, `label_share` counts agreement over all replicates so parse
  failures lower it, and the parsed-only share moved to
  `label_share_parsed`.
* Parser output is validated centrally: a value that matches no codebook
  label is a parse failure, never an undeclared category, and a parser
  returning more than one value errors.
* `codebook()` refuses labels that collide after case and whitespace
  normalization ("Yes" and "yes"), which normalized matching could not
  tell apart.
* `code_corpus()` refuses corpora that already contain its result columns
  instead of overwriting them, and refuses missing text; `audit_plan()`
  refuses a pre-existing `label` column and missing text; `gold_set()`
  refuses missing text.
* `gold_correct()` requires a declared sampling design (`design = "srs"`)
  for the audited units, and refuses audit units that map to a corpus row
  another audit unit already matched -- double counting that could push
  the finite-population variance factor negative.
* Archives are sealed under two roots: a content root over the ordered
  record hashes alone (the stable, machine-independent identity to cite)
  and a seal root that also binds the manifest and environment, so
  manifest metadata can no longer change without breaking the seal.
  `archive_check()` verifies cardinality before any hash comparison (an
  emptied record list used to pass by recycling), and `archive_replay()`
  and `archive_drift()` refuse archives that fail it.
* Validation accuracy intervals are exact (Clopper-Pearson) rather than
  unseeded bootstrap: deterministic, and no longer a point interval at
  accuracy 0 or 1.
* The protocol hash covers the normalizer version and the aggregation
  policy, and `LLMR::report()` refuses a protocol whose hash does not
  match the validation it is asked to describe.
* Studio: the audit's default temperature grid is `0` (a temperature arm
  is measured with one draw per cell, so its differences include sampling
  noise; a warning says so when a positive temperature is added); the
  label-permutation placebo is no longer offered (every Studio estimand
  is a function of the label marginal, for which that placebo is
  degenerate by construction); the difference estimand requires two
  distinct labels; corrected prevalences wait for an explicit
  random-subsample confirmation; gold and corpus mapping gained optional
  stable-id columns; the gold-size planner reports both the evaluation
  split and the implied total.
* LLMR floor raised to 0.8.11, the first release with locale-independent
  hashing, which protocol locks and archive seals rely on.

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
- Gold, corpus, and audit text mappings now prefer recognized column names;
  the bundled gold data therefore start with `text` and `label` selected.
- Primary run and continuation controls are disabled while their known
  prerequisites are incomplete and show the requirement beside the control.
  The validation step names the locked protocol and short hash and places its
  ledger confirmation beside the validation action.
