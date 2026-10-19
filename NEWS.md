# LLMRcontent 0.2.2

* `audit_run()` accepts a cell's estimate only when the estimator returns
  one finite number, and records each cell's outcome in the new columns
  `estimate_status` and `estimate_note`. Four kinds of return used to pass
  without notice: a vector estimate was cut to its first element, a
  character value was converted with `as.numeric()`, an infinite estimate
  counted as valid in `audit_stability()` and `audit_fragility()`, and an
  estimator error became `NA` with its message discarded. Such cells
  have `estimate = NA`, and the warning "The estimator failed in k of n
  audit cell(s)." names the first failure. An estimate of `NA` or `NaN`
  has status `"missing"` and raises no warning.
* Audit cells gain an `n_labels` column, the number of parsed labels the
  estimator received.
* `audit_placebo(type = "label_permutation")` applies the same rule to
  each permuted estimate, keeps the first reason in the new `perm_note`
  column, and warns "k permutation estimate(s) produced no usable value
  and were excluded." Vector and infinite permuted estimates used to enter
  the permutation distribution, and errors were counted without a reason.
* The Studio's "most common label is the target" estimand returns `NA`
  when no label is parsed or the most frequent labels tie. It used to
  return 0 in both cases, breaking ties alphabetically, so a cell whose
  replies all failed to parse could flip the sign of a reference estimate
  of 1.
* When the reference estimate is 0, the Studio's audit summary says that
  the reference has no sign, so no cell can change it, and for the
  most-common-label estimand that a cell estimate of 1 is not counted as a
  change; it omits the share of cells with the reference sign. The summary
  used to say "No cell in this grid changes the reference sign.", which
  read as a stable result.
* The Studio's audit summary begins "Across the k of n cells with an
  estimate" when some cells have no estimate. It used to say "Across n
  cells" although the range and the sign share cover only the cells with an
  estimate.
* Token usage that the runner does not report is unknown, not zero. A
  call's tokens are the runner's `total_tokens` when it reports one, and
  otherwise `sent_tokens` plus `rec_tokens` when both are reported. The
  `tokens` values of tuning tables, validation results, and audit cells sum
  the calls with known totals and are `NA` when no call has one. Tuning
  results gain a `usage` table, validation results gain `calls` and
  `tokens_unknown`, and audit cells gain `tokens_unknown`. `tokens` used to
  be an integer sum of `sent_tokens` and `rec_tokens` that read missing
  counts as 0 and ignored `total_tokens`, so usage that was never reported
  showed as 0 and a call with one count missing contributed the other.
  `tokens` is a double.
* Token count columns that are not numeric, or that hold negative,
  fractional, or infinite values, are treated as unknown, with the warnings
  "The runner's sent_tokens column is <character>, not numeric; its counts
  are treated as unknown." and "The runner's sent_tokens column contains
  invalid counts; they are treated as unknown." (and likewise for
  `rec_tokens` and `total_tokens`).
* The archive appendix reports sent and received tokens separately and
  counts calls with incomplete usage. It used to print usage logged as
  `null` as "0 sent, 0 received".
* The Studio counts usage from the calls each run made, including runs that
  fail, and requires LLMR.shiny 0.1.3 or later. The validation tab used to
  pass only a call count, so the usage tile showed 0 tokens sent and
  received after live audit calls.
* When the runner reports a failed call or returns a missing response,
  `code_corpus()`, `tune_protocol()`, `validate_protocol()`, and
  `audit_run()` stop with an error of class `llmrcontent_run_failure`. Its
  `completed` field holds the runner rows that succeeded, earlier replicate
  passes included, and its `failed` field identifies the failed calls, by
  the unit's `id` value when the corpus or gold set declares one. The error
  used to carry only a count of failed rows, and the work of the whole run
  was lost when one call failed.
* The Studio's robustness audit refuses a temperature field in which a
  token is not a decimal number or lies outside the provider's range, 0 to
  1 for Anthropic and 0 to 2 for other providers (the range to which
  `llm_config()` clamps; `audit_run()` sets grid temperatures directly).
  The sidebar temperature, which joins the grid, is held to the same
  range. The Run audit button is disabled with a reason that names the
  token, such as
  "The audit is disabled until the temperatures are numbers from 0 to 2;
  '0.7x' is not a number." The field used to drop tokens that did not parse
  and keep the rest: "0, 0.7x, 1" built arms for temperatures 0 and 1,
  "0,0,7" (a decimal comma) built arms for 0 and 7, and "0, -1, 5" built
  arms for all three.
* The `table` and `usage` fields of `tune_protocol()` results gain a last
  column, `protocol_id`, each protocol's position in `protocols`, which the
  sort by accuracy keeps. The README selected the winner with
  `candidates[[tuning$table$protocol[[1]]]]`, which finds the right
  protocol only when the list names equal the labels, and the labels are
  made unique with suffixes such as `.1` when protocols share one. The
  README and the usage capsule select by `protocol_id`.
* The getting-started vignette starts from a finite corpus, draws the
  audited statements from it by simple random sampling, and splits them
  with `stratify = FALSE`, so that the test split is a simple random sample
  of the corpus. It describes the estimator `gold_correct()` implements: the
  corpus share of model labels plus the mean paired difference between the
  human and model indicators. The vignette used to form its corpus by
  appending new statements to the gold data, split them with the default
  stratification by label, and describe a confusion-matrix adjustment, so
  its own `design = "srs"` assertion did not hold.
* `code_corpus()` refuses an `id` column with missing or repeated values
  before any call, with the error "`id` column 'doc' must have no NA and no
  duplicate values in `corpus`: ...", which counts the missing ids and names
  the repeated ones. Such a corpus used to be coded, and the problem
  surfaced only in `gold_correct()`. The `gold_set()` error for the same
  problem names the ids as well.
* `gold_set()` refuses a `split` whose names are missing, empty, or
  repeated, or whose proportions are not finite and nonnegative, with
  errors that name the entries ("`split` proportions must be finite and
  nonnegative: test = -0.5."). A missing or negative proportion used to
  fail with an unrelated error from the allocation, a repeated name put
  every unit in one split, and an empty name created a split named "". A
  sum other than 1 is reported with the actual sum. Valid specifications
  allocate units as before.
* The `gold_split()` error for a split with no units, "No split named
  'test' in this gold set.", is followed by the units by split and label,
  so that a split the allocation left empty can be told from a misspelled
  name.
* The Studio reads uploaded and demo CSV files with every column as text,
  so identifiers keep their leading zeros and all their digits. Once the
  columns are mapped, identifier columns (the mapped id column and columns
  named like `id`, `doc_id`, or `response_id`) stay as written, the text and
  label columns stay character, with blank labels missing, and the other
  columns are converted as `read.csv()` converts them. The Studio used to infer every column's type
  on reading, so `007`, `07`, and `7` became one number and a gold set with
  such identifiers was refused for duplicates, a gold unit could link to a
  different corpus row, and a 20-digit identifier lost its last digits. The
  gold, corpus, audit, and archive-results uploads are affected.
* A demo export from the Studio's coding tab writes `coded.csv` once, with
  the `demo_notice` column added to the coded rows. The export used to write
  the file, read it back, and write it again with the notice. When the
  re-read failed, the file went out without its notice and no message was
  shown, and the re-read inferred column types, so an identifier such as
  `007` was written as `7`.
* Replicate passes whose runner rows omit a column are bound with missing
  values of that column's type. When the first pass lacked a date-time
  column, its values used to become plain numbers, and a missing list entry
  used to be a logical `NA`. Runner columns without a name are left out of
  the bound rows.

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

Not published. The July 2026 CRAN pretest declined this version because the
LLMR.shiny then on CRAN did not export four objects the Studio uses.

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
