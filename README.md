# LLMRcontent <img src="man/figures/logo.png" align="right" width="120" alt="LLMRcontent icon" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/asanaei/LLMRcontent/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/asanaei/LLMRcontent/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Website](https://img.shields.io/badge/docs-pkgdown-blue.svg)](https://asanaei.github.io/LLMRcontent/)
<!-- badges: end -->

LLMRcontent uses large language models to code text for quantitative analysis.
Coding functions define and validate protocols. Audit functions recompute
estimates under specified coding choices. Archive functions preserve
[LLMR](https://github.com/asanaei/LLMR) call logs for checking and replay.

## The three workflows

**Coding** begins with a codebook and a human-labeled gold set. Candidate
protocols are compared on the development split, while the sealed holdout is
reserved for the locked protocol. `gold_correct()` uses matched holdout errors
to estimate corrected category prevalences:

```r
cb <- codebook(
  "policy appraisal", "one statement",
  list(cb_category("supportive", "Presents the proposal as beneficial."),
       cb_category("critical", "Presents the proposal as harmful or deficient."))
)
set.seed(110)
g <- gold_set(data, text = "text", label = "label")
size <- gold_size(expected_agreement = 0.85, ci_width = 0.10)

candidates <- list(
  baseline = protocol(cb, config, label = "baseline"),
  concise = protocol(cb, config, prompt = "{codebook}\n\n{text}\nLabel:",
                     label = "concise")
)
tuning <- tune_protocol(candidates, g)                       # development split
p <- protocol_lock(candidates[[tuning$table$protocol[[1]]]]) # selected protocol
v <- validate_protocol(p, g)                                 # sealed holdout
coded <- code_corpus(corpus, p, "text")                      # full corpus
gc <- gold_correct(coded, g)                                 # prevalence + SEs
```

**Robustness audits** recompute an estimator for each selected prompt, model,
label order, and temperature. `audit_stability()` and `audit_fragility()`
summarize how the estimates change:

```r
plan <- audit_plan(data, "text", estimator, labels, prompt)
plan <- audit_add_models(plan, config)
plan <- audit_add_perturbations(plan, label_order = c("as_given", "reversed"),
                                temperature = c(0, 0.7))
a <- audit_run(plan)
audit_stability(a)
audit_fragility(a)                                          # flipping cells + distance
```

**Archives** store LLMR audit logs for checking and replay. `archive_build()`
reads a log into an archive that can be sealed and checked:

```r
ar <- archive_build(log)                                    # records use content hashes
ar <- archive_seal(ar)                                      # frozen under a root hash
archive_check(ar)                                           # integrity + completeness
replay <- archive_replay(ar)                                # recompute offline, no keys
archive_drift(ar, n = 10)                                   # optional live drift sample
```

`LLMR::diagnostics()` returns machine-readable summaries for validation,
correction, audit, and archive objects. `LLMR::report()` creates draft reports
for validation, audit, and archive objects. Result objects retain the inputs
and run records needed to interpret them: tuning has `table`, `per_category`,
and `split`; audits have `cells`, `units`, and `plan`.
`tibble::as_tibble()` extracts their main tables.

## Shiny interface

`run_content_studio()` provides a Shiny interface to the three workflows:

```r
install.packages(c("shiny", "bslib", "DT", "ggplot2", "LLMR.shiny"))
run_content_studio()      # coding, robustness audit, and archive tabs
```

Live runs read provider keys from environment variables. Demo mode uses
offline response rows.

## Scope

LLMRcontent is intended for analyses that use model labels as variables in
quantitative estimates: each unit receives one label from a fixed codebook,
and the labels feed prevalence estimates or downstream models. Exploratory
or interpretive coding, multi-field extraction, and segmentation are outside
its scope.

## Install

```r
install.packages("LLMR")                     # on CRAN
remotes::install_github("asanaei/LLMRcontent")
```

## Offline execution for examples and tests

Coding and audit functions accept a `.runner` interface for supplying response
rows. Live analyses use the configured provider call by default; an injected
function supports examples and tests without making a provider call.
