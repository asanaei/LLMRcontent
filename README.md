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

**Coding** evaluates a locked protocol against held-out human labels and applies
it to a corpus. `gold_correct()` uses matched holdout errors to estimate
corrected category prevalences:

```r
g <- gold_set(data, text = "text", labels = "label")        # sealed test split
p <- protocol_lock(protocol(codebook, config))              # hash-locked instrument
v <- validate_protocol(p, g)                                # evaluate on the sealed split
coded <- code_corpus(corpus, p, "text")                     # apply to the full corpus
gc <- gold_correct(coded, g)                                # corrected prevalences + SEs
```

**Robustness audits** recompute an estimator for each selected prompt, model,
label order, and temperature. `audit_stability()` and `audit_fragility()`
summarize how the estimates change:

```r
plan <- audit_plan(data, "text", estimator, labels, prompt)
plan <- audit_add_models(plan, configs)
plan <- audit_add_perturbations(plan, label_order = c("as_given", "reversed"),
                                temperature = c(0, 0.7))
a <- audit_run(plan)
audit_stability(a); audit_fragility(a)                      # sign, rank, fragility
```

**Archives** store LLMR audit logs for checking and replay. `archive_build()`
reads a log into an archive that can be sealed and checked:

```r
ar <- archive_build(log)                                    # content-addressed
ar <- archive_seal(ar)                                      # frozen under a root hash
archive_check(ar)                                           # integrity + completeness
replay <- archive_replay(ar)                                # recompute offline, no keys
```

`LLMR::diagnostics()` returns machine-readable summaries for validation,
correction, audit, and archive objects. `LLMR::report()` creates draft reports
for validation, audit, and archive objects. `tibble::as_tibble()` extracts
tabular results.

## Shiny interface

`run_content_studio()` provides a Shiny interface to the three workflows:

```r
install_gui_deps()        # shiny, bslib, DT, ggplot2, and the LLMR.shiny substrate
run_content_studio()      # coding, robustness audit, and archive tabs
```

Live runs read provider keys from environment variables. Demo mode uses an
offline runner.

## Scope

`quallmer` supports qualitative coding. LLMRcontent is intended for analyses
that use model labels as variables in quantitative estimates.

## Install

```r
install.packages("LLMR")                     # on CRAN
remotes::install_github("asanaei/LLMRcontent")
```
