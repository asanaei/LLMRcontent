# LLMRcontent <img src="man/figures/logo.png" align="right" width="120" alt="LLMRcontent icon" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/asanaei/LLMRcontent/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/asanaei/LLMRcontent/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Website](https://img.shields.io/badge/docs-pkgdown-blue.svg)](https://asanaei.github.io/LLMRcontent/)
<!-- badges: end -->

LLM-assisted content analysis for the social sciences, built on
[LLMR](https://github.com/asanaei/LLMR). Three connected concerns, one package:
coding, robustness audits, and replication archives. When a text label becomes a
variable in quantitative analysis, this is the workflow that keeps the
measurement honest.

## The three workflows

**Coding** turns a codebook and a sealed gold standard into error-corrected
category prevalences:

```r
g <- gold_set(data, text = "text", labels = "label")        # sealed test split
p <- protocol_lock(protocol(codebook, config))              # hash-locked instrument
v <- validate_protocol(p, g)                                # the one honest evaluation
coded <- code_corpus(corpus, p, "text")                     # apply to the full corpus
gc <- gold_correct(coded, g)                                # corrected prevalences + SEs
```

**Robustness audits** ask whether a coded conclusion survives the measurement
multiverse of prompts, models, label orders, and temperatures:

```r
plan <- audit_plan(data, "text", estimator, labels, prompt)
plan <- audit_add_models(plan, configs)
plan <- audit_add_perturbations(plan, label_order = c("as_given", "reversed"),
                                temperature = c(0, 0.7))
a <- audit_run(plan)
audit_stability(a); audit_fragility(a)                      # sign, rank, fragility
```

**Archives** turn the audit log that LLMR writes into a reviewer-runnable
replication record:

```r
ar <- archive_build(log)                                    # content-addressed
ar <- archive_seal(ar)                                      # frozen under a root hash
archive_check(ar)                                           # integrity + completeness
replay <- archive_replay(ar)                                # recompute offline, no keys
```

The shared generics `LLMR::diagnostics()`, `LLMR::report()`, and
`tibble::as_tibble()` dispatch across all three families of result objects.

## Point-and-click

The same three workflows run from a Shiny GUI, for collaborators who would
rather not write R:

```r
install_gui_deps()        # shiny, bslib, DT, and the LLMR.shiny substrate
run_content_studio()      # coding, robustness audit, and archive tabs
```

The GUI wraps the package API rather than reimplementing it, reads provider keys
from environment variables only, and has a deterministic demo mode that runs
offline.

## Scope

For accessible qualitative coding use `quallmer`; use LLMRcontent when the label
becomes a variable in quantitative analysis. A coding result is a measurement
with known error, an audit is a fragility statement, and an archive is a
verifiable record -- none of them a claim of truth.

## Install

```r
remotes::install_github("asanaei/LLMR")
remotes::install_github("asanaei/LLMRcontent")
```
