## Resubmission

Resubmission of LLMRcontent 0.2.0. The incoming pretest of 2026-07-27 gave
one WARNING under "checking dependencies in R code": four objects used by
the optional GUI (LLMR.shiny::guess_column, LLMR.shiny::help_tip,
LLMR.shiny::llmr_theme, LLMR.shiny::text_block_output) are not exported by
LLMR.shiny 0.1.1, the version the pretest machines held. LLMR.shiny 0.1.2
exports them. The Suggests entry now states the version requirement,
LLMR.shiny (>= 0.1.2).

The accompanying NOTE flagged "LLM" in the Description as possibly
misspelled; it is the standard initialism for large language model.

## The package

LLMRcontent provides LLM-assisted content analysis for the social sciences,
built on LLMR. Codebook coding is validated against a sealed gold standard
with error-corrected prevalences, measurement-multiverse robustness audits,
and content-addressed replication archives, with an optional Shiny GUI.

The package Imports LLMR, which is on CRAN. It Suggests LLMR.shiny
(>= 0.1.2), the family's shared GUI substrate; every use of LLMR.shiny, and
of the other GUI packages shiny, bslib, DT, and ggplot2, is guarded with
requireNamespace(), so the package installs, checks, and runs without any of
them. All tests run offline against injected mock runners; no test, example,
or check step makes a network call or needs an API key.

## Test environments

- local macOS 26.5 (arm64), R 4.4.3
- R CMD check --as-cran with NOT_CRAN=false and _R_CHECK_FORCE_SUGGESTS_=false

## R CMD check results

0 errors | 0 warnings | 3 notes

- "checking CRAN incoming feasibility ... NOTE: New submission".
- "checking for future file timestamps ... NOTE: unable to verify current
  time". Environmental; the check machine had no access to a time service.
- "checking HTML version of manual ... NOTE": emitted by an older system
  `tidy` that does not recognize the HTML5 elements R generates; it does not
  reproduce on CRAN.

## Reverse dependencies

None; this is a new package.
