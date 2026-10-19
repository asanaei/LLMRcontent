## Submission

This is a new submission of LLMRcontent, version 0.2.2. The package is not
on CRAN.

Version 0.2.0 was submitted in July 2026. The pretest declined it with a
WARNING because four objects its GUI uses were not exported by the
LLMR.shiny then on CRAN, and that version was never published. Version
0.2.1 incorporated corrections from an external methodological review, and
0.2.2 corrects estimator-failure handling, unknown token usage, recovery of
completed calls when a run fails, input checks, and how the GUI reads
identifiers from CSV files.

The package Suggests LLMR.shiny (>= 0.1.3). That version exports the
objects the GUI uses and the CSV reader options it calls, and it is
submitted to CRAN ahead of this package.

The pretest NOTE flagged "LLM" in the Description as possibly misspelled;
it is the standard initialism for large language model.

## The package

LLMRcontent provides LLM-assisted content analysis for the social sciences,
built on LLMR. Codebook coding is validated against held-out human labels
and yields error-corrected prevalences; robustness audits recompute an
estimate across measurement choices; replication archives store the
underlying call records. An optional Shiny GUI calls the same functions.

The package Imports LLMR (>= 0.8.11), which is on CRAN. Every use of
LLMR.shiny, and of the other GUI packages shiny, bslib, DT, and ggplot2, is
guarded with requireNamespace(), so the package installs, checks, and runs
without any of them. All tests run offline against injected mock runners;
no test, example, or check step makes a network call or needs an API key.

## Test environments

- local macOS 26.7.1 (arm64), R 4.4.3, R CMD check --as-cran, with LLMR
  0.8.14 and LLMR.shiny 0.1.3 installed
- the same machine with LLMR 0.8.11, the declared minimum: Status OK

## R CMD check results

0 errors | 0 warnings | 2 notes

- "checking CRAN incoming feasibility ... NOTE: New submission".
- "checking for future file timestamps ... NOTE: unable to verify current
  time". Environmental; the check machine had no access to a time service.

## Reverse dependencies

None; this is a new package.
