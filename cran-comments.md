## Submission

Initial submission of LLMRcontent: LLM-assisted content analysis for the
social sciences, built on LLMR. Codebook coding validated against a sealed
gold standard with error-corrected prevalences, measurement-multiverse
robustness audits, and content-addressed replication archives, with an
optional Shiny GUI.

The package Imports LLMR, which is on CRAN. It Suggests LLMR.shiny (the
family's shared GUI substrate), which is being submitted in sequence; every
use of LLMR.shiny -- and of the other GUI packages shiny, bslib, DT, and
ggplot2 -- is guarded with `requireNamespace()`, so the package installs,
checks, and runs without any of them.

All tests run offline against injected mock runners; no test, example, or
check step makes a network call or needs an API key.

## Test environments

- local macOS 26.5 (arm64), R 4.4.3
- R CMD check --as-cran with NOT_CRAN=false and _R_CHECK_FORCE_SUGGESTS_=false

## R CMD check results

0 errors | 0 warnings | 2 notes

- "checking CRAN incoming feasibility ... NOTE": "New submission" (expected
  for a first submission) and "Suggests or Enhances not in mainstream
  repositories: LLMR.shiny" (see above; all uses guarded, and the check was
  run with _R_CHECK_FORCE_SUGGESTS_=false to confirm the package is clean
  without it).
- "checking for future file timestamps ... NOTE" ("unable to verify current
  time"): environmental; the check machine had no access to a time service.

## Reverse dependencies

None; this is a new package.
