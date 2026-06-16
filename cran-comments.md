## Submission

First submission of LLMRcontent (LLM-assisted content analysis: coding, robustness
audits, and replication archives). It Imports LLMR, which is submitted ahead of it;
this package is submitted only after LLMR is on CRAN.

## Test environments

- local macOS (R 4.4.3)
- R CMD check --as-cran

## R CMD check results

0 errors | 0 warnings | notes as below.

- "checking for future file timestamps ... NOTE" and "checking HTML version of
  manual ... NOTE": both environmental (a local clock artifact and an older system
  `tidy` not recognizing valid HTML5 in R's generated help); neither reproduces on
  CRAN.
- "New submission": expected for a first submission.

The `Remotes` field has been removed; LLMR is a normal CRAN dependency.

## Reverse dependencies

None. This package now bundles its own optional Shiny GUI
(`run_content_studio()`), which Suggests LLMR.shiny, shiny, bslib, and DT and is
guarded with `requireNamespace()`, so non-GUI users install none of them.
