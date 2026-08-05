## Resubmission

Resubmission of LLMRcontent, bumped to 0.2.1 (0.2.0 was never published).
The July pretest declined 0.2.0 with a WARNING because four objects its GUI
uses were not exported by the LLMR.shiny then on CRAN; LLMR.shiny 0.1.2,
which exports them, is on CRAN now, and the Suggests entry states the
requirement (LLMR.shiny >= 0.1.2).

Version 0.2.1 also incorporates corrections from an external methodological
review: replicate ties yield NA rather than an arbitrary pick, agreement
shares count parse failures, prevalence correction requires a declared
sampling design and refuses double-counted audit units, archives are sealed
under a content root plus a manifest-binding seal root with cardinality
checks, accuracy intervals are exact rather than bootstrap, and result
columns refuse to overwrite user data. The LLMR floor rose to 0.8.11, the
first release with locale-independent hashing, which protocol locks and
archive seals rely on.

The pretest NOTE flagged "LLM" in the Description as possibly misspelled;
it is the standard initialism for large language model.

## The package

LLMRcontent provides LLM-assisted content analysis for the social sciences,
built on LLMR. Codebook coding is validated against held-out human labels
with error-corrected prevalences, measurement-multiverse robustness audits,
and content-addressed replication archives, with an optional Shiny GUI.

The package Imports LLMR (>= 0.8.11), which is on CRAN. It Suggests
LLMR.shiny (>= 0.1.2), the family's shared GUI substrate; every use of
LLMR.shiny, and of the other GUI packages shiny, bslib, DT, and ggplot2, is
guarded with requireNamespace(), so the package installs, checks, and runs
without any of them. All tests run offline against injected mock runners; no
test, example, or check step makes a network call or needs an API key.

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
