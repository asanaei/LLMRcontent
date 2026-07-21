---
name: llmrcontent
description: >-
  LLMRcontent codes text with language models for quantitative content analysis.
  It validates coding protocols against held-out human labels and estimates
  corrected category prevalences. It recomputes an estimator under alternative
  coding specifications. It stores LLMR call logs for checks and offline replay.
---

# LLMRcontent usage reference

This reference lists the main workflows, object contracts, and common errors.
See `vignette("getting-started", package = "LLMRcontent")` for a complete
offline example.

LLMRcontent provides three related workflows. The coding workflow defines and
validates a protocol before applying it to a corpus. The audit workflow
recomputes an estimator under alternative prompts, models, label orders, and
temperatures. The archive workflow stores LLMR call records and supports
integrity checks and offline replay. The execution functions use the same
`.runner` contract.

## Install

```r
remotes::install_github("asanaei/LLMRcontent")   # depends on LLMR (>= 0.8.6)
```

The optional Shiny interface calls the same package functions. Install its
dependencies with
`install.packages(c("shiny", "bslib", "DT", "ggplot2", "LLMR.shiny"))`,
then start it with `run_content_studio()`. Use the functions below in scripts.

## Coding

The coding workflow estimates category prevalences from model labels.
`gold_set()` divides human-labeled units into development and holdout splits.
Use `tune_protocol()` on the development split when comparing protocols, lock
the selected protocol with `protocol_lock()`, and evaluate it with
`validate_protocol()`. Then apply it with `code_corpus()` and pass the result
to `gold_correct()` to estimate corrected prevalences and standard errors.

### Core API (exact signatures)

```r
cb_category(label, definition, include = NULL, exclude = NULL,
            examples = NULL, counterexamples = NULL)
codebook(name, unit, categories, instructions = NULL, version = "1.0")
codebook_labels(x); codebook_hash(x); format_codebook(x)

gold_set(data, text, label, split = c(dev = 0.6, test = 0.4),
         holdout = "test", stratify = TRUE, seal_holdout = TRUE,
         coders = NULL, id = NULL)
gold_split(x, split = "dev"); gold_ledger(x)
gold_size(expected_agreement = 0.85, ci_width = 0.10, conf = 0.95,
          n_grid = c(50, 100, 200, 300, 500, 800), sims = 2000)

protocol(codebook, config, prompt = NULL, parser = NULL,
         replicates = 1L, label = NULL)
protocol_lock(x)

tune_protocol(protocols, gold, split = "dev", .runner = NULL, ...)
validate_protocol(protocol, gold, split = NULL, .runner = NULL, ...)
  # split = NULL means the gold set's holdout split ("test" by default)
code_corpus(corpus, protocol, text, id = NULL, .runner = NULL, ...)

gold_correct(coded, gold, conf = 0.95)

coder_agreement(x, cols = NULL)

LLMR::diagnostics(x, ...)
LLMR::report(x, ...)
tibble::as_tibble(x, ...)
```

`tune_protocol()` returns `table`, `per_category`, and `split` fields.
`code_corpus()` returns `data`, protocol identifiers, the text and label column
names, the optional id column name, and the codebook labels as ordinary fields.
Use `tibble::as_tibble()` to extract either main table.

### Canonical workflow

```r
library(LLMRcontent)

gold <- gold_set(labeled_df, text = "text", label = "label")

cb <- codebook("tone", "one sentence",
  list(cb_category("positive", "Approving or hopeful."),
       cb_category("negative", "Critical or alarmed.")))

cfg <- LLMR::llm_config("groq", "openai/gpt-oss-20b", temperature = 0)
ps  <- list(protocol(cb, cfg, label = "base"),
            protocol(cb, cfg, prompt = "{codebook}\n\n{text}\nLabel:",
                     label = "terse"))

tuning <- tune_protocol(ps, gold)        # compare protocols on the dev split
winner <- protocol_lock(ps[[1]])         # hash covers prompt+model+params+parser
v <- validate_protocol(winner, gold)     # evaluate on the test split
coded <- code_corpus(big_df, winner, "text")
correction <- gold_correct(coded, gold)  # corrected prevalences with SEs

LLMR::diagnostics(v)
LLMR::diagnostics(correction)
LLMR::report(v, gold = gold, protocol = winner)
tibble::as_tibble(tuning)
tibble::as_tibble(correction)
tibble::as_tibble(coded)
```

### Coding rules

- `tune_protocol()` refuses the split named by `holdout`; use the development
  split for protocol comparisons.
- `validate_protocol()` requires a locked protocol on the holdout split, and
  `code_corpus()` always requires a locked protocol.
- If the prompt, parser, model, parameters, or replicate count changes, lock
  and validate the revised protocol.
- Tuning, validation, and corpus coding use the protocol's replicate count and
  take the modal parsed label for each unit.
- Evaluations of a sealed holdout split are appended to `gold_ledger()` and
  included in the coding report.
- Validation counts `NA` labels as errors. `gold_correct()` reports parse
  failures and conditions prevalence estimates on parsed corpus labels.
- Set `temperature = 0` for deterministic annotation. Use replicates when the
  analysis concerns variation across model responses.
- Custom prompts must contain `{text}`; `{codebook}` inserts `format_codebook()`.
- `LLMR::report()` on a `protocol_validation` needs `gold =` and
  `protocol =` to assemble the ledger and instrument context.
- `code_corpus()` returns a `coded_corpus`. Its `data`, `protocol_hash`,
  `protocol_label`, `text`, `label`, `id`, and `labels` fields carry the coded
  rows and linkage provenance. Use `tibble::as_tibble()` for the row table.
- `gold_size()` returns `recommended_size` and the full `candidates` grid.

### Coding error meanings

- "Refusing to evaluate an unlocked protocol" means call `protocol_lock()` first.
- "The holdout split (...) is sealed for tuning" means tune on the development
  split and use `validate_protocol()` for the holdout.
- "Gold labels contain NA" means adjudicate or drop those rows before `gold_set()`.
- "must contain the {text} placeholder" means fix the prompt template.
- Many `NA` labels in output means model replies do not match
  `codebook_labels()`; tighten the prompt's final instruction or the parser.
- "No gold units from the holdout split (...) matched the coded corpus" means
  `gold_correct()` links gold units to corpus rows by a shared `id` when both
  carry one, and by a hash of the text otherwise; either way the audited
  units must be part of the coded corpus.

## Robustness audits

`audit_plan()` defines the data, estimator, labels, and baseline prompt. Add
model configurations, prompt variants, label orders, and temperatures to the
plan, then call `audit_run()`. The result contains one estimate per grid cell.
`audit_stability()` summarizes their distribution, and `audit_fragility()`
counts how many grid dimensions separate the reference cell from the nearest
cell that changes the stated conclusion.

An `audit` stores its per-cell table in `cells`, its typed unit-level trail in
`units`, and the originating `audit_plan` in `plan`. `audit_units()` returns
the unit trail and `tibble::as_tibble()` returns the cell table.

### Core API (exact signatures)

```r
audit_plan(data, text, estimator, labels, prompt)
audit_add_models(plan, config)               # NAMED list of llm_config objects
audit_add_prompts(plan, ...)                 # named templates with {text}
audit_add_perturbations(plan, label_order = NULL, temperature = NULL)

audit_run(plan, .runner = NULL, ...)         # the full grid, one estimate per cell
audit_units(audit)                           # unit-level labels per cell (+ response_id)
audit_stability(audit, reference = 1L)
audit_curve(audit, plot = FALSE)
audit_fragility(audit, reference = 1L, flip = sign_flip())
sign_flip(); threshold_flip(at)              # pluggable conclusion rules
audit_placebo(audit, type = c("label_permutation", "irrelevant_text"),
              reps = 200L, texts = NULL, .runner = NULL, ...)
LLMR::diagnostics(audit)
LLMR::report(audit)
tibble::as_tibble(audit)
```

### Estimator contract

`estimator` receives `data` with one added character column, `label`, and must
return one numeric value. The `label` column may contain `NA` for parse
failures, so the estimator must define how to handle them. If the estimator
errors in a cell, that cell receives `estimate = NA`; the remaining cells are
still computed.

### Canonical workflow

```r
library(LLMRcontent)
speeches <- data.frame(
  text = c("cut taxes now", "shrink the state", "fund the schools",
           "expand health coverage", "deregulate energy", "raise the wage"))
audit <- audit_plan(
    data = speeches, text = "text",
    estimator = function(d) mean(d$label == "conservative", na.rm = TRUE) - 0.5,
    labels = c("conservative", "progressive"),
    prompt = "Classify as one of: {labels}.\n\n{text}\n\nLabel:") |>
  audit_add_models(list(
    oss  = LLMR::llm_config("groq", "openai/gpt-oss-20b", temperature = 0),
    qwen = LLMR::llm_config("groq", "qwen/qwen3-32b", temperature = 0))) |>
  audit_add_prompts(terse = "{labels}? {text}") |>
  audit_add_perturbations(label_order = "reversed", temperature = 0.7) |>
  audit_run()

audit_stability(audit)
fragility <- audit_fragility(audit)
fragility$fragility           # smallest number of changed choices
fragility$flipping_cells      # cells attaining that distance
LLMR::diagnostics(audit)
tibble::as_tibble(audit)
set.seed(110)
audit_placebo(audit)          # permutation null per cell; zero new calls
LLMR::report(audit)
```

### Audit rules

- Cell 1 uses the baseline prompt, first model, `"as_given"` label order, and
  first temperature. The summary functions use it as the default reference.
- Include more than one model family when the analysis concerns sensitivity
  to the choice of model family.
- Prompt variants should pose the same coding task. Develop the prompt before
  constructing the audit grid.
- `sign_flip()` flags nothing when the reference estimate is exactly 0
  (no sign to flip); use `threshold_flip(at =)` there and for one-sided
  or bounded estimands.
- `audit_fragility()$fragility == Inf` with `status = "no_flip"` means that no
  cell in the specified grid meets the selected flip rule. A failed reference
  has `status = "reference_failed"`.
- `audit_placebo(type = "label_permutation")` flags pure-marginal estimands
  as `degenerate` with `p = NA`; that flag is the correct output, not a
  failure. `type = "irrelevant_text"` needs researcher-supplied
  construct-free `texts` and re-runs the grid (costs calls unless a
  `.runner` is injected). Set a seed before either for reproducibility.
- Report the cell estimates together with the stability and fragility
  summaries.

### Audit error meanings

- "The plan has no models" -> `audit_add_models()` before `audit_run()`.
- "must contain the {text} placeholder" -> fix the template.
- "must be a *named* list" -> name every config; names appear in reports.
- "`texts` is required" -> supply construct-free texts for the
  irrelevant-text placebo; the package cannot invent them.

## Archives

`archive_build()` reads an LLMR JSONL log and stores its records and manifest.
`archive_seal()` adds a root hash, `archive_check()` checks stored record hashes
and result identifiers, and `archive_replay()` returns a runner that serves
stored responses. `archive_redact()` removes prompts and replies when those
texts cannot be distributed.

### Precondition

Enable the LLMR audit log before the first model call and disable it after the
last call:

```r
LLMR::llm_log_enable("study.jsonl")    # every call recorded; schema_version pinned
# ... all coding and audit calls ...
LLMR::llm_log_disable()
```

### Core API (exact signatures)

```r
archive_build(log, name = NULL)         # parse JSONL -> content-addressed archive
archive_seal(archive)                   # one root hash; cite it in the paper
archive_check(archive, results = NULL)  # integrity + completeness linting
archive_redact(archive)                 # strip text, keep hash families
archive_write(archive, dir, overwrite = FALSE); archive_read(dir)

archive_replay(archive, replay_mode = c("queue", "first", "strict_once"))
archive_drift(archive, fraction = NULL, n = NULL,
              strata = c("provider", "model"), .runner = NULL, ...)
LLMR::reset(x, ...)                     # reset an archive replayer

LLMR::diagnostics(archive)              # one-row machine-readable summary
LLMR::report(archive)                   # reproducibility appendix
tibble::as_tibble(archive)              # manifest tibble
```

### Canonical workflow

```r
library(LLMRcontent)
a <- archive_seal(archive_build("study.jsonl", name = "smith2026"))
archive_check(a, results = my_results)  # results needs a response_id column
LLMR::report(a)                         # includes verifiability conclusions
LLMR::diagnostics(a)                    # n_records, seal, redaction, horizon counts
archive_write(a, "replication/llm-archive")

# reviewer side:
b <- archive_read("replication/llm-archive")
archive_check(b)                        # reading and verifying are separate acts
replay <- archive_replay(b)             # pass as `.runner =` to recompute offline
```

The object returned by `archive_replay()` follows the `.runner` contract. Pass
it to `code_corpus()` or `audit_run()` through `.runner =` to use stored
responses instead of making provider calls.

### Redacted archives

`archive_redact()` removes prompts and replies. Original record hashes and the
seal root remain in the manifest. A public hash is added for each redacted
record so `archive_check()` can check the distributed files. The manifest
retains call counts, models, parameters, timings, and token totals.

### Archive generic surface

- `LLMR::diagnostics(archive)` returns a one-row tibble with `n_records`,
  `sealed`, `root`, `redacted`, `n_open_pinnable`, and `n_api_contingent`.
- `LLMR::report(archive)` returns the printable reproducibility appendix.
- `tibble::as_tibble(archive)` returns the manifest tibble.

### Archive rules

- Seal an archive before depositing it or citing its root.
- Do not edit `records.jsonl`; use `archive_check()` to compare its lines with
  the stored hashes.
- Keep `response_id` in result frames. `call_llm_par()` provides it and
  `audit_units()` carries it, allowing `archive_check()` to match result rows
  to logged calls.
- The open-weight classification in diagnostics and reports is a name
  heuristic; pass `open_patterns` through the generic when you serve something
  unusual.
- `archive_replay()` matches calls by the request hash from
  `LLMR::llm_request_hash()`, rebuilt from each stored raw log line. The hash
  covers provider, model, canonical message content, and generation parameters.
  Repeated requests are served in archived order. Records without message
  content are excluded, and redacted archives cannot be replayed.
- `archive_drift()` re-issues real calls. Supply either `fraction` or `n`; when
  neither is supplied it samples fraction 0.05. The `.runner` argument is the
  offline test seam. Exact reproduction is expected only for temperature-0
  calls on pinned open-weight backends.
- `archive_write()` refuses an existing target directory unless
  `overwrite = TRUE` is explicit.

### Archive error meanings

- "already redacted" -> redaction is one-way on a given object.
- "Not an archive directory" -> point `archive_read()` at the folder
  containing `records.jsonl` + `manifest.json`.
- "log contains no records" -> enable logging before the calls, not after.
- check prints TAMPERED -> a stored line no longer matches its hash;
  diff against your deposited copy.

## Offline seam (tests, vignettes, dry runs)

Execution functions accept a `.runner` in place of the default provider call.
The runner receives an `experiments` tibble with `config` and `messages` list
columns. It must return that data with a `response_text` column. Token columns
are retained when present, and supplied `response_id` values remain available
to functions that keep the unit-level call records. When a runner supplies a
`success` column, failed rows abort the operation rather than becoming labels
or estimates.

```r
deterministic_coder <- function(experiments, ...) {
  experiments$response_text <- ifelse(
    grepl("great|hopeful", vapply(experiments$messages, `[[`, "", "user")),
    "positive", "negative")
  experiments
}
tune_protocol(ps, gold, .runner = deterministic_coder)
audit_run(plan, .runner = deterministic_coder)
```

`archive_replay()` supplies a runner for recomputing coding and audit results
from stored responses.
