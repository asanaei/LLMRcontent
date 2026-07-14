---
name: llmrcontent
description: LLM-assisted content analysis for the social sciences in R, built on LLMR. One package, three connected concerns. Coding turns a codebook and a sealed gold split into error-corrected category prevalences validated against held-out human labels, with locked protocols and development-split tuning. Robustness audits recompute the reported estimate across the measurement multiverse of prompts, model families, label order, and temperature, with stability metrics, spec curves, and a fragility index. Archives turn the LLMR audit log of a study into a content-addressed, sealed, reviewer-runnable replication record with completeness linting, IRB-grade redaction, and a verifiability horizon.
---

# LLMRcontent usage capsule for AI assistants

This file is the compact manual: enough to use the package correctly
without reading every help page. `vignette("design", package =
"LLMRcontent")` goes deeper.

LLMRcontent merges three former packages into one workflow. Coding (once
`LLMRcoder`) makes an LLM label a trustworthy variable; robustness audits
(once `LLMRvalid`) ask whether the conclusion drawn from that variable
survives the measurement multiverse; archives (once `LLMRarchive`) turn the
study's call log into a verifiable replication record. The three sections
below share one offline seam (`.runner`) and one LLMR foundation, so a study
can run coding, audit the result, and archive the whole thing without
leaving the package.

## Install

```r
remotes::install_github("asanaei/LLMRcontent")   # depends on LLMR (>= 0.8.6)
```

An optional Shiny GUI ships with the package. Install its extra
dependencies with `install_gui_deps()`, then launch it with
`run_content_studio()`. The GUI is for interactive exploration; the API
below is what you generate code against.

## Coding

Use the coding surface when an LLM label becomes a variable in quantitative
analysis. The canonical order is `gold_set()` -> `protocol_lock()` ->
`validate_protocol()` -> `gold_correct()`: build a gold split, lock the
codebook protocol, validate once on the sealed holdout split, code the
corpus, and correct category prevalences with the audit. For accessible qualitative
coding use 'quallmer'; use this surface when the label feeds inference.

### Core API (exact signatures)

```r
cb_category(label, definition, include = NULL, exclude = NULL,
            examples = NULL, counterexamples = NULL)
codebook(name, unit, categories, instructions = NULL, version = "1.0")
codebook_labels(x); codebook_hash(x); format_codebook(x)

gold_set(data, text, labels, split = c(dev = 0.6, test = 0.4),
         holdout = "test", stratify = TRUE, seal_test = TRUE,
         coders = NULL, id = NULL)
gold_split(x, split = "dev"); gold_ledger(x)
gold_size(expected_agreement = 0.85, ci_width = 0.10, conf = 0.95,
          n_grid = c(50, 100, 200, 300, 500, 800), sims = 2000)

protocol(codebook, config, prompt = NULL, parser = parse_label(),
         replicates = 1L, label = NULL)
protocol_lock(x)

tune_protocol(protocols, gold, split = "dev", .runner = NULL, ...)
validate_protocol(protocol, gold, split = NULL, .runner = NULL, ...)
  # split = NULL means the gold set's holdout split ("test" by default)
code_corpus(corpus, protocol, text, .runner = NULL, id = NULL, ...)

gold_correct(coded, gold, conf = 0.95)

coder_agreement(x, cols = NULL)
coding_report(validation, gold, protocol)
export_caqdas(coded, path, format = c("csv", "jsonl"))

LLMR::diagnostics(x, ...)
LLMR::report(x, ...)
tibble::as_tibble(x, ...)
```

### Canonical workflow

```r
library(LLMRcontent)

gold <- gold_set(labeled_df, text = "text", labels = "label")

cb <- codebook("tone", "one sentence",
  list(cb_category("positive", "Approving or hopeful."),
       cb_category("negative", "Critical or alarmed.")))

cfg <- LLMR::llm_config("groq", "openai/gpt-oss-20b", temperature = 0)
ps  <- list(protocol(cb, cfg, label = "base"),
            protocol(cb, cfg, prompt = "{codebook}\n\n{text}\nLabel:",
                     label = "terse"))

tuning <- tune_protocol(ps, gold)        # tune freely; dev split only
winner <- protocol_lock(ps[[1]])         # hash covers prompt+model+params+parser
v <- validate_protocol(winner, gold)     # one honest test-split evaluation
coded <- code_corpus(big_df, winner, "text")
correction <- gold_correct(coded, gold)  # corrected prevalences with SEs

LLMR::diagnostics(v)
LLMR::diagnostics(correction)
LLMR::report(v, gold = gold, protocol = winner)
tibble::as_tibble(tuning)
tibble::as_tibble(correction)
```

### Coding rules

- Tune on `split = "dev"` only; tuning on the gold set's holdout split
  (named at creation via `holdout =`, `"test"` by default) is refused.
- Lock before the holdout split or the corpus; `validate_protocol()` and
  `code_corpus()` refuse unlocked protocols.
- Do not edit prompt, parser, model, parameters, or replicates after
  validation. That voids the hash; re-lock and re-validate instead.
- Every holdout-split evaluation lands in `gold_ledger()` and prints in the
  report.
- Parse failures (`NA` labels) count as errors; never silently drop them.
- `temperature = 0` for annotation unless replicate variability is the object.
- Custom prompts must contain `{text}`; `{codebook}` inserts `format_codebook()`.
- `LLMR::report()` on a `protocol_validation` needs `gold =` and
  `protocol =` so it can delegate to `coding_report()`.

### Coding error meanings

- "Refusing to evaluate an unlocked protocol" means call `protocol_lock()` first.
- "The holdout split (...) is sealed for tuning" means tune on dev; validate once.
- "Gold labels contain NA" means adjudicate or drop those rows before `gold_set()`.
- "must contain the {text} placeholder" means fix the prompt template.
- Many `NA` labels in output means model replies do not match
  `codebook_labels()`; tighten the prompt's final instruction or the parser.
- "No gold units from the holdout split (...) matched the coded corpus" means
  `gold_correct()` links gold units to corpus rows by a shared `id` when both
  carry one, and by a hash of the text otherwise; either way the audited
  units must be part of the coded corpus.

## Robustness audits

A validated coding result is a measurement; the audit asks whether the
conclusion drawn from it survives the measurement multiverse. It recomputes
the reported estimate across prompts, model families, label order, and
temperature, then reports stability, a spec curve, and a fragility index.
Honest prompt improvement is tuning and belongs in the coding tournament
above, before the audit.

### Core API (exact signatures)

```r
audit_plan(data, text, estimator, labels, prompt)
audit_add_models(plan, configs)              # NAMED list of llm_config objects
audit_add_prompts(plan, ...)                 # named templates with {text}
audit_add_perturbations(plan, label_order = NULL, temperature = NULL)

audit_run(plan, .runner = NULL, ...)         # the full grid, one estimate per cell
audit_units(audit)                           # unit-level labels per cell (+ response_id)
audit_stability(audit, reference = 1L)
audit_curve(audit, plot = interactive())
audit_fragility(audit, reference = 1L, flip = sign_flip())
sign_flip(); threshold_flip(at)              # pluggable conclusion rules
audit_placebo(audit, type = c("label_permutation", "irrelevant_text"),
              reps = 200L, texts = NULL, .runner = NULL, ...)
audit_report(audit, ...)
LLMR::diagnostics(audit)
LLMR::report(audit)
tibble::as_tibble(audit)
```

### Estimator contract

`estimator` is a function receiving `data` with one added character column
`label` and returning ONE number -- whatever the paper reports.
`label` MAY CONTAIN `NA` (parse failures): the estimator must decide their
meaning explicitly. If the estimator errors in a cell, that cell's estimate
is `NA` and is counted; the grid does not abort.

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
audit_fragility(audit)        # smallest #choices that flips the conclusion
LLMR::diagnostics(audit)
tibble::as_tibble(audit)
set.seed(110)
audit_placebo(audit)          # permutation null per cell; zero new calls
audit_report(audit)
LLMR::report(audit)
```

### Audit rules

- Cell 1 (baseline prompt, first model, "as_given", first temperature) is
  the reference; order your inputs accordingly.
- Model FAMILIES, not sizes of one family -- same-family agreement is
  family resemblance, not robustness.
- Prompt variants must be honest paraphrases; improving the prompt is
  tuning and belongs in the coding tournament above, before the audit.
- `sign_flip()` flags nothing when the reference estimate is exactly 0
  (no sign to flip); use `threshold_flip(at =)` there and for one-sided
  or bounded estimands.
- `audit_fragility() == Inf` is a statement about THIS grid, not a
  robustness guarantee; the report says so verbatim.
- `audit_placebo(type = "label_permutation")` flags pure-marginal estimands
  as `degenerate` with `p = NA`; that flag is the correct output, not a
  failure. `type = "irrelevant_text"` needs researcher-supplied
  construct-free `texts` and re-runs the grid (costs calls unless a
  `.runner` is injected). Set a seed before either for reproducibility.
- No "passed" verdicts exist anywhere; report the distribution.

### Audit error meanings

- "The plan has no models" -> `audit_add_models()` before `audit_run()`.
- "must contain the {text} placeholder" -> fix the template.
- "must be a *named* list" -> name every config; names appear in reports.
- "`texts` is required" -> supply construct-free texts for the
  irrelevant-text placebo; the package cannot invent them.

## Archives

Once a study has run its coding and audit, the archive turns the LLMR call
log into a verifiable replication record: content-addressed records, a
sealed root, completeness linting, IRB-grade redaction, and a verifiability
horizon.

### Precondition

The substrate is LLMR's audit log. Before the study:

```r
LLMR::llm_log_enable("study.jsonl")    # every call recorded; schema_version pinned
# ... all coding and audit calls ...
LLMR::llm_log_disable()
```

### Core API (exact signatures)

```r
archive_build(log, name = NULL)         # parse JSONL -> content-addressed archive
archive_current(name = NULL)            # archive whatever log is active now
archive_seal(archive)                   # one root hash; cite it in the paper
archive_check(archive, results = NULL)  # integrity + completeness linting
archive_redact(archive)                 # strip text, keep hash families
verifiability_horizon(archive, open_patterns)  # open_patterns defaults to an open-weight name regex
archive_appendix(archive, ...)          # the reproducibility appendix
archive_diff(a, b)                      # compare runs by request identity
archive_write(archive, dir); archive_read(dir)

archive_replay(archive)                 # runner that answers calls from the archive
archive_verify(archive, sample = 0.05,  # re-issue a stratified sample live
               strata = c("provider", "model"), .runner = NULL, ...)
reset(x, ...)                           # reset an archive replayer

LLMR::diagnostics(archive)              # one-row machine-readable summary
LLMR::report(archive)                   # delegates to archive_appendix()
tibble::as_tibble(archive)              # manifest tibble
```

### Canonical workflow

```r
library(LLMRcontent)
a <- archive_seal(archive_build("study.jsonl", name = "smith2026"))
archive_check(a, results = my_results)  # results needs a response_id column
archive_appendix(a)                     # includes the verifiability horizon
LLMR::diagnostics(a)                    # n_records, seal, redaction, horizon counts
archive_write(a, "replication/llm-archive")

# reviewer side:
b <- archive_read("replication/llm-archive")
archive_check(b)                        # reading and verifying are separate acts
replay <- archive_replay(b)             # pass as `.runner =` to recompute offline
```

The replayer is a `.runner`: pass it as `.runner =` to any execution
function in the package (`code_corpus()` for coding, `audit_run()` for the
audit), where it replaces the live LLMR runner, and the original study
recomputes from the archive with no keys and no spending.

### Redaction semantics (two hash families, by design)

`archive_redact()` removes prompts and replies. The ORIGINAL record hashes
and seal root stay in the manifest (attestable by whoever holds the
unredacted archive, e.g. under IRB terms); per-record PUBLIC hashes are
added so the redacted artifact passes its own `archive_check()`. Counts,
models, parameters, timings, token totals all remain public.

### Archive generic surface

- `LLMR::diagnostics(archive)` returns a one-row tibble with `n_records`,
  `sealed`, `root`, `redacted`, `n_open_pinnable`, and `n_api_contingent`.
- `LLMR::report(archive)` returns the same `archive_appendix` object as
  `archive_appendix(archive, ...)`.
- `tibble::as_tibble(archive)` returns the manifest tibble.

### Archive rules

- Seal before depositing or citing; an unsealed archive has no root.
- Never edit `records.jsonl` by hand; any edit is what `archive_check()`
  exists to catch.
- Completeness: keep `response_id` in results frames (LLMR's
  `call_llm_par()` provides it, and `audit_units()` carries it) so every
  reported number maps to a logged call.
- The horizon's open-weight classification is a name heuristic; pass
  `open_patterns` when you serve something unusual.
- `archive_replay()` matches a call by its full request hash
  (`LLMR::llm_request_hash()`): provider, model, canonical message content, and
  the generation parameters (temperature and the rest), so the same prompt at
  two temperatures does not collide. It serves repeated identical requests in
  archived order and excludes records logged without message content. It refuses
  a redacted archive (no content to replay).
- `archive_verify()` re-issues real calls; the `.runner` argument is the
  offline test seam. Exact reproduction is expected only for
  temperature-0 calls on pinned open-weight backends.

### Archive error meanings

- "already redacted" -> redaction is one-way on a given object.
- "Not an archive directory" -> point `archive_read()` at the folder
  containing `records.jsonl` + `manifest.json`.
- "log contains no records" -> enable logging before the calls, not after.
- check prints TAMPERED -> a stored line no longer matches its hash;
  diff against your deposited copy.

## Offline seam (tests, vignettes, dry runs)

All three surfaces share one seam. `.runner` replaces the network: it
receives an `experiments` tibble (`config` list-column, `messages`
list-column of named character vectors) and must return it with a
`response_text` column; `sent_tokens`/`rec_tokens` are kept as cost columns
when present, and `response_id` is carried through when supplied.

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

The archive replayer from `archive_replay()` is the natural production
`.runner`: it answers coding and audit calls from a sealed archive instead
of a stub.
