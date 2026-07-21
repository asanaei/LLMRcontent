# LLMRcontent 0.2.0

Initial CRAN release.

- `gold_set()` creates development and holdout splits from human-coded data.
  `protocol_lock()` identifies the protocol evaluated by `validate_protocol()`
  and applied by `code_corpus()`. `gold_correct()` estimates category
  prevalences and standard errors from matched holdout errors.
- `audit_plan()` defines a grid of prompts, models, label orders, and
  temperatures. `audit_run()` evaluates the estimator across the grid.
  `audit_stability()` and `audit_fragility()` summarize the estimates and
  conclusion flips.
- `archive_build()` reads LLMR audit logs into archives. Archives can be sealed,
  checked, and replayed with `archive_seal()`, `archive_check()`, and
  `archive_replay()`.
- `run_content_studio()` provides an optional Shiny interface. Its dependencies
  are listed in `Suggests`.
