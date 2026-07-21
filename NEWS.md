# LLMRcontent 0.2.0

Initial CRAN release.

- `gold_set()` creates development and holdout splits from human-coded data.
  `protocol_lock()` identifies the protocol and its hash is rechecked during
  validation and corpus coding. Tuning, validation, and corpus coding apply the
  same configured replicate count and modal-label rule. `gold_correct()`
  estimates category prevalences and standard errors from matched holdout
  errors.
- `audit_plan()` defines a grid of prompts, models, label orders, and
  temperatures. `audit_run()` evaluates the estimator across the grid.
  `audit_stability()` and `audit_fragility()` summarize the estimates and
  conclusion flips.
- `archive_build()` reads LLMR audit logs into archives. Archives can be sealed,
  checked, and replayed with `archive_seal()`, `archive_check()`, and
  `archive_replay()`. Replay and integrity diagnostics derive their trusted
  records and request identities from the sealed raw log lines.
- `run_content_studio()` provides an optional Shiny interface. Its dependencies
  are listed in `Suggests`.
