# LLMRcontent 0.2.0

Initial CRAN release.

- Codebook-first coding validated against a sealed gold standard, with
  error-corrected category prevalences: `gold_set()`, `protocol_lock()`,
  `validate_protocol()`, `code_corpus()`, `gold_correct()`.
- Measurement-multiverse robustness audits across prompts, models, label
  orders, and temperatures, with stability and fragility summaries:
  `audit_plan()`, `audit_run()`, `audit_stability()`, `audit_fragility()`.
- Content-addressed, sealed replication archives built from LLMR audit logs,
  with offline replay and integrity checks: `archive_build()`,
  `archive_seal()`, `archive_check()`, `archive_replay()`.
- Optional Shiny GUI (`run_content_studio()`) running the same workflow
  interactively; every GUI dependency is a guarded Suggests.
