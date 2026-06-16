# LLMRcontent 0.2.0

* Absorbed the Shiny GUI that previously shipped as the separate LLMRstudio
  package. Launch it with `run_content_studio()`; install its suggested
  packages (shiny, bslib, DT, and the LLMR.shiny substrate) with
  `install_gui_deps()`. The GUI is guarded with `requireNamespace()`, so non-GUI
  users install none of those dependencies.

# LLMRcontent 0.1.0

* First release. A validated workflow for content analysis with large language
  models: codebook coding with sealed gold-set validation and error-corrected
  prevalences, measurement-robustness audits with a fragility index, and
  verifiable replication archives from audit logs. The package merges the work
  of the then-separate LLMRcoder, LLMRvalid, and LLMRarchive packages.
