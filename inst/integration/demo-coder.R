# Live integration demo for LLMRcontent: a small stance-coding pipeline run
# against a real (cheap) model. This is NOT a unit test; it lives under
# inst/integration so R CMD check never runs it and you are billed only when you
# run it yourself. It exercises the flagship coder path end to end:
#   codebook() -> gold_set() -> protocol() -> protocol_lock()
#   -> tune_protocol() (live, ranks on dev)
#   -> validate_protocol() (live, dev) -> coding_report()
#   -> code_corpus() (live, two held-out sentences: deployment coding)
#
# All sentences are authored for this demo (no copyrighted text). The two stance
# labels are short and lowercase so a small model can echo them exactly; the
# prompt forbids punctuation because the parser matches labels exactly
# (case-insensitively).
#
# Run with a key in the environment, e.g.:
#   GROQ_API_KEY=... Rscript inst/integration/demo-coder.R

# A live runner that fails LOUD: if any row's call did not succeed, stop rather
# than let credential or rate-limit failures become silent NA parse failures and
# a bogus 0.000 report.
.live_runner <- function(experiments, ...) {
  res <- LLMR::call_llm_par(experiments, progress = FALSE, max_workers = 1L, ...)
  if ("success" %in% names(res) && !all(res$success %in% TRUE)) {
    nfail <- sum(!(res$success %in% TRUE))
    stop(sprintf("%d/%d live calls failed (check the API key, model id, and rate limits).",
                 nfail, nrow(res)))
  }
  res
}

run_coder_demo <- function(provider = Sys.getenv("LLMR_DEMO_PROVIDER", "groq"),
                           model = Sys.getenv("LLMR_DEMO_MODEL", "openai/gpt-oss-20b")) {
  stopifnot(requireNamespace("LLMRcontent", quietly = TRUE))
  library(LLMRcontent)

  # Eight one-sentence units, authored here, with an unambiguous stance toward
  # cooperation vs self_reliance. Gold labels are the intended reading.
  units <- data.frame(
    text = c(
      "When neighbors pool their tools, the whole street finishes its repairs faster.",
      "A town that shares one fund can build what no single household could afford.",
      "We solve the flood problem only by digging the channel together.",
      "The choir sounds best when every voice blends into the others.",
      "Save your own coins, because no one will guard your purse for you.",
      "Each person should learn to fix the leak without waiting for help.",
      "Trust your own two hands before you lean on a neighbor.",
      "The farmer who keeps his own grain store never goes hungry in winter."
    ),
    label = c("cooperation", "cooperation", "cooperation", "cooperation",
              "self_reliance", "self_reliance", "self_reliance", "self_reliance"),
    stringsAsFactors = FALSE
  )

  cb <- codebook(
    name = "civic-stance", unit = "one sentence",
    categories = list(
      cb_category("cooperation",
                  "The sentence favors collective action, sharing, or mutual aid."),
      cb_category("self_reliance",
                  "The sentence favors individual effort, independence, or self-help.")
    ),
    version = "1.0"
  )

  # Gold set split dev/test; seal the test half so it cannot leak into tuning.
  gold <- suppressWarnings(
    gold_set(units, text = "text", labels = "label",
             split = c(dev = 0.5, test = 0.5),
             stratify = TRUE, seal_test = TRUE))

  # Reasoning models (e.g. gpt-oss-20b) spend tokens on hidden reasoning before
  # the visible answer; too small a budget yields an empty reply that parses to
  # NA. Measured on these sentences: 64 left 4/8 empty, 160 left 0/8. Use 160.
  cfg <- LLMR::llm_config(provider, model, temperature = 0, max_tokens = 160)

  # The coder prompt supports {text} and {codebook}; {codebook} expands to the
  # label list and definitions via format_codebook(). (There is no {labels}
  # placeholder on the coder side -- that one belongs to the audit API.)
  prompt <- paste0(
    "{codebook}\n\n",
    "Reply with exactly one label and nothing else. No punctuation.\n\n",
    "Sentence: {text}\nLabel:")

  p <- protocol(cb, cfg, prompt = prompt, replicates = 1L, label = "stance-v1")
  pl <- protocol_lock(p)

  # Live: rank the (single) protocol on dev, validate on dev, build the report.
  tune <- tune_protocol(list(pl), gold, split = "dev", .runner = .live_runner)
  v <- validate_protocol(pl, gold, split = "dev", .runner = .live_runner)
  rep <- coding_report(v, gold = gold, protocol = pl)

  # Live deployment coding: code two fresh, unlabeled sentences with the locked
  # protocol (exercises code_corpus() and the modal-label machinery).
  fresh <- data.frame(
    text = c("Three families lifted the fallen beam that none could move alone.",
             "Keep a spare key of your own so you never depend on a locksmith."),
    stringsAsFactors = FALSE)
  coded <- code_corpus(fresh, pl, text = "text", .runner = .live_runner)

  list(report = rep, tuning = tune, validation = v, coded = coded,
       gold = gold, config = cfg)
}

if (sys.nframe() == 0L) {
  res <- run_coder_demo()
  cat("\n==== LLMRcontent coder report ====\n"); print(res$report)
  cat("\n==== protocol tuning ====\n"); print(res$tuning)
  cat("\n==== deployment coding (code_corpus) ====\n")
  print(res$coded[, c("text", "label", "label_share")])
}
