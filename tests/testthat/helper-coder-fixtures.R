# Shared fixtures: a tiny codebook, gold set, protocols, and a fake runner
# that "codes" by string matching, so the whole execution layer runs offline.

fix_codebook <- function() {
  codebook(
    name = "tone", unit = "one sentence",
    categories = list(
      cb_category("positive", "Approving or hopeful."),
      cb_category("negative", "Critical or alarmed.")
    ),
    version = "1.0"
  )
}

fix_gold <- function(n = 8, seed = 110, ...) {
  set.seed(seed)
  texts <- c("great news today", "terrible outcome", "lovely weather",
             "awful decision", "wonderful result", "dreadful mess",
             "fine work", "bad call")[seq_len(n)]
  labs <- rep(c("positive", "negative"), length.out = n)
  suppressWarnings(   # tiny fixtures trip the small-test-split warning
    gold_set(data.frame(text = texts, label = labs),
             text = "text", labels = "label",
             split = c(dev = 0.5, test = 0.5), ...))
}

fix_config <- function(model = "fake-model") {
  LLMR::llm_config("groq", model, temperature = 0)
}

# A runner that labels by keyword: "perfect" coder for these fixtures.
# Reports token usage so cost columns get exercised.
fake_runner_perfect <- function(experiments, ...) {
  experiments$response_text <- vapply(experiments$messages, function(m) {
    txt <- m[["user"]]
    if (grepl("great|lovely|wonderful|fine", txt)) "positive" else "negative"
  }, character(1))
  experiments$sent_tokens <- rep(10L, nrow(experiments))
  experiments$rec_tokens <- rep(2L, nrow(experiments))
  experiments$success <- TRUE
  experiments
}

# A runner that always answers the same label (a bad protocol).
fake_runner_constant <- function(label) {
  function(experiments, ...) {
    experiments$response_text <- rep(label, nrow(experiments))
    experiments$success <- TRUE
    experiments
  }
}

# A runner that returns unparseable junk for every kth row.
fake_runner_flaky <- function(every = 3L) {
  function(experiments, ...) {
    experiments$response_text <- vapply(seq_len(nrow(experiments)), function(i) {
      if (i %% every == 0L) "???" else
        if (grepl("great|lovely|wonderful|fine",
                  experiments$messages[[i]][["user"]])) "Positive " else "NEGATIVE"
    }, character(1))
    experiments$success <- TRUE
    experiments
  }
}
