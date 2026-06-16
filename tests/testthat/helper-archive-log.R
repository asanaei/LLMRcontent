# A synthetic LLMR-shaped audit log: same fields the real logger writes
# (schema_version, kind, provider, model, request, text, usage, ...).

fake_log <- function(path = tempfile(fileext = ".jsonl"), n = 4) {
  mk <- function(i, provider, model, version) {
    jsonlite::toJSON(list(
      ts = sprintf("2026-06-10T12:00:%02d+0000", i),
      schema_version = "1.0",
      llmr_version = "0.8.3",
      kind = if (i == n) "error" else "call",
      provider = provider, model = model, status = if (i == n) 500L else 200L,
      request = list(model = model,
                     messages = list(list(role = "user",
                                          content = paste("question", i))),
                     temperature = 0),
      model_version = version,
      finish_reason = "stop",
      usage = list(sent = 10L + i, rec = 5L, total = 15L + i),
      response_id = paste0("resp-", i),
      duration_s = 0.3,
      text = paste("answer", i)
    ), auto_unbox = TRUE, null = "null")
  }
  lines <- vapply(seq_len(n), function(i) {
    if (i %% 2 == 0) mk(i, "openai", "gpt-4o", "gpt-4o-2026-01")
    else mk(i, "groq", "openai/gpt-oss-20b", "gpt-oss-20b")
  }, character(1))
  writeLines(lines, path, useBytes = TRUE)
  path
}
