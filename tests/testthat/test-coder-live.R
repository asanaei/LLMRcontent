# One live smoke test on an inexpensive open-weight model; gated on the key
# and never run on CRAN.

test_that("live: a one-protocol tournament runs end to end on groq", {
  testthat::skip_if(!nzchar(Sys.getenv("GROQ_API_KEY")), "Requires GROQ_API_KEY")
  testthat::skip_on_cran()

  cb <- codebook(
    "tone", "one sentence",
    list(cb_category("positive", "Approving, pleased, or hopeful."),
         cb_category("negative", "Critical, displeased, or alarmed.")))
  g <- gold_set(
    data.frame(text = c("What a wonderful day!", "This is a disaster.",
                        "I love this result.", "Everything went wrong."),
               label = c("positive", "negative", "positive", "negative")),
    text = "text", labels = "label", split = c(dev = 0.5, test = 0.5))
  cfg <- LLMR::llm_config("groq", "openai/gpt-oss-20b", temperature = 0)
  p <- protocol(cb, cfg, label = "gpt-oss-20b")

  res <- tune_protocol(p, g, split = "dev", progress = FALSE)
  expect_s3_class(res, "protocol_tuning")
  expect_gte(res$accuracy[1], 0.5)

  v <- validate_protocol(protocol_lock(p), g, progress = FALSE)
  expect_equal(nrow(gold_ledger(g)), 1L)
})
