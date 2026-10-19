# Studio CSV reads keep identifiers as written and restore the other column
# types after mapping.

write_id_csv <- function(lines) {
  path <- tempfile(fileext = ".csv")
  writeLines(lines, path)
  path
}

test_that("Studio reads keep identifiers and restore the other types", {
  skip_if_not_installed("LLMR.shiny", "0.1.3")
  path <- write_id_csv(c(
    "doc_id,key,text,score,flag,note",
    "007,7,NA,1.5,TRUE,NA",
    "07,12345678901234567890,second text,,FALSE,x",
    "7,0012,007,3,NA,y",
    "12345678901234567890,  ,1.10,4,TRUE,"))
  on.exit(unlink(path), add = TRUE)
  raw <- LLMRcontent:::read_csv_path(path)
  expect_true(all(vapply(raw, is.character, logical(1))))
  expect_identical(raw$doc_id, c("007", "07", "7", "12345678901234567890"))

  typed <- LLMRcontent:::.content_type_columns(raw, ids = "key", text = "text")
  expect_identical(typed$doc_id, c("007", "07", "7", "12345678901234567890"))
  expect_identical(typed$key, c("7", "12345678901234567890", "0012", NA))
  expect_identical(typed$text, c(NA, "second text", "007", "1.10"))

  inferred <- utils::read.csv(path, stringsAsFactors = FALSE)
  for (column in c("score", "flag", "note")) {
    expect_identical(typed[[column]], inferred[[column]])
  }
  expect_type(typed$score, "double")
  expect_type(typed$flag, "logical")
  # The default inference is what the Studio used to apply to identifiers.
  expect_identical(inferred$doc_id[1:3], c(7, 7, 7))
  expect_false(identical(sprintf("%.0f", inferred$doc_id[4]),
                         "12345678901234567890"))
})

test_that("mapped gold and corpus keep distinct identifiers and link by them", {
  skip_if_not_installed("LLMR.shiny", "0.1.3")
  ids <- c("007", "07", "7", "0007", "12345678901234567890",
           "12345678901234567891", "010", "10")
  texts <- c("great news today", "terrible outcome", "lovely weather",
             "awful decision", "wonderful result", "dreadful mess",
             "fine work", "bad call")
  labels <- rep(c("positive", "negative"), 4)
  gold_path <- write_id_csv(c("case,text,label",
                              sprintf("%s,%s,%s", ids, texts, labels)))
  corpus_path <- write_id_csv(c("case,text,year",
                                sprintf("%s,%s,%d", ids, texts, 2001:2008)))
  on.exit(unlink(c(gold_path, corpus_path)), add = TRUE)

  set.seed(110)
  gold <- suppressWarnings(LLMRcontent:::call_gold_set_mapped(
    LLMRcontent:::read_csv_path(gold_path), "text", "label",
    split = c(dev = 0.5, test = 0.5), stratify = TRUE, id_col = "case"))
  expect_identical(gold$data$case, ids)

  p <- protocol_lock(protocol(fix_codebook(), fix_config()))
  coded <- LLMRcontent:::call_code_corpus_mapped(
    LLMRcontent:::read_csv_path(corpus_path), "text", p,
    .runner = fake_runner_perfect, id_col = "case")
  expect_identical(coded$data$case, ids)
  expect_type(coded$data$year, "integer")

  correction <- suppressWarnings(gold_correct(coded, gold, design = "srs"))
  expect_s3_class(correction, "gold_correction")

  # The inferring reader turns the eight identifiers into four numbers, and
  # the gold set refuses them as duplicates.
  inferred <- utils::read.csv(gold_path, stringsAsFactors = FALSE)
  expect_error(suppressWarnings(LLMRcontent:::call_gold_set_mapped(
    inferred, "text", "label", split = c(dev = 0.5, test = 0.5),
    stratify = TRUE, id_col = "case")), "duplicate")
})

test_that("a missing gold label written as NA still disables gold creation", {
  skip_if_not_installed("LLMR.shiny", "0.1.3")
  path <- write_id_csv(c("id,text,label", "001,a,x", "002,b,NA", "003,c,y"))
  on.exit(unlink(path), add = TRUE)
  raw <- LLMRcontent:::read_csv_path(path)
  typed <- LLMRcontent:::.content_type_columns(raw["label"], text = "label")
  expect_identical(typed$label, c("x", NA, "y"))
  expect_error(suppressWarnings(LLMRcontent:::call_gold_set_mapped(
    raw, "text", "label", split = c(dev = 0.5, test = 0.5), stratify = FALSE,
    id_col = "id")), "Gold labels contain NA")
})

test_that("archive results keep response identifiers as written", {
  skip_if_not_installed("LLMR.shiny", "0.1.3")
  path <- write_id_csv(c("response_id,response_text,tokens",
                         "000123,positive,12",
                         "98765432109876543210,1,NA"))
  on.exit(unlink(path), add = TRUE)
  typed <- LLMRcontent:::.content_type_columns(LLMRcontent:::read_csv_path(path))
  expect_identical(typed$response_id, c("000123", "98765432109876543210"))
  expect_identical(typed$response_text, c("positive", "1"))
  expect_identical(typed$tokens, c(12L, NA))
})

test_that("the audit tab passes uploaded identifiers to the plan unchanged", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")
  skip_if_not_installed("LLMR.shiny", "0.1.3")
  path <- write_id_csv(c("speech_id,text,year",
                         "0001,cut taxes now,1999",
                         "12345678901234567890,fund the schools,2004"))
  on.exit(unlink(path), add = TRUE)
  shared <- list(
    mode = shiny::reactive("demo"), provider = shiny::reactive("groq"),
    model = shiny::reactive(""), temperature = shiny::reactive(0),
    max_tokens = shiny::reactive(128L), reasoning_effort = shiny::reactive("low"),
    can_run = shiny::reactive(TRUE), key = shiny::reactive(list()),
    set_plan = function(...) NULL, add_usage = function(...) NULL)
  shiny::testServer(
    LLMRcontent:::mod_valid_server,
    args = list(shared = shared, active = shiny::reactive("valid")),
    {
      session$setInputs(data_file = list(datapath = path, name = "speeches.csv"))
      session$setInputs(text_col = "text", labels = "conservative, progressive",
                        estimand = "share", target = "conservative",
                        prompt = "Classify as {labels}.\n\n{text}",
                        orders = "as_given", temps = "0",
                        add_paraphrase = FALSE)
      run_audit(confirmed = TRUE)
      data <- audit()$plan$data
      expect_identical(data$speech_id, c("0001", "12345678901234567890"))
      expect_identical(data$year, c(1999L, 2004L))
    }
  )
})
