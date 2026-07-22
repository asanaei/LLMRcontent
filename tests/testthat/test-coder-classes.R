test_that("codebooks validate, print, render, and hash stably", {
  category <- cb_category("positive", "Approving or hopeful.")
  expect_s3_class(category, "cb_category")
  expect_output(print(category), "<cb_category 'positive'>", fixed = TRUE)

  cb <- fix_codebook()
  expect_s3_class(cb, "codebook")
  expect_identical(codebook_labels(cb), c("positive", "negative"))
  expect_output(print(cb), "codebook 'tone'")

  txt <- format_codebook(cb)
  expect_match(txt, "LABEL: positive")
  expect_match(txt, "Assign exactly one")

  h1 <- codebook_hash(cb)
  h2 <- codebook_hash(fix_codebook())
  expect_identical(h1, h2)                       # same content, same hash
  cb2 <- fix_codebook(); cb2$version <- "1.1"
  expect_false(identical(codebook_hash(cb2), h1)) # any edit changes it

  expect_error(codebook("x", "u", list(cb_category("a", "d"))), "categories")
  expect_error(codebook("x", "u", list(cb_category("a", "d"),
                                       cb_category("a", "e"))), "unique")
})

test_that("gold sets split, seal, and expose a visible ledger", {
  g <- fix_gold(8)
  expect_s3_class(g, "gold_set")
  expect_identical(g$label, "label")
  expect_identical(
    names(formals(gold_set)),
    c("data", "text", "label", "split", "holdout", "stratify",
      "seal_holdout", "coders", "id")
  )
  expect_setequal(unique(g$split), c("dev", "test"))
  expect_equal(nrow(gold_split(g, "dev")) + nrow(gold_split(g, "test")), 8L)
  expect_error(gold_split(g, "nope"), "No split")
  expect_equal(nrow(gold_ledger(g)), 0L)
  expect_output(print(g), "SEALED")

  LLMRcontent:::.gold_ledger_append(g, "test", "abc", "p1", 4L, 0.75)
  expect_equal(nrow(gold_ledger(g)), 1L)         # append survives, no reassign
  expect_identical(gold_ledger(g)$protocol_label, "p1")
})

test_that("split allocation is exact, stratified, and guarded", {
  # largest remainder: no NA assignments at awkward sizes (round() would fail)
  alloc <- LLMRcontent:::.alloc_split(5, c(dev = 0.5, test = 0.5))
  expect_length(alloc, 5L)
  expect_false(anyNA(alloc))
  expect_setequal(unique(alloc), c("dev", "test"))

  # stratification: each label class splits in proportion
  set.seed(110)
  big <- data.frame(text = paste("unit", 1:100),
                    label = rep(c("a", "b"), each = 50))
  g <- suppressWarnings(gold_set(big, "text", "label",
                                 split = c(dev = 0.6, test = 0.4)))
  dev_tab <- table(gold_split(g, "dev")$label)
  expect_equal(as.integer(dev_tab[["a"]]), 30L)
  expect_equal(as.integer(dev_tab[["b"]]), 30L)

  # NA gold labels are rejected outright
  bad <- data.frame(text = c("a", "b"), label = c("x", NA))
  expect_error(gold_set(bad, "text", "label"), "not gold")

  # a tiny test split warns toward gold_size()
  expect_warning(
    gold_set(data.frame(text = letters[1:6], label = rep(c("x", "y"), 3)),
             "text", "label"),
    "gold_size")
})

test_that("protocols lock with a content hash over everything that matters", {
  p <- protocol(fix_codebook(), fix_config(), label = "p1")
  expect_false(p$locked)
  expect_output(print(p), "unlocked")

  pl <- protocol_lock(p)
  expect_true(pl$locked)
  expect_match(pl$hash, "^[a-f0-9]{64}$")
  expect_output(print(pl), "LOCKED")

  # Identical inputs hash identically; a model or prompt change alters the hash.
  pl2 <- protocol_lock(protocol(fix_codebook(), fix_config(), label = "p1"))
  expect_identical(pl$hash, pl2$hash)
  pl3 <- protocol_lock(protocol(fix_codebook(), fix_config("other-model")))
  expect_false(identical(pl3$hash, pl$hash))
  pl4 <- protocol_lock(protocol(fix_codebook(), fix_config(),
                                prompt = "Different. {text}"))
  expect_false(identical(pl4$hash, pl$hash))
  # the parser is part of the instrument: swapping it changes the hash
  pl5 <- protocol_lock(protocol(fix_codebook(), fix_config(),
                                parser = function(text, labels) labels[1]))
  expect_false(identical(pl5$hash, pl$hash))

  cfg_embedding <- fix_config()
  cfg_embedding$embedding <- TRUE
  pl6 <- protocol_lock(protocol(fix_codebook(), cfg_embedding))
  expect_false(identical(pl6$hash, pl$hash))
})

test_that("protocol uses an internal default parser and accepts ordinary functions", {
  expect_false("parse_label" %in% getNamespaceExports("LLMRcontent"))
  expect_null(formals(protocol)$parser)

  p <- protocol(fix_codebook(), fix_config())
  expect_identical(p$parser(" Positive ", c("positive", "negative")),
                   "positive")
  expect_true(is.na(p$parser("other", c("positive", "negative"))))

  custom <- function(text, labels) labels[[2]]
  expect_identical(protocol(fix_codebook(), fix_config(), parser = custom)$parser,
                   custom)
})

test_that("protocol requires a positive whole-number replicate count", {
  expect_error(protocol(fix_codebook(), fix_config(), replicates = 1.9),
               "finite, positive whole-number scalar")
  expect_error(protocol(fix_codebook(), fix_config(), replicates = -1),
               "finite, positive whole-number scalar")
  expect_error(protocol(fix_codebook(), fix_config(), replicates = NA),
               "finite, positive whole-number scalar")
})

test_that("the ecosystem hash convention is pinned (drift guard vs LLMR)", {
  expect_identical(
    LLMR::llm_hash(list(model = "gpt-oss-20b", temperature = 0)),
    "7c5ffbb0b308f20bf188a3efd962a2895f45ad202307234ee1965d86abc0606c")
})

test_that("prompt rendering is literal: braces in the text cannot break it", {
  p <- protocol(fix_codebook(), fix_config())
  out <- LLMRcontent:::.render_prompt(p, "code {this} and {that}")
  expect_match(out, "code \\{this\\} and \\{that\\}")
  expect_match(out, "LABEL: positive")
  expect_error(protocol(fix_codebook(), fix_config(), prompt = "no placeholder"),
               "\\{text\\}")
})

test_that("gold_size returns a sensible plan", {
  set.seed(110)
  out <- gold_size(expected_agreement = 0.85, ci_width = 0.10)
  expect_s3_class(out, "gold_size")
  expect_named(out, c("recommended_size", "candidates"))
  expect_true(out$recommended_size %in% c(50L, 100L, 200L, 300L, 500L, 800L))
  expect_named(out$candidates, c("n", "mean_ci_width", "meets_target"))
  expect_type(out$candidates$n, "integer")
  expect_type(out$candidates$mean_ci_width, "double")
  expect_type(out$candidates$meets_target, "logical")
  expect_true(all(diff(out$candidates$mean_ci_width) < 0))
  expect_output(print(out), "recommended n")

  set.seed(110)
  unsorted <- gold_size(ci_width = 0.30,
                        n_grid = c(300, 50, 200, 100), sims = 50)
  expect_identical(unsorted$recommended_size, 50L)

  set.seed(110)
  expect_warning(
    no_match <- gold_size(ci_width = 1e-8,
                          n_grid = c(100, 50, 200), sims = 10),
    "largest n"
  )
  expect_identical(no_match$recommended_size, 200L)
  expect_error(gold_size(n_grid = integer()), "n_grid")
})

test_that("a seal without a split named 'test' warns instead of passing silently", {
  expect_warning(
    gold_set(data.frame(text = letters[1:4], label = rep(c("x", "y"), 2)),
             text = "text", label = "label",
             split = c(dev = 0.5, holdout = 0.5), seal_holdout = TRUE),
    "no split is named 'test'"
  )
})
