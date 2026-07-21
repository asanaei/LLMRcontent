# archive_contracts.R -----------------------------------------------------------
# Offline replay and live spot-checking of archived calls. The replay key is the
# call's full request hash (LLMR::llm_request_hash), derived from the stored raw
# record; an experiments row recomputes the same value from its config and
# messages, so the two sides match by construction.

#' Replay archived responses offline
#'
#' Builds a runner that serves archived replies from an `archive`. Passed
#' as the `.runner` argument to an LLMR-style execution function, it lets the
#' original pipeline recompute from stored responses with no provider calls and
#' no keys: the reviewer reruns the study and gets the paper's numbers back.
#'
#' Matching is by provider, model, canonical message content, and the
#' generation parameters that change the answer (temperature, max tokens), so
#' the same prompt at two temperatures does not collide. Repeated identical
#' requests are served in archived order. Under the original parallel execution
#' that order is completion order, so for sampled (temperature above zero)
#' studies the multiset of draws is preserved while their assignment to
#' replicate indices is not; for temperature-zero studies the draws are
#' identical and the point is moot. Records logged without message content
#' (`include_messages = FALSE`) cannot be replayed and are excluded.
#'
#' The replayer is stateful: each key holds a queue consumed as requests arrive.
#' `LLMR::reset()` returns it to its initial position so a deterministic
#' pipeline can be replayed again. `replay_mode` chooses what a repeated
#' request gets:
#' `"queue"` (default) serves the next archived response in order; `"first"`
#' always serves the first response for a key
#' (idempotent, never exhausts); `"strict_once"` errors if a key is requested
#' more times than it was archived, catching unintended reuse.
#'
#' @param archive An unredacted `archive` (replay needs the content).
#' @param replay_mode One of `"queue"`, `"first"`, `"strict_once"`; see Details.
#' @return A function of class `archive_replayer`, suitable as a `.runner`
#'   argument. It returns the input experiments with `response_text`,
#'   `sent_tokens`, `rec_tokens`, `response_id`, `success`, and
#'   `error_message` columns; unmatched rows carry `NA` text, `success = FALSE`,
#'   and `"not in archive"`. Reset it with `LLMR::reset()`.
#' @examples
#' # One archived call (in practice the log comes from LLMR::llm_log_enable()).
#' log <- tempfile(fileext = ".jsonl")
#' writeLines(paste0('{"ts":"2026-06-01T10:00:01+0000","schema_version":"1.0",',
#'   '"kind":"call","provider":"openai","model":"gpt-4o-mini","status":200,',
#'   '"request":{"messages":[{"role":"user","content":"Capital of France?"}],',
#'   '"temperature":0},"usage":{"sent":5,"rec":1},',
#'   '"response_id":"r-1","text":"Paris"}'), log)
#' a <- archive_build(log)
#'
#' replay <- archive_replay(a)
#' replay   # how many records, over how many distinct requests
#'
#' # The original pipeline's calls, answered from the archive. The config's
#' # generation parameters are part of the key, so set them as the study did:
#' experiments <- tibble::tibble(
#'   config   = list(LLMR::llm_config("openai", "gpt-4o-mini", temperature = 0)),
#'   messages = list(c(user = "Capital of France?")))
#' replay(experiments)$response_text
#'
#' # The queue advances as it serves; LLMR::reset() restores it for a second pass.
#' LLMR::reset(replay)
#' replay(experiments)$response_text
#' @export
archive_replay <- function(archive, replay_mode = c("queue", "first", "strict_once")) {
  stopifnot(inherits(archive, "archive"))
  replay_mode <- match.arg(replay_mode)
  if (isTRUE(archive$redacted)) {
    abort("A redacted archive has no content to replay.")
  }

  # Derive both the record and its request hash from the stored raw line. The
  # seal binds that line through record_hash, while the parsed-record cache and
  # manifest request_hash are conveniences outside the seal.
  entries <- list()
  keys <- character(0)
  for (i in seq_along(archive$records)) {
    rec <- .archive_record_from_raw(archive$records[[i]])
    if (!.archive_replayable(rec)) next
    key <- .archive_request_hash_from_record(rec)
    if (is.na(key)) next
    entries[[length(entries) + 1L]] <- list(idx = i, rec = rec, request_hash = key)
    keys <- c(keys, key)
  }

  # Queue state lives in an environment the replayer closes over, so
  # LLMR::reset() can restore it. .seed holds the initial state to rebuild from.
  state <- new.env(parent = emptyenv())
  state$.seed <- list()
  for (key in unique(keys)) {
    state$.seed[[key]] <- entries[keys == key]
  }
  .archive_replay_reset_env(state)

  runner <- function(experiments, ...) {
    if (!is.data.frame(experiments)) abort("`experiments` must be a data frame.")
    if (!all(c("config", "messages") %in% names(experiments))) {
      abort("`experiments` must carry `config` and `messages` columns.")
    }
    n <- nrow(experiments)
    out <- tibble::as_tibble(experiments)
    out$response_text <- rep(NA_character_, n)
    out$sent_tokens <- rep(NA_real_, n)
    out$rec_tokens <- rep(NA_real_, n)
    out$response_id <- rep(NA_character_, n)
    out$success <- rep(FALSE, n)
    out$error_message <- rep(NA_character_, n)

    misses <- 0L
    for (i in seq_len(n)) {
      cfg <- experiments$config[[i]]
      key <- LLMR::llm_request_hash(config = cfg,
                                    messages = experiments$messages[[i]])
      queue <- state[[key]]
      if (is.null(queue)) {
        misses <- misses + 1L
        out$error_message[i] <- "not in archive"
        next
      }
      pos <- switch(replay_mode,
                    first = 1L,
                    queue = queue$pos,
                    strict_once = queue$pos)
      if (pos > length(queue$records)) {
        if (identical(replay_mode, "strict_once")) {
          abort(sprintf("Replay key requested more times than archived (strict_once); request %d.", i))
        }
        misses <- misses + 1L
        out$error_message[i] <- "not in archive"
        next
      }
      entry <- queue$records[[pos]]
      if (!identical(replay_mode, "first")) {
        queue$pos <- pos + 1L
        state[[key]] <- queue
      }
      rec <- entry$rec
      usage <- rec$usage %||% list()
      out$response_text[i] <- as.character(rec$text %||% NA_character_)
      out$sent_tokens[i] <- as.numeric(usage$sent %||% NA_real_)
      out$rec_tokens[i] <- as.numeric(usage$rec %||% NA_real_)
      out$response_id[i] <- as.character(rec$response_id %||% NA_character_)
      out$success[i] <- TRUE
    }
    if (misses) {
      cli::cli_warn("Archive replay could not match {misses} row{?s}; response_text set to NA.")
    }
    out
  }

  structure(runner, class = c("archive_replayer", "function"),
            n_replayable = length(entries), n_keys = length(unique(keys)),
            replay_mode = replay_mode,
            replay_key = "llm_request_hash",
            state = state)
}

# Internal: (re)initialize the live queues from the seeded initial state.
.archive_replay_reset_env <- function(state) {
  for (key in ls(state, all.names = TRUE)) {
    if (identical(key, ".seed")) next
    rm(list = key, envir = state)
  }
  for (key in names(state$.seed)) {
    state[[key]] <- list(records = state$.seed[[key]], pos = 1L)
  }
  invisible(state)
}

#' Reset an archive replayer to its initial position
#'
#' @param x An `archive_replayer` from [archive_replay()].
#' @param ... Ignored.
#' @return `x`, invisibly, with its queues restored to the start.
#' @exportS3Method LLMR::reset
reset.archive_replayer <- function(x, ...) {
  st <- attr(x, "state")
  if (is.environment(st)) .archive_replay_reset_env(st)
  invisible(x)
}

#' @export
print.archive_replayer <- function(x, ...) {
  cat(sprintf("<archive_replayer | %d replayable record(s) | %d distinct request(s) | mode '%s'>\n",
              attr(x, "n_replayable") %||% 0L, attr(x, "n_keys") %||% 0L,
              attr(x, "replay_mode") %||% "queue"))
  invisible(x)
}

#' Measure drift in archived calls against the live providers
#'
#' Draws a stratified sample of archived calls, re-issues them, and reports
#' exact-match rates and any change in the served `model_version`. Exact
#' reproduction is expected only for temperature-zero calls on pinned
#' open-weight backends; for sampled calls disagreement is sampling, not
#' necessarily drift, and the print method says so.
#'
#' `fraction` and `n` are mutually exclusive. When neither is supplied,
#' `fraction = 0.05` is used. A fraction draws `ceiling(fraction * n)` records
#' from each stratum. A count is allocated across strata by largest remainder
#' in proportion to stratum size, with at least one record per nonempty stratum
#' when the count allows. The draw uses [sample()]; set a seed beforehand for a
#' reproducible sample. Eligibility, request reconstruction, and archived text
#' are read from the stored raw records, not their parsed cache.
#'
#' @param archive An `archive`.
#' @param fraction Fraction of eligible calls to re-issue within each stratum,
#'   in `(0, 1]`. Mutually exclusive with `n`.
#' @param n Total number of eligible calls to re-issue, allocated across
#'   strata. Mutually exclusive with `fraction`.
#' @param strata Manifest columns to stratify by.
#' @param .runner Offline seam: a function taking an experiments data frame
#'   with `config` and `messages` list-columns and returning those rows with at
#'   least a `response_text` column. Default `LLMR::call_llm_par()`.
#' @param ... Passed to the runner.
#' @return An `archive_drift` object: `table` is a per-stratum tibble that
#'   begins with the chosen `strata` columns (`provider`, `model` by default),
#'   followed by `n_eligible`, `n_sampled`, `n_exact`, `exact_rate`,
#'   `n_temperature0`, `n_version_changed`; `details` is a per-record
#'   tibble (`idx`, `provider`, `model`, `temperature`, `exact`,
#'   `archived_version`, `served_version`).
#' @examples
#' log <- tempfile(fileext = ".jsonl")
#' writeLines(paste0('{"ts":"2026-06-01T10:00:01+0000","schema_version":"1.0",',
#'   '"kind":"call","provider":"openai","model":"gpt-4o-mini","status":200,',
#'   '"request":{"messages":[{"role":"user","content":"Capital of France?"}],',
#'   '"temperature":0},"model_version":"gpt-4o-mini-2026-06-01",',
#'   '"usage":{"sent":5,"rec":1},"response_id":"r-1","text":"Paris"}'), log)
#' a <- archive_build(log)
#'
#' # Live use issues real calls; here a runner that echoes the archived text
#' # stands in through the .runner seam so the example needs no key.
#' echo <- function(experiments, ...) {
#'   experiments$response_text <- "Paris"
#'   experiments$model_version <- "gpt-4o-mini-2026-06-01"
#'   experiments
#' }
#' archive_drift(a, fraction = 1, .runner = echo)$table
#' @export
archive_drift <- function(archive, fraction = NULL, n = NULL,
                          strata = c("provider", "model"),
                          .runner = NULL, ...) {
  stopifnot(inherits(archive, "archive"))
  if (!is.null(fraction) && !is.null(n)) {
    abort("Supply only one of `fraction` and `n`.")
  }
  if (is.null(fraction) && is.null(n)) fraction <- 0.05
  if (!is.null(fraction) &&
      (!is.numeric(fraction) || length(fraction) != 1L ||
       !is.finite(fraction) || fraction <= 0 || fraction > 1)) {
    abort("`fraction` must be one number in (0, 1].")
  }
  if (!is.null(n) &&
      (!is.numeric(n) || length(n) != 1L || !is.finite(n) ||
       n < 1 || n != floor(n))) {
    abort("`n` must be a positive whole number.")
  }
  strata <- as.character(strata)
  if (!length(strata)) abort("`strata` must name at least one manifest column.")

  raw_records <- lapply(archive$records, .archive_record_from_raw)
  requests <- lapply(raw_records, function(rec) {
    if (!.archive_replayable(rec)) return(NULL)
    req <- LLMR::llm_request_from_log(rec, on_unsupported = "quiet")
    if (!isTRUE(req$complete) || !inherits(req$config, "llm_config") ||
        !length(req$messages)) return(NULL)
    req
  })
  eligible_idx <- which(!vapply(requests, is.null, logical(1)))
  if (!length(eligible_idx)) {
    abort("No call records with reconstructible requests and text are available to check for drift.")
  }
  eligible <- tibble::as_tibble(archive$manifest[eligible_idx, , drop = FALSE])
  unknown <- setdiff(strata, names(eligible))
  if (length(unknown)) {
    abort(sprintf("`strata` names unknown manifest column(s): %s.",
                  paste(unknown, collapse = ", ")))
  }

  gkey <- .archive_group_key(eligible, strata)
  groups <- unique(gkey)
  gid <- match(gkey, groups)
  sizes <- as.integer(tabulate(gid, nbins = length(groups)))
  alloc <- .archive_sample_allocation(sizes, fraction = fraction, n = n)

  sampled_idx <- integer(0)
  sampled_group <- integer(0)
  for (g in seq_along(groups)) {
    members <- eligible_idx[gid == g]
    k <- alloc[g]
    if (k > 0L) {
      draw <- if (length(members) == 1L) members else base::sample(members, k)
      sampled_idx <- c(sampled_idx, draw)
      sampled_group <- c(sampled_group, rep(g, k))
    }
  }

  recs <- raw_records[sampled_idx]
  reqs <- requests[sampled_idx]
  configs <- lapply(reqs, `[[`, "config")
  messages <- lapply(reqs, `[[`, "messages")
  temperatures <- vapply(configs, function(cfg) {
    v <- cfg$model_params$temperature
    if (is.null(v) || !length(v)) NA_real_ else suppressWarnings(as.numeric(v)[1])
  }, numeric(1))

  experiments <- tibble::tibble(
    archive_idx = as.integer(sampled_idx),
    config = configs,
    messages = messages
  )
  results <- if (is.null(.runner)) LLMR::call_llm_par(experiments, ...)
             else .runner(experiments, ...)
  required <- c(names(experiments), "response_text")
  if (!is.data.frame(results) || !all(required %in% names(results)) ||
      nrow(results) != length(sampled_idx)) {
    abort("`.runner` must return the sampled experiment rows with a `response_text` column.")
  }
  result_idx <- suppressWarnings(as.integer(results$archive_idx))
  if (anyNA(result_idx) || anyDuplicated(result_idx) ||
      !setequal(result_idx, sampled_idx)) {
    abort("`.runner` must preserve sampled experiment row identity.")
  }
  results <- results[match(sampled_idx, result_idx), , drop = FALSE]
  failed <- if ("success" %in% names(results)) {
    which(!(results$success %in% TRUE))
  } else {
    integer(0)
  }
  failed <- union(failed, which(is.na(results$response_text)))
  if (length(failed)) {
    abort(sprintf("The runner failed for %d sampled record(s); drift was not computed.",
                  length(failed)))
  }

  archived_text <- vapply(recs, function(r) as.character(r$text %||% NA_character_),
                          character(1))
  new_text <- as.character(results$response_text)
  exact <- trimws(new_text) == trimws(archived_text)

  archived_version <- vapply(recs, function(r)
    as.character(r$model_version %||% NA_character_), character(1))
  has_version <- "model_version" %in% names(results)
  served_version <- if (has_version) as.character(results$model_version)
                    else rep(NA_character_, length(sampled_idx))
  version_changed <- if (has_version)
    .archive_version_changed(archived_version, served_version)
  else rep(NA, length(sampled_idx))

  details <- tibble::tibble(
    idx = sampled_idx,
    provider = vapply(recs, function(r) as.character(r$provider %||% NA_character_), character(1)),
    model = vapply(recs, function(r) as.character(r$model %||% NA_character_), character(1)),
    temperature = temperatures, exact = exact,
    archived_version = archived_version, served_version = served_version)

  first <- match(groups, gkey)
  tab <- tibble::as_tibble(eligible[first, strata, drop = FALSE])
  tab$n_eligible <- sizes
  tab$n_sampled <- as.integer(tabulate(sampled_group, nbins = length(groups)))
  tab$n_exact <- as.integer(tabulate(sampled_group[exact], nbins = length(groups)))
  tab$exact_rate <- ifelse(tab$n_sampled > 0L, tab$n_exact / tab$n_sampled, NA_real_)
  tab$n_temperature0 <- as.integer(tabulate(
    sampled_group[!is.na(temperatures) & temperatures == 0],
    nbins = length(groups)))
  tab$n_version_changed <- if (has_version)
    as.integer(tabulate(sampled_group[version_changed], nbins = length(groups)))
  else rep(NA_integer_, length(groups))

  structure(list(table = tab, details = details), class = "archive_drift")
}

#' @export
print.archive_drift <- function(x, ...) {
  n <- nrow(x$details)
  cat(sprintf("<archive_drift | %d sampled record(s) | exact %d/%d>\n",
              n, sum(x$details$exact, na.rm = TRUE), n))
  print(x$table)
  cat("Exact reproduction is the expectation only for temperature-0 calls on pinned open-weight backends; for sampled calls disagreement is sampling, not necessarily drift.\n")
  invisible(x)
}

# ---- canonical turns and helpers ------------------------------------------------

.archive_replayable <- function(rec) {
  identical(rec$kind, "call") && !is.null(rec$request) && !is.null(rec$text)
}

.archive_record_from_raw <- function(record) {
  jsonlite::fromJSON(record$raw, simplifyVector = FALSE)
}

.archive_request_hash_from_record <- function(rec) {
  if (is.null(rec[["request"]])) return(NA_character_)
  req <- LLMR::llm_request_from_log(rec, on_unsupported = "quiet")
  if (!isTRUE(req$complete) || !length(req$messages)) return(NA_character_)
  LLMR::llm_request_hash(config = req$config, messages = req$messages)
}

.archive_group_key <- function(x, cols) {
  vapply(seq_len(nrow(x)), function(i) {
    paste(vapply(cols, function(col) {
      val <- x[[col]][i]
      if (length(val) == 0L || is.na(val)) "<NA>" else as.character(val)
    }, character(1)), collapse = "\r")
  }, character(1))
}

# Largest-remainder allocation of a sample across strata.
.archive_sample_allocation <- function(sizes, fraction = NULL, n = NULL) {
  if (!is.null(fraction)) return(as.integer(ceiling(fraction * sizes)))
  total <- min(n, sum(sizes))
  if (total <= 0L) return(rep(0L, length(sizes)))
  quota <- total * sizes / sum(sizes)
  alloc <- floor(quota)
  rem <- quota - alloc
  while (sum(alloc) < total) {
    candidates <- which(alloc < sizes)
    pick <- candidates[order(-rem[candidates], -sizes[candidates], candidates)][1]
    alloc[pick] <- alloc[pick] + 1L
  }
  if (total >= length(sizes)) {
    for (z in which(sizes > 0L & alloc == 0L)) {
      donors <- which(alloc > 1L)
      if (!length(donors)) break
      donor <- donors[order(rem[donors], -alloc[donors], donors)][1]
      alloc[donor] <- alloc[donor] - 1L
      alloc[z] <- 1L
    }
  }
  as.integer(pmin(alloc, sizes))
}

.archive_version_changed <- function(archived, served) {
  archived <- as.character(archived)
  served <- as.character(served)
  !((is.na(archived) & is.na(served)) |
      (!is.na(archived) & !is.na(served) & archived == served))
}
