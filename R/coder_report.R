# coder_report.R ----------------------------------------------------------------------
# Reporting: turn the artifacts into the prose and tables a methods section
# needs, including the part nobody volunteers -- how many times the holdout
# split was consulted.

#' Draft the methods text and validation summary
#'
#' Assembles, from the objects themselves, the paragraph and tables that LLM
#' annotation studies should report and rarely do: the instrument (codebook
#' name, version, hash), the protocol (model, parameters, prompt hash), the
#' validation result with its confidence interval, per-category performance,
#' and the gold set's complete holdout-split ledger -- every evaluation that
#' ever touched the sealed split, not just the flattering one.
#'
#' @param validation A [validate_protocol()] result.
#' @param gold The [gold_set()] used (for the ledger and split sizes).
#' @param protocol The locked [protocol()] (for instrument identifiers).
#' @return A character vector of report lines (class `coding_report`), with
#'   a print method. Paste into the appendix and edit; this is a draft whose
#'   numbers are right, not finished prose.
#' @noRd
coding_report <- function(validation, gold, protocol) {
  stopifnot(inherits(validation, "protocol_validation"),
            inherits(gold, "gold_set"),
            inherits(protocol, "coding_protocol"))
  cb <- protocol$codebook
  tab <- table(gold$split)
  led <- gold_ledger(gold)
  lines <- c(
    sprintf("MEASUREMENT. Units (%s) were coded into %d categories (%s) using codebook '%s' v%s (SHA-256 %s).",
            cb$unit, length(cb$categories),
            paste(codebook_labels(cb), collapse = ", "),
            cb$name, cb$version, substr(codebook_hash(cb), 1, 12)),
    sprintf("PROTOCOL. Coding was performed by %s under locked protocol %s (temperature and all inference settings are part of the hash).",
            protocol$label, substr(protocol$hash %||% "<unlocked>", 1, 12)),
    sprintf("GOLD SET. %d human-labeled units (%s).",
            nrow(gold$data),
            paste(sprintf("%s = %d", names(tab), as.integer(tab)), collapse = ", ")),
    sprintf("VALIDATION (split '%s', n = %d). Accuracy %.3f (95%% CI %.3f-%.3f); macro-F1 %.3f; parse failures %d.",
            validation$split, validation$n, validation$accuracy,
            validation$acc_lo, validation$acc_hi, validation$macro_f1,
            validation$parse_failures),
    sprintf("%s-SPLIT LEDGER. The sealed split was evaluated %d time(s)%s.",
            toupper(.gold_holdout(gold)), nrow(led),
            if (nrow(led) > 1)
              " -- all evaluations are listed below, in order" else ""),
    if (nrow(led)) {
      apply(led, 1L, function(r) {
        sprintf("  %s | protocol %s (%s) | n = %s | accuracy %s",
                r[["ts"]], substr(r[["protocol_hash"]], 1, 12),
                r[["protocol_label"]], r[["n"]],
                format(round(as.numeric(r[["accuracy"]]), 3)))
      })
    }
  )
  structure(lines, class = "coding_report")
}

#' @export
print.coding_report <- function(x, ...) {
  cat(paste(unclass(x), collapse = "\n"), "\n")
  invisible(x)
}
