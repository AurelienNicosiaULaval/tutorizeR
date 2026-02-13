# Reports -----------------------------------------------------------------

#' Build a conversion report object
#' @keywords internal
new_tutorize_report <- function(
  input_file,
  output_file,
  format,
  assessment,
  stats,
  render_result,
  warnings = character(),
  lint_report = NULL,
  messages = character()
) {
  lint_summary <- if (inherits(lint_report, "tutorize_lint_report")) {
    lint_report$summary
  } else {
    list(errors = 0L, warnings = 0L, infos = 0L)
  }

  structure(
    list(
      schema_version = "1.0",
      input_file = input_file,
      output_file = output_file,
      format = format,
      assessment = assessment,
      stats = stats,
      sections = stats$sections %||% 0L,
      chunks_total = stats$chunks_total %||% 0L,
      mcq_explicit = stats$mcq_explicit %||% 0L,
      mcq_from_bank = stats$mcq_from_bank %||% 0L,
      estimated_minutes = stats$estimated_minutes %||% NA_real_,
      render_ok = isTRUE(render_result$ok),
      render_message = render_result$message %||% "",
      warnings = as.character(warnings),
      lint_summary = lint_summary,
      messages = messages,
      generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    ),
    class = "tutorize_report"
  )
}

#' Print method for tutorize conversion reports
#'
#' @param x A `tutorize_report` object.
#' @param ... Unused.
#'
#' @return `x`, invisibly.
#' @export
print.tutorize_report <- function(x, ...) {
  cat("tutorizeR report\n")
  cat("- input:           ", x$input_file, "\n", sep = "")
  cat("- output:          ", x$output_file, "\n", sep = "")
  cat("- format:          ", x$format, "\n", sep = "")
  cat("- assessment:      ", x$assessment, "\n", sep = "")
  cat("- sections:        ", x$sections, "\n", sep = "")
  cat("- chunks:          ", x$chunks_total, "\n", sep = "")
  cat("- exercises:       ", x$stats$exercises, "\n", sep = "")
  cat("- solutions:       ", x$stats$solutions, "\n", sep = "")
  cat("- mcq:             ", x$stats$mcq, "\n", sep = "")
  cat("  - explicit:      ", x$mcq_explicit, "\n", sep = "")
  cat("  - from bank:     ", x$mcq_from_bank, "\n", sep = "")
  cat("- est. minutes:    ", x$estimated_minutes, "\n", sep = "")
  cat("- lint errors:     ", x$lint_summary$errors, "\n", sep = "")
  cat("- lint warnings:   ", x$lint_summary$warnings, "\n", sep = "")

  if (!is.null(x$render_ok) && isTRUE(x$format == "learnr")) {
    cat("- render:          ", if (isTRUE(x$render_ok)) "OK" else "FAILED", "\n", sep = "")
    if (!isTRUE(x$render_ok) && nzchar(x$render_message)) {
      cat("  message:         ", x$render_message, "\n", sep = "")
    }
  }

  if (length(x$warnings) > 0L) {
    cat("\nWarnings:\n")
    for (w in x$warnings) {
      cat("- ", w, "\n", sep = "")
    }
  }

  invisible(x)
}

#' Write a conversion report to JSON or YAML
#'
#' @param report A `tutorize_report` object.
#' @param file Output file path.
#' @param format Output serialization format (`"json"` or `"yaml"`).
#' @param pretty Logical; pretty-print JSON output.
#'
#' @return Invisibly returns `file`.
#' @export
write_tutorize_report <- function(report, file, format = c("json", "yaml"), pretty = TRUE) {
  format <- match.arg(format)

  if (!inherits(report, "tutorize_report")) {
    rlang::abort("`report` must be a tutorize_report object.", class = c("tutorizeR_error_validation", "tutorizeR_error"))
  }

  out_dir <- dirname(file)
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }

  payload <- unclass(report)

  if (format == "json") {
    jsonlite::write_json(payload, path = file, auto_unbox = TRUE, pretty = isTRUE(pretty), null = "null")
  } else {
    writeLines(yaml::as.yaml(payload, indent.mapping.sequence = TRUE), con = file, useBytes = TRUE)
  }

  invisible(file)
}

#' Build folder conversion report object
#' @keywords internal
new_tutorize_folder_report <- function(results) {
  structure(
    list(results = results),
    class = "tutorize_folder_report"
  )
}

#' Print method for folder conversion reports
#'
#' @param x A `tutorize_folder_report` object.
#' @param ... Unused.
#'
#' @return `x`, invisibly.
#' @export
print.tutorize_folder_report <- function(x, ...) {
  res <- x$results
  total <- nrow(res)
  succeeded <- sum(res$status == "ok")
  failed <- sum(res$status == "error")

  cat("tutorizeR folder report\n")
  cat("- files:      ", total, "\n", sep = "")
  cat("- succeeded:  ", succeeded, "\n", sep = "")
  cat("- failed:     ", failed, "\n", sep = "")

  if (failed > 0L) {
    cat("\nFailed files:\n")
    failed_rows <- res[res$status == "error", , drop = FALSE]
    for (i in seq_len(nrow(failed_rows))) {
      cat("- ", failed_rows$input[i], " -> ", failed_rows$message[i], "\n", sep = "")
    }
  }

  invisible(x)
}
