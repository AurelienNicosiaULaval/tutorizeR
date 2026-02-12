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
  messages = character()
) {
  structure(
    list(
      input_file = input_file,
      output_file = output_file,
      format = format,
      assessment = assessment,
      stats = stats,
      render_ok = isTRUE(render_result$ok),
      render_message = render_result$message %||% "",
      messages = messages,
      generated_at = Sys.time()
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
  cat("- input:      ", x$input_file, "\n", sep = "")
  cat("- output:     ", x$output_file, "\n", sep = "")
  cat("- format:     ", x$format, "\n", sep = "")
  cat("- assessment: ", x$assessment, "\n", sep = "")
  cat("- exercises:  ", x$stats$exercises, "\n", sep = "")
  cat("- solutions:  ", x$stats$solutions, "\n", sep = "")
  cat("- mcq:        ", x$stats$mcq, "\n", sep = "")

  if (!is.null(x$render_ok) && isTRUE(x$format == "learnr")) {
    cat("- render:     ", if (isTRUE(x$render_ok)) "OK" else "FAILED", "\n", sep = "")
    if (!isTRUE(x$render_ok) && nzchar(x$render_message)) {
      cat("  message:    ", x$render_message, "\n", sep = "")
    }
  }

  invisible(x)
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
