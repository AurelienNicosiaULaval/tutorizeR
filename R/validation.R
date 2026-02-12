# Validation --------------------------------------------------------------

#' Validate tutorizeR input arguments
#'
#' Performs defensive checks for file path, format, assessment, and extension.
#'
#' @param input Path to source `.Rmd` or `.qmd` document.
#' @param format Target output format (`"learnr"` or `"quarto-live"`).
#' @param assessment Assessment mode (`"code"`, `"mcq"`, or `"both"`).
#'
#' @return Invisibly returns `TRUE` if validation succeeds.
#' @export
validate_input <- function(input, format = c("learnr", "quarto-live"), assessment = c("code", "mcq", "both")) {
  format <- match.arg(format)
  assessment <- match.arg(assessment)

  if (!is_scalar_character(input) || !nzchar(trimws(input))) {
    rlang::abort(
      "`input` must be a non-empty file path.",
      class = c("tutorizeR_error_validation", "tutorizeR_error")
    )
  }

  if (!file.exists(input)) {
    rlang::abort(
      sprintf("Input file not found: %s", input),
      class = c("tutorizeR_error_validation", "tutorizeR_error")
    )
  }

  if (dir.exists(input)) {
    rlang::abort(
      sprintf("`input` must point to a file, not a directory: %s", input),
      class = c("tutorizeR_error_validation", "tutorizeR_error")
    )
  }

  ext <- tolower(tools::file_ext(input))
  if (!(ext %in% c("rmd", "qmd"))) {
    rlang::abort(
      sprintf("Unsupported input extension '.%s'. Expected .Rmd or .qmd.", ext),
      class = c("tutorizeR_error_validation", "tutorizeR_error")
    )
  }

  invisible(TRUE)
}

#' Validate output path and overwrite strategy
#'
#' @param output_file Destination output file path.
#' @param input_file Source file path.
#' @param overwrite Logical; allow replacing an existing output.
#'
#' @return Invisibly returns `TRUE` if validation succeeds.
#' @export
validate_output <- function(output_file, input_file, overwrite = FALSE) {
  if (!is_scalar_character(output_file) || !nzchar(trimws(output_file))) {
    rlang::abort(
      "`output_file` must be a non-empty file path.",
      class = c("tutorizeR_error_validation", "tutorizeR_error")
    )
  }

  normalized_input <- normalizePath(input_file, winslash = "/", mustWork = TRUE)
  normalized_output <- tryCatch(
    normalizePath(output_file, winslash = "/", mustWork = FALSE),
    error = function(e) output_file
  )

  if (identical(normalized_input, normalized_output)) {
    rlang::abort(
      "`output_file` cannot be the same as `input_file`.",
      class = c("tutorizeR_error_validation", "tutorizeR_error")
    )
  }

  out_dir <- dirname(output_file)
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }

  if (file.exists(output_file) && !isTRUE(overwrite)) {
    rlang::abort(
      sprintf("Output already exists. Set `overwrite = TRUE` to replace: %s", output_file),
      class = c("tutorizeR_error_validation", "tutorizeR_error")
    )
  }

  invisible(TRUE)
}

#' Resolve output file path from user input
#' @keywords internal
resolve_output_file <- function(input, format, output_dir = NULL, output_file = NULL) {
  if (!is.null(output_file)) {
    return(output_file)
  }

  base <- tools::file_path_sans_ext(basename(input))
  suffix <- if (format == "learnr") "-tutorial.Rmd" else "-live.qmd"
  out_dir <- output_dir %||% dirname(input)

  file.path(out_dir, paste0(base, suffix))
}
