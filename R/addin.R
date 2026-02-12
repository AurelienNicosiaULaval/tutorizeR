#' RStudio Addin: Convert active document to tutorial
#'
#' Opens a lightweight prompt flow (format, assessment, output directory,
#' overwrite) and converts the currently active file.
#'
#' @return Invisibly returns the `tutorize_report` object, or `NULL` when
#' cancelled.
#' @export
launch_tutorizeR_addin <- function() {
  if (!rstudioapi::isAvailable()) {
    rlang::abort(
      "RStudio addins are only available inside RStudio.",
      class = c("tutorizeR_error_validation", "tutorizeR_error")
    )
  }

  ctx <- rstudioapi::getSourceEditorContext()
  path <- ctx$path

  if (!nzchar(path)) {
    rlang::abort(
      "Please save the active document before converting.",
      class = c("tutorizeR_error_validation", "tutorizeR_error")
    )
  }

  format_in <- rstudioapi::showPrompt(
    title = "tutorizeR",
    message = "Output format (learnr|quarto-live)",
    default = "learnr"
  )
  if (is.null(format_in)) {
    return(invisible(NULL))
  }

  assessment_in <- rstudioapi::showPrompt(
    title = "tutorizeR",
    message = "Assessment (code|mcq|both)",
    default = "both"
  )
  if (is.null(assessment_in)) {
    return(invisible(NULL))
  }

  overwrite <- isTRUE(rstudioapi::showQuestion(
    title = "tutorizeR",
    message = "Overwrite existing output if present?",
    ok = "Yes",
    cancel = "No"
  ))

  out_dir <- rstudioapi::selectDirectory(caption = "Select output directory (Cancel = source directory)")
  if (is.null(out_dir) || !nzchar(out_dir)) {
    out_dir <- NULL
  }

  report <- tutorize(
    input = path,
    output_dir = out_dir,
    format = tolower(trimws(format_in)),
    assessment = tolower(trimws(assessment_in)),
    overwrite = overwrite,
    verbose = TRUE
  )

  invisible(report)
}

#' RStudio Addin: Convert a folder to tutorials
#'
#' Prompts for folder and conversion options, then converts all matching files.
#'
#' @return Invisibly returns a `tutorize_folder_report` object, or `NULL` when
#' cancelled.
#' @export
launch_tutorizeR_folder_addin <- function() {
  if (!rstudioapi::isAvailable()) {
    rlang::abort(
      "RStudio addins are only available inside RStudio.",
      class = c("tutorizeR_error_validation", "tutorizeR_error")
    )
  }

  folder <- rstudioapi::selectDirectory(caption = "Select folder to convert")
  if (is.null(folder) || !nzchar(folder)) {
    return(invisible(NULL))
  }

  format_in <- rstudioapi::showPrompt(
    title = "tutorizeR",
    message = "Output format (learnr|quarto-live)",
    default = "learnr"
  )
  if (is.null(format_in)) {
    return(invisible(NULL))
  }

  assessment_in <- rstudioapi::showPrompt(
    title = "tutorizeR",
    message = "Assessment (code|mcq|both)",
    default = "both"
  )
  if (is.null(assessment_in)) {
    return(invisible(NULL))
  }

  recursive <- isTRUE(rstudioapi::showQuestion(
    title = "tutorizeR",
    message = "Scan subdirectories recursively?",
    ok = "Yes",
    cancel = "No"
  ))

  report <- convert_folder(
    dir = folder,
    recursive = recursive,
    format = tolower(trimws(format_in)),
    assessment = tolower(trimws(assessment_in)),
    verbose = TRUE
  )

  invisible(report)
}
