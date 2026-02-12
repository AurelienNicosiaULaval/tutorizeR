#' Convert all `.Rmd`/`.qmd` files in a folder
#'
#' Converts every matching source file in `dir`, with resilient per-file
#' error handling and a summary report.
#'
#' @param dir Folder containing source files.
#' @param pattern Regex pattern used to select files.
#' @param recursive Logical; recurse into subdirectories?
#' @param output_dir Optional output directory.
#' @param format Output format (`"learnr"` or `"quarto-live"`).
#' @param assessment Assessment mode (`"code"`, `"mcq"`, `"both"`).
#' @param overwrite Logical; overwrite existing outputs?
#' @param language Message/template language (`"en"` or `"fr"`).
#' @param seed Optional integer seed injected in setup chunk.
#' @param verbose Logical; print progress and summary?
#'
#' @return A `tutorize_folder_report` object (invisibly).
#' @export
#' @examples
#' \dontrun{
#' report <- convert_folder("labs", recursive = TRUE)
#' print(report)
#' }
convert_folder <- function(
  dir = ".",
  pattern = "\\.(Rmd|qmd)$",
  recursive = FALSE,
  output_dir = NULL,
  format = c("learnr", "quarto-live"),
  assessment = c("code", "mcq", "both"),
  overwrite = FALSE,
  language = c("en", "fr"),
  seed = NULL,
  verbose = TRUE
) {
  format <- match.arg(format)
  assessment <- match.arg(assessment)
  language <- match.arg(language)

  if (!dir.exists(dir)) {
    rlang::abort(
      sprintf("Directory not found: %s", dir),
      class = c("tutorizeR_error_validation", "tutorizeR_error")
    )
  }

  files <- list.files(
    path = dir,
    pattern = pattern,
    recursive = recursive,
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(files) == 0L) {
    if (isTRUE(verbose)) {
      cli::cli_alert_info("No matching files found in {dir}")
    }

    empty <- data.frame(
      input = character(),
      output = character(),
      status = character(),
      message = character(),
      stringsAsFactors = FALSE
    )

    return(invisible(new_tutorize_folder_report(empty)))
  }

  if (isTRUE(verbose)) {
    cli::cli_h1("Converting {length(files)} file(s)")
  }

  rows <- lapply(files, function(path) {
    one <- tryCatch(
      tutorize(
        input = path,
        output_dir = output_dir,
        format = format,
        assessment = assessment,
        overwrite = overwrite,
        language = language,
        seed = seed,
        verbose = verbose
      ),
      error = identity
    )

    if (inherits(one, "error")) {
      return(data.frame(
        input = path,
        output = "",
        status = "error",
        message = conditionMessage(one),
        stringsAsFactors = FALSE
      ))
    }

    data.frame(
      input = one$input_file,
      output = one$output_file,
      status = "ok",
      message = "",
      stringsAsFactors = FALSE
    )
  })

  results <- do.call(rbind, rows)

  if (isTRUE(verbose)) {
    ok_n <- sum(results$status == "ok")
    err_n <- sum(results$status == "error")
    cli::cli_alert_success("Conversion complete: {ok_n} succeeded, {err_n} failed.")
  }

  invisible(new_tutorize_folder_report(results))
}
