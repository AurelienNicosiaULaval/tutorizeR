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
#' @param question_bank Optional path(s) or `tutorize_question_bank` object.
#' @param mcq_source MCQ generation source (`"inline"`, `"bank"`, `"mixed"`).
#' @param lint_strict Logical; fail conversion when lint reports errors.
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
  verbose = TRUE,
  question_bank = NULL,
  mcq_source = c("inline", "bank", "mixed"),
  lint_strict = FALSE
) {
  format <- match.arg(format)
  assessment <- match.arg(assessment)
  language <- resolve_language(match.arg(language))
  mcq_source <- match.arg(mcq_source)

  if (!dir.exists(dir)) {
    rlang::abort(
      tr("errors.dir_not_found", path = dir, language = language),
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
      cli::cli_alert_info(tr("messages.no_files_found", dir = dir, language = language))
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
    cli::cli_h1(tr("messages.converting_files", n = length(files), language = language))
  }

  qb <- NULL
  if (!is.null(question_bank)) {
    if (inherits(question_bank, "tutorize_question_bank")) {
      qb <- question_bank
    } else {
      qb <- load_question_bank(question_bank, strict = TRUE, language = language)
    }
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
        verbose = verbose,
        question_bank = qb,
        mcq_source = mcq_source,
        lint_strict = lint_strict
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
    cli::cli_alert_success(tr("messages.conversion_complete", ok = ok_n, err = err_n, language = language))
  }

  invisible(new_tutorize_folder_report(results))
}
