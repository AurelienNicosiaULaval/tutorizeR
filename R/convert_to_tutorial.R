#' Convert a source document into an interactive tutorial
#'
#' `tutorize()` is the canonical high-level API. It converts an existing
#' `.Rmd` or `.qmd` source document into a tutorial-ready document for
#' `learnr` or `quarto-live`.
#'
#' @param input Path to source `.Rmd` or `.qmd` file.
#' @param output_dir Optional output directory. Defaults to source directory.
#' @param format Output format: `"learnr"` (default) or `"quarto-live"`.
#' @param assessment Assessment mode: `"code"`, `"mcq"`, or `"both"`.
#' @param overwrite Logical; overwrite existing output file?
#' @param language Message/template language: `"en"` (default) or `"fr"`.
#' @param seed Optional integer seed injected into setup chunk.
#' @param verbose Logical; show progress and diagnostic messages?
#' @param output_file Optional explicit output file path. Useful for wrappers.
#'
#' @return A `tutorize_report` object (invisibly).
#' @export
#' @examples
#' \dontrun{
#' rep <- tutorize("analysis.qmd", format = "learnr", assessment = "both")
#' print(rep)
#' }
tutorize <- function(
  input,
  output_dir = NULL,
  format = c("learnr", "quarto-live"),
  assessment = c("code", "mcq", "both"),
  overwrite = FALSE,
  language = c("en", "fr"),
  seed = NULL,
  verbose = TRUE,
  output_file = NULL
) {
  format <- match.arg(format)
  assessment <- match.arg(assessment)
  language <- match.arg(language)

  validate_input(input = input, format = format, assessment = assessment)

  resolved_output <- resolve_output_file(
    input = input,
    format = format,
    output_dir = output_dir,
    output_file = output_file
  )

  validate_output(
    output_file = resolved_output,
    input_file = input,
    overwrite = overwrite
  )

  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1L || is.na(seed)) {
      rlang::abort(
        "`seed` must be NULL or a single non-missing numeric value.",
        class = c("tutorizeR_error_validation", "tutorizeR_error")
      )
    }
  }

  parsed <- tryCatch(
    parse_input_document(input),
    error = function(e) {
      rlang::abort(
        message = paste0("Could not parse input document: ", conditionMessage(e)),
        class = c("tutorizeR_error_parse", "tutorizeR_error")
      )
    }
  )

  converted <- build_tutorial_lines(
    parsed = parsed,
    format = format,
    assessment = assessment,
    language = language,
    seed = seed
  )

  yaml_lines <- build_output_yaml(
    source_yaml = parsed$front_matter$yaml,
    format = format,
    language = language
  )

  preamble <- yaml_lines

  if (format == "quarto-live") {
    include_line <- "{{< include ./_extensions/r-wasm/live/_knitr.qmd >}}"
    if (!any(grepl("_extensions/r-wasm/live/_knitr.qmd", converted$lines, fixed = TRUE))) {
      preamble <- c(preamble, "", include_line)
    }

    extension_file <- file.path(dirname(input), "_extensions", "r-wasm", "live", "_knitr.qmd")
    if (!file.exists(extension_file) && isTRUE(verbose)) {
      cli::cli_alert_warning(
        "Quarto Live extension not found near the input file ({extension_file}). Run `quarto add r-wasm/quarto-live` in the project root."
      )
    }
  }

  final_lines <- trim_trailing_blank_lines(c(preamble, "", converted$lines))

  tryCatch(
    writeLines(final_lines, resolved_output, useBytes = TRUE),
    error = function(e) {
      rlang::abort(
        message = paste0("Failed to write output file: ", conditionMessage(e)),
        class = c("tutorizeR_error_validation", "tutorizeR_error")
      )
    }
  )

  if (isTRUE(verbose)) {
    cli::cli_alert_success("Tutorial written to {resolved_output}")
  }

  render_result <- list(ok = NA, message = "", result = NULL)
  if (format == "learnr") {
    render_result <- check_tutorial_render(resolved_output)
    if (isTRUE(verbose)) {
      if (isTRUE(render_result$ok)) {
        cli::cli_alert_success("Tutorial renders without error.")
      } else {
        cli::cli_alert_danger("Rendering failed: {render_result$message}")
      }
    }
  } else if (isTRUE(verbose)) {
    cli::cli_alert_info("Skipping render check for Quarto Live output.")
  }

  report <- new_tutorize_report(
    input_file = input,
    output_file = resolved_output,
    format = format,
    assessment = assessment,
    stats = converted$stats,
    render_result = render_result
  )

  invisible(report)
}

#' Backward-compatible wrapper for tutorial conversion
#'
#' `convert_to_tutorial()` is kept for compatibility with earlier versions.
#' New code should use [tutorize()].
#'
#' @param input_file Path to source `.Rmd` or `.qmd` file.
#' @param output_file Optional output path. If `NULL`, a default path is used.
#' @param assessment Assessment mode: `"code"`, `"mcq"`, or `"both"`.
#' @param format Output format: `"learnr"` or `"quarto-live"`.
#' @param add_mcq Deprecated. Use `assessment` instead.
#'
#' @return Invisibly, the generated output file path.
#' @export
#' @examples
#' \dontrun{
#' convert_to_tutorial("analysis.Rmd")
#' }
convert_to_tutorial <- function(
  input_file,
  output_file = NULL,
  assessment = c("code", "mcq", "both"),
  format = c("learnr", "quarto-live"),
  add_mcq = NULL
) {
  format <- match.arg(format)

  if (!is.null(add_mcq)) {
    lifecycle::deprecate_warn(
      when = "0.3.0",
      what = "convert_to_tutorial(add_mcq)",
      with = "convert_to_tutorial(assessment)"
    )

    if (isTRUE(add_mcq)) {
      assessment <- "both"
    } else {
      assessment <- "code"
    }
  }

  assessment <- match.arg(assessment)

  report <- tutorize(
    input = input_file,
    output_dir = if (is.null(output_file)) NULL else dirname(output_file),
    output_file = output_file,
    format = format,
    assessment = assessment,
    overwrite = TRUE,
    verbose = TRUE
  )

  invisible(report$output_file)
}

#' Render a generated tutorial and return render status
#'
#' @param file Path to tutorial `.Rmd` to render.
#'
#' @return Invisibly, the render output path or the error object.
#' @export
#' @examples
#' \dontrun{
#' check_tutorial("analysis-tutorial.Rmd")
#' }
check_tutorial <- function(file) {
  res <- check_tutorial_render(file)

  if (isTRUE(res$ok)) {
    cli::cli_alert_success("Tutorial renders without error.")
  } else {
    cli::cli_alert_danger("Rendering failed: {res$message}")
  }

  invisible(res$result)
}

#' Render check implementation for generated learnr tutorials
#' @keywords internal
check_tutorial_render <- function(file) {
  if (!file.exists(file)) {
    rlang::abort(
      sprintf("File not found: %s", file),
      class = c("tutorizeR_error_validation", "tutorizeR_error")
    )
  }

  tmpdir <- tempfile("tutorizer-render-")
  dir.create(tmpdir, recursive = TRUE, showWarnings = FALSE)

  result <- tryCatch(
    rmarkdown::render(
      input = file,
      output_dir = tmpdir,
      quiet = TRUE,
      envir = new.env(parent = globalenv())
    ),
    error = identity
  )

  if (inherits(result, "error")) {
    class(result) <- c("tutorizeR_error_render", class(result))
    return(list(ok = FALSE, message = conditionMessage(result), result = result))
  }

  list(ok = TRUE, message = "", result = result)
}

# Backward-compatible internal helpers ------------------------------------

#' Remove leading YAML front matter lines
#' @keywords internal
strip_yaml <- function(lines) {
  split_front_matter(lines)$body_lines
}

#' Transform source lines into converted tutorial body
#' @keywords internal
parse_chunks <- function(lines, assessment = "code", format = "learnr") {
  parsed <- list(
    front_matter = list(yaml = list()),
    blocks = parse_markdown_blocks(lines)
  )

  converted <- build_tutorial_lines(
    parsed = parsed,
    format = format,
    assessment = assessment,
    language = "en",
    seed = NULL
  )

  converted$lines
}
