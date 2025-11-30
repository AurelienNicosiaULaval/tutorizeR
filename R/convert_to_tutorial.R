#' Convert an .Rmd *or* .qmd into a learnr tutorial (.Rmd)
#'
#' Workflow :
#' \enumerate{
#'   \item Strips any existing YAML front-matter;
#'   \item Converts each original code chunk into a learnr \emph{exercise}
#'         and a \emph{solution} chunk;
#'   \item Optionally appends a skeleton MCQ after each pair;
#'   \item Renders the produced file once to ensure it compiles.
#' }
#'
#' @param input_file  Path to the source \code{.Rmd} or \code{.qmd}.
#' @param output_file Destination \code{.Rmd}; default
#'        \code{<input>-tutorial.Rmd}.
#' @param add_mcq     Logical, should an empty MCQ block be added
#'        after each exercise/solution pair?
#' @param assessment  Character, type of assessment to generate:
#'        \code{"code"} (default), \code{"mcq"}, or \code{"both"}.
#' @param format      Character, output format. Either \code{"learnr"} (default)
#'        or \code{"quarto-live"}.
#'
#' @return Invisibly, the path to the generated tutorial file.
#' @examples
#' \dontrun{
#' convert_to_tutorial("analysis.qmd")
#' }
#' @export
#' @importFrom rmarkdown render
#' @importFrom cli cli_alert_success cli_alert_danger

convert_to_tutorial <- function(
  input_file,
  output_file = NULL,
  assessment = c("code", "mcq", "both"),
  format = c("learnr", "quarto-live"),
  add_mcq = NULL # Deprecated
) {
  stopifnot(file.exists(input_file))
  format <- match.arg(format)

  # Handle deprecation
  if (!is.null(add_mcq)) {
    warning(
      "The 'add_mcq' argument is deprecated. Please use 'assessment' instead."
    )
    if (isTRUE(add_mcq)) {
      assessment <- "both"
    } else {
      assessment <- "code"
    }
  }
  assessment <- match.arg(assessment)

  # 1 . read + strip YAML
  lines <- strip_yaml(readLines(input_file, warn = FALSE))

  # 2 . transform chunks
  body <- parse_chunks(lines, assessment = assessment, format = format)

  if (format == "learnr") {
    header <- template_header_rmd()
  } else {
    header <- template_header_quarto_live()

    # Check for extension
    if (!file.exists("_extensions/r-wasm/live/_knitr.qmd")) {
      cli::cli_alert_warning(
        "Quarto Live extension not found at '_extensions/r-wasm/live/_knitr.qmd'.\nRun 'quarto add r-wasm/quarto-live' to install it."
      )
    }
  }

  out <- c(header, "", body)

  # 3 . write output
  if (is.null(output_file)) {
    ext <- if (format == "learnr") "-tutorial.Rmd" else "-live.qmd"
    output_file <- sub("\\.(R|r|Q|q)md$", ext, input_file)
  }
  writeLines(out, output_file)
  cli::cli_alert_success("Tutorial written to {output_file}")

  # 4 . automatically render & report
  # Only check learnr tutorials for now, as quarto-live requires extension
  if (format == "learnr") {
    res <- check_tutorial(output_file)
    if (inherits(res, "error")) {
      cli::cli_alert_danger(
        "Rendering failed. Inspect {output_file} for problematic code."
      )
    } else {}
  } else {
    cli::cli_alert_success(
      "Skipping render check for Quarto Live (requires extension)."
    )
  }

  invisible(output_file)
}


# -------------------------------------------------------------------------
# Internal helpers --------------------------------------------------------
# -------------------------------------------------------------------------
#' Transform source chunks into learnr exercises + solutions
#'
#' \strong{Rule}: any chunk whose label contains the word
#' \code{setup} (case-insensitive) is copied verbatim and \emph{not}
#' converted.
#'
#' @param lines      Character vector, output of \code{readLines()}.
#' @param assessment Character, type of assessment ("code", "mcq", "both").
#' @param format     Output format ("learnr" or "quarto-live").
#'
#' @return Character vector representing the transformed body.
#' @keywords internal

parse_chunks <- function(lines, assessment = "code", format = "learnr") {
  in_chunk <- FALSE
  buf <- character()
  out <- character()
  id <- 0
  chunk_hdr <- ""

  add_mcq_block <- function(v, i) {
    if (assessment == "code") {
      return(v)
    }

    if (format == "learnr") {
      c(
        v,
        sprintf("```{r mcq%d, echo=FALSE}", i),
        'learnr::question(',
        '  "TODO - edit your question here",',
        '  answer("A", correct = TRUE),',
        '  answer("B"),',
        '  answer("C")',
        ')',
        "```",
        ""
      )
    } else {
      # Quarto Live MCQ placeholder
      c(
        v,
        "::: {.callout-note}",
        "## Question",
        "TODO - edit your question here",
        ":::",
        ""
      )
    }
  }

  for (ln in lines) {
    if (grepl("^```", ln)) {
      # -- fence
      if (!in_chunk) {
        # -- opening
        in_chunk <- TRUE
        buf <- character()
        chunk_hdr <- ln
      } else {
        # -- closing
        in_chunk <- FALSE

        # -----------------------------------------------------------------
        # 1) Is it a setup chunk?
        # -----------------------------------------------------------------
        if (grepl("setup", chunk_hdr, ignore.case = TRUE)) {
          # keep chunk exactly as it was
          out <- c(out, chunk_hdr, buf, ln, "")
          next
        }

        # -----------------------------------------------------------------
        # 1.5) Is it a skipped chunk?
        # -----------------------------------------------------------------
        # Check for # tutorizeR: skip in the buffer or header
        # (Checking buffer is easier as we have it now)
        is_skipped <- any(grepl(
          "#\\s*tutorizeR:\\s*skip",
          buf,
          ignore.case = TRUE
        ))

        if (is_skipped) {
          # Keep as regular chunk (maybe remove the skip comment?)
          # For now, just keep it verbatim
          out <- c(out, chunk_hdr, buf, ln, "")
          next
        }

        # -----------------------------------------------------------------
        # 2) Regular chunk -> exercise + solution
        # -----------------------------------------------------------------
        id <- id + 1

        # If assessment is ONLY MCQ, we might want to show the code as static
        # or just hide it? Let's assume we show it as static code if not "code" mode?
        # Actually, "mcq" mode usually implies the code IS the question context.
        # But the user request implies "Generate an MCQ *instead* of an exercise".
        # Let's keep it simple:
        # If assessment == "code" or "both": Generate Exercise
        # If assessment == "mcq": Generate Static Code (Context) + MCQ

        generate_exercise <- assessment %in% c("code", "both")

        if (generate_exercise) {
          if (format == "learnr") {
            exercise <- c(
              sprintf("```{r ex%d, exercise=TRUE, exercise.lines=5}", id),
              "# Write your code below",
              "```"
            )

            solution <- c(
              sprintf("```{r ex%d-solution}", id),
              buf,
              "```"
            )

            out <- c(out, exercise, "", solution, "")
          } else {
            # Quarto Live format
            exercise <- c(
              "{webr}",
              "# Write your code below"
            )

            solution <- c(
              "::: {.callout-tip collapse='true'}",
              "## Solution",
              "```r",
              buf,
              "```",
              ":::"
            )

            out <- c(out, exercise, "", solution, "")
          }
        } else {
          # MCQ only mode: Just show the original code as a static block (context)
          # Or maybe the user wants NO code block?
          # Let's assume they want the original code as context.
          out <- c(out, chunk_hdr, buf, ln, "")
        }

        out <- add_mcq_block(out, id)
      }
    } else if (in_chunk) {
      # inside chunk
      buf <- c(buf, ln)
    } else {
      # narrative text
      out <- c(out, ln)
    }
  }
  out
}


#' @return Character vector of YAML + setup chunk.
#' @keywords internal

template_header_rmd <- function() {
  c(
    "---",
    'title: "Interactive Tutorial"',
    "output: learnr::tutorial",
    "runtime: shiny_prerendered",
    "---",
    "",
    "```{r setup, include=FALSE}",
    "library(learnr)",
    "library(gradethis)",
    "gradethis_setup()",
    "```"
  )
}

#' @return Character vector of YAML for Quarto Live.
#' @keywords internal
template_header_quarto_live <- function() {
  c(
    "---",
    'title: "Interactive Tutorial"',
    "format: live-html",
    "engine: knitr",
    "---",
    "",
    "{{< include ./_extensions/r-wasm/live/_knitr.qmd >}}",
    ""
  )
}

#' Remove leading YAML front-matter (if present)
#' @keywords internal
strip_yaml <- function(x) {
  if (length(x) == 0L) {
    return(x)
  }
  first <- which(trimws(x) != "")[1]
  if (is.na(first) || trimws(x[first]) != "---") {
    return(x)
  }

  end <- which(trimws(x[(first + 1):length(x)]) %in% c("---", "..."))
  if (length(end) == 0L) {
    return(x)
  } # malformed
  end <- end[1] + first

  x[-seq(first, end)] # drop YAML
}


#' Render a generated tutorial to ensure it compiles
#'
#' The file is rendered in a temporary directory; warnings and errors are
#' captured.  If rendering fails, a short message is printed.
#'
#' @param file Path to the tutorial `.Rmd` you want to test.
#' @return Invisibly, the path to the rendered HTML **or** the error object.
#' @export
#' @importFrom rmarkdown render
check_tutorial <- function(file) {
  if (!file.exists(file)) {
    stop("File not found: ", file, call. = FALSE)
  }

  tmpdir <- tempfile("tut-render-")
  dir.create(tmpdir)
  result <- tryCatch(
    rmarkdown::render(
      file,
      output_dir = tmpdir,
      quiet = TRUE,
      envir = new.env(parent = globalenv())
    ),
    error = identity
  )

  if (inherits(result, "error")) {
    cli::cli_alert_danger("Rendering failed:\n{result$message}")
  } else {
    cli::cli_alert_success("Tutorial renders without error.")
  }
  invisible(result)
}
