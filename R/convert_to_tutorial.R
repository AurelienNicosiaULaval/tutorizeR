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
#'        \code{<input>\emph{-tutorial.Rmd}}.
#' @param add_mcq     Logical, should an empty MCQ block be added
#'        after each exercise/solution pair?
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
  add_mcq = FALSE
) {
  stopifnot(file.exists(input_file))

  # 1 · read + strip YAML
  lines <- strip_yaml(readLines(input_file, warn = FALSE))

  # 2 · transform chunks
  body <- parse_chunks(lines, add_mcq = add_mcq)
  header <- template_header_rmd()
  out <- c(header, "", body)

  # 3 · write output
  if (is.null(output_file)) {
    output_file <- sub("\\.(R|r|Q|q)md$", "-tutorial.Rmd", input_file)
  }
  writeLines(out, output_file)
  cli::cli_alert_success("Tutorial written to {output_file}")

  # 4 · automatically render & report
  res <- check_tutorial(output_file)
  if (inherits(res, "error")) {
    cli::cli_alert_danger(
      "❌ Rendering failed. Inspect {output_file} for problematic code."
    )
  } else {
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
#' @param lines   Character vector, output of \code{readLines()}.
#' @param add_mcq Logical, add MCQ skeletons?
#'
#' @return Character vector representing the transformed body.
#' @keywords internal

parse_chunks <- function(lines, add_mcq = FALSE) {
  in_chunk <- FALSE
  buf <- character()
  out <- character()
  id <- 0
  chunk_hdr <- ""

  add_mcq_block <- function(v, i) {
    if (!add_mcq) return(v)
    c(
      v,
      sprintf("```{r mcq%d, echo=FALSE}", i),
      'learnr::question(',
      '  "TODO – edit your question here",',
      '  answer("A", correct = TRUE),',
      '  answer("B"),',
      '  answer("C")',
      ')',
      "```",
      ""
    )
  }

  for (ln in lines) {
    if (grepl("^```", ln)) {
      # ── fence
      if (!in_chunk) {
        # ── opening
        in_chunk <- TRUE
        buf <- character()
        chunk_hdr <- ln
      } else {
        # ── closing
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
        # 2) Regular chunk → exercise + solution
        # -----------------------------------------------------------------
        id <- id + 1

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

#' Remove leading YAML front-matter (if present)
#' @keywords internal
strip_yaml <- function(x) {
  if (length(x) == 0L) return(x)
  first <- which(trimws(x) != "")[1]
  if (is.na(first) || trimws(x[first]) != "---") return(x)

  end <- which(trimws(x[(first + 1):length(x)]) %in% c("---", "..."))
  if (length(end) == 0L) return(x) # malformed
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
  if (!file.exists(file)) stop("File not found: ", file, call. = FALSE)

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
    cli::cli_alert_danger("❌ Rendering failed:\n{result$message}")
  } else {
    cli::cli_alert_success("✅ Tutorial renders without error.")
  }
  invisible(result)
}
