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
      tr("errors.addin_rstudio_only"),
      class = c("tutorizeR_error_validation", "tutorizeR_error")
    )
  }

  ctx <- rstudioapi::getSourceEditorContext()
  path <- ctx$path

  if (!nzchar(path)) {
    rlang::abort(
      tr("errors.addin_save_file"),
      class = c("tutorizeR_error_validation", "tutorizeR_error")
    )
  }

  format_in <- rstudioapi::showPrompt(
    title = "tutorizeR",
    message = tr("addin.output_format"),
    default = "learnr"
  )
  if (is.null(format_in)) {
    return(invisible(NULL))
  }

  assessment_in <- rstudioapi::showPrompt(
    title = "tutorizeR",
    message = tr("addin.assessment"),
    default = "both"
  )
  if (is.null(assessment_in)) {
    return(invisible(NULL))
  }

  overwrite <- isTRUE(rstudioapi::showQuestion(
    title = "tutorizeR",
    message = tr("addin.overwrite_question"),
    ok = "Yes",
    cancel = "No"
  ))

  out_dir <- rstudioapi::selectDirectory(caption = tr("addin.select_output_dir"))
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
      tr("errors.addin_rstudio_only"),
      class = c("tutorizeR_error_validation", "tutorizeR_error")
    )
  }

  folder <- rstudioapi::selectDirectory(caption = tr("addin.select_folder"))
  if (is.null(folder) || !nzchar(folder)) {
    return(invisible(NULL))
  }

  format_in <- rstudioapi::showPrompt(
    title = "tutorizeR",
    message = tr("addin.output_format"),
    default = "learnr"
  )
  if (is.null(format_in)) {
    return(invisible(NULL))
  }

  assessment_in <- rstudioapi::showPrompt(
    title = "tutorizeR",
    message = tr("addin.assessment"),
    default = "both"
  )
  if (is.null(assessment_in)) {
    return(invisible(NULL))
  }

  recursive <- isTRUE(rstudioapi::showQuestion(
    title = "tutorizeR",
    message = tr("addin.recursive_question"),
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

#' RStudio Addin: Preview conversion with lint and diff
#'
#' Opens a gadget with Source, Output, Diff, Lint and Logs tabs.
#'
#' @return Invisibly returns a `tutorize_report` object or `NULL`.
#' @export
launch_tutorizeR_preview_addin <- function() {
  if (!rstudioapi::isAvailable()) {
    rlang::abort(
      tr("errors.addin_rstudio_only"),
      class = c("tutorizeR_error_validation", "tutorizeR_error")
    )
  }

  if (!requireNamespace("shiny", quietly = TRUE) || !requireNamespace("miniUI", quietly = TRUE)) {
    rlang::abort(
      "The preview addin requires packages 'shiny' and 'miniUI'.",
      class = c("tutorizeR_error_validation", "tutorizeR_error")
    )
  }

  ctx <- rstudioapi::getSourceEditorContext()
  input_file <- ctx$path

  if (!nzchar(input_file)) {
    rlang::abort(
      tr("errors.addin_save_file"),
      class = c("tutorizeR_error_validation", "tutorizeR_error")
    )
  }

  if (!interactive()) {
    rlang::abort(
      "Preview addin requires an interactive R session.",
      class = c("tutorizeR_error_validation", "tutorizeR_error")
    )
  }

  run_preview_gadget(input_file)
}

#' Run conversion preview gadget
#' @keywords internal
run_preview_gadget <- function(input_file) {
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar(tr("addin.preview_title")),
    miniUI::miniContentPanel(
      shiny::fluidRow(
        shiny::column(
          width = 3,
          shiny::selectInput("format", "Format", choices = c("learnr", "quarto-live"), selected = "learnr"),
          shiny::selectInput("assessment", "Assessment", choices = c("code", "mcq", "both"), selected = "both"),
          shiny::checkboxInput("force_convert", tr("addin.preview_force"), value = FALSE),
          shiny::actionButton("refresh", tr("addin.preview_refresh"), class = "btn-primary")
        ),
        shiny::column(
          width = 9,
          shiny::tabsetPanel(
            shiny::tabPanel("Source", shiny::verbatimTextOutput("source_text")),
            shiny::tabPanel("Output", shiny::verbatimTextOutput("output_text")),
            shiny::tabPanel("Diff", shiny::verbatimTextOutput("diff_text")),
            shiny::tabPanel("Lint", shiny::verbatimTextOutput("lint_text")),
            shiny::tabPanel("Logs", shiny::verbatimTextOutput("log_text"))
          )
        )
      )
    )
  )

  server <- function(input, output, session) {
    source_lines <- readLines(input_file, warn = FALSE)
    state <- shiny::reactiveValues(preview = NULL, lint = NULL, logs = "")

    preview_once <- function() {
      lint <- tryCatch(
        lint_source(input_file, strict = FALSE),
        error = identity
      )

      tmp_ext <- if (identical(input$format, "learnr")) ".Rmd" else ".qmd"
      tmp_out <- tempfile(fileext = tmp_ext)

      conv <- tryCatch(
        tutorize(
          input = input_file,
          output_file = tmp_out,
          format = input$format,
          assessment = input$assessment,
          overwrite = TRUE,
          verbose = FALSE,
          lint_strict = FALSE
        ),
        error = identity
      )

      state$lint <- lint
      state$preview <- conv

      log_lines <- c(
        sprintf("Input: %s", input_file),
        sprintf("Format: %s", input$format),
        sprintf("Assessment: %s", input$assessment)
      )

      if (inherits(conv, "error")) {
        log_lines <- c(log_lines, sprintf("Conversion error: %s", conditionMessage(conv)))
      } else {
        log_lines <- c(log_lines, sprintf("Preview output: %s", conv$output_file))
      }

      state$logs <- paste(log_lines, collapse = "\n")
    }

    shiny::observeEvent(input$refresh, preview_once(), ignoreInit = FALSE)

    output$source_text <- shiny::renderText({
      paste(source_lines, collapse = "\n")
    })

    output$output_text <- shiny::renderText({
      if (is.null(state$preview)) return("")
      if (inherits(state$preview, "error")) return(conditionMessage(state$preview))
      paste(readLines(state$preview$output_file, warn = FALSE), collapse = "\n")
    })

    output$diff_text <- shiny::renderText({
      if (is.null(state$preview) || inherits(state$preview, "error")) return("")
      out_lines <- readLines(state$preview$output_file, warn = FALSE)
      generate_simple_diff(source_lines, out_lines)
    })

    output$lint_text <- shiny::renderPrint({
      if (is.null(state$lint)) return(invisible(NULL))
      print(state$lint)
    })

    output$log_text <- shiny::renderText({
      state$logs
    })

    shiny::observeEvent(input$done, {
      if (inherits(state$lint, "tutorize_lint_report") && has_lint_errors(state$lint) && !isTRUE(input$force_convert)) {
        shiny::showNotification("Lint errors detected. Enable force conversion to continue.", type = "error")
        return()
      }

      report <- tryCatch(
        tutorize(
          input = input_file,
          format = input$format,
          assessment = input$assessment,
          overwrite = TRUE,
          verbose = TRUE,
          lint_strict = FALSE
        ),
        error = identity
      )

      shiny::stopApp(report)
    })

    shiny::observeEvent(input$cancel, {
      shiny::stopApp(NULL)
    })
  }

  shiny::runGadget(ui, server, viewer = shiny::dialogViewer(tr("addin.preview_title")))
}

#' Build a simple line diff between source and output
#' @keywords internal
generate_simple_diff <- function(source_lines, output_lines) {
  max_len <- max(length(source_lines), length(output_lines))
  source_lines <- c(source_lines, rep("", max_len - length(source_lines)))
  output_lines <- c(output_lines, rep("", max_len - length(output_lines)))

  diffs <- character()
  for (i in seq_len(max_len)) {
    s <- source_lines[i]
    o <- output_lines[i]
    if (!identical(s, o)) {
      diffs <- c(diffs, sprintf("Line %d", i), paste0("- ", s), paste0("+ ", o), "")
    }
  }

  if (length(diffs) == 0L) {
    return("No differences.")
  }

  paste(diffs, collapse = "\n")
}
