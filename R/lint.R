# Linting -----------------------------------------------------------------

#' Lint source pedagogical constraints before conversion
#'
#' @param input Source `.Rmd` or `.qmd` file.
#' @param question_bank Optional `tutorize_question_bank` object.
#' @param language Message language (`"en"` or `"fr"`).
#' @param strict Logical; if `TRUE`, stop when lint errors are present.
#'
#' @return A `tutorize_lint_report` object.
#' @export
lint_source <- function(
  input,
  question_bank = NULL,
  language = c("en", "fr"),
  strict = FALSE
) {
  language <- match.arg(language)

  validate_input(input = input, format = "learnr", assessment = "code", language = language)
  parsed <- parse_input_document(input)

  findings <- list()
  label_seen <- new.env(parent = emptyenv())
  setup_count <- 0L

  for (block in parsed$blocks) {
    if (!identical(block$type, "chunk")) {
      next
    }

    line <- block$start_line %||% NA_integer_

    if (!is.null(block$label) && nzchar(block$label)) {
      label <- block$label
      if (exists(label, envir = label_seen, inherits = FALSE)) {
        findings[[length(findings) + 1L]] <- lint_row(
          code = "TRZ001",
          severity = "error",
          message = sprintf("Duplicate chunk label '%s'", label),
          line = line,
          chunk_label = label
        )
      }
      assign(label, TRUE, envir = label_seen)
    }

    if (is_setup_chunk(block)) {
      setup_count <- setup_count + 1L
    }

    unknown_tags <- detect_unknown_tags(block$body_lines)
    if (length(unknown_tags) > 0L) {
      for (tag in unknown_tags) {
        findings[[length(findings) + 1L]] <- lint_row(
          code = "TRZ002",
          severity = "warning",
          message = tr("errors.lint_unknown_tag", tag = tag, language = language),
          line = line,
          chunk_label = block$label %||% ""
        )
      }
    }

    conflict <- detect_tag_conflict(block$tags)
    if (length(conflict) > 0L) {
      findings[[length(findings) + 1L]] <- lint_row(
        code = "TRZ003",
        severity = "error",
        message = tr(
          "errors.lint_tag_conflict",
          label = block$label %||% "",
          tags = paste(conflict, collapse = ", "),
          language = language
        ),
        line = line,
        chunk_label = block$label %||% ""
      )
    }

    if (is_mcq_block(block)) {
      ok <- tryCatch({
        definition <- parse_mcq_yaml(block)
        validate_mcq_definition(definition, index = 1L)
        TRUE
      }, error = function(e) e)

      if (inherits(ok, "error")) {
        findings[[length(findings) + 1L]] <- lint_row(
          code = "TRZ004",
          severity = "error",
          message = conditionMessage(ok),
          line = line,
          chunk_label = block$label %||% ""
        )
      }
    }

    if (is_mcq_ref_block(block)) {
      if (is.null(question_bank)) {
        findings[[length(findings) + 1L]] <- lint_row(
          code = "TRZ005",
          severity = "error",
          message = tr("errors.qb_ref_requires_bank", language = language),
          line = line,
          chunk_label = block$label %||% ""
        )
      } else {
        definition <- tryCatch(parse_mcq_ref_yaml(block), error = identity)
        if (inherits(definition, "error")) {
          findings[[length(findings) + 1L]] <- lint_row(
            code = "TRZ006",
            severity = "error",
            message = conditionMessage(definition),
            line = line,
            chunk_label = block$label %||% ""
          )
        } else if (length(definition$ids) > 0L) {
          available <- vapply(question_bank$questions, function(q) q$id, character(1))
          missing_ids <- setdiff(definition$ids, available)
          if (length(missing_ids) > 0L) {
            for (id in missing_ids) {
              findings[[length(findings) + 1L]] <- lint_row(
                code = "TRZ007",
                severity = "error",
                message = tr("errors.qb_ref_unknown_id", id = id, language = language),
                line = line,
                chunk_label = block$label %||% ""
              )
            }
          }
        }
      }
    }

    if (is_r_chunk(block) && !is.null(block$options_raw) && nzchar(block$options_raw)) {
      opts <- split_option_tokens(block$options_raw)
      if (any(grepl("^eval\\s*=\\s*FALSE$", opts, ignore.case = TRUE)) && isTRUE(block$tags$exercise_only)) {
        findings[[length(findings) + 1L]] <- lint_row(
          code = "TRZ008",
          severity = "warning",
          message = "Chunk has eval=FALSE and exercise-only tag; learners may get empty context.",
          line = line,
          chunk_label = block$label %||% ""
        )
      }
    }
  }

  if (setup_count > 1L) {
    findings[[length(findings) + 1L]] <- lint_row(
      code = "TRZ009",
      severity = "warning",
      message = sprintf("Detected %d setup chunks; verify setup side effects.", setup_count),
      line = NA_integer_,
      chunk_label = "setup"
    )
  }

  findings_df <- if (length(findings) == 0L) empty_findings() else do.call(rbind, findings)

  report <- new_tutorize_lint_report(
    source = input,
    findings = findings_df,
    context = "source"
  )

  if (isTRUE(strict) && has_lint_errors(report)) {
    rlang::abort(
      tr("messages.lint_blocking", n = sum(report$findings$severity == "error"), language = language),
      class = c("tutorizeR_error_validation", "tutorizeR_error")
    )
  }

  report
}

#' Build an empty lint findings table
#' @keywords internal
empty_findings <- function() {
  data.frame(
    code = character(),
    severity = character(),
    message = character(),
    line = integer(),
    chunk_label = character(),
    stringsAsFactors = FALSE
  )
}

#' Build a lint finding row
#' @keywords internal
lint_row <- function(code, severity, message, line = NA_integer_, chunk_label = "") {
  data.frame(
    code = as.character(code),
    severity = as.character(severity),
    message = as.character(message),
    line = as.integer(line),
    chunk_label = as.character(chunk_label),
    stringsAsFactors = FALSE
  )
}

#' Construct a lint report object
#' @keywords internal
new_tutorize_lint_report <- function(source, findings, context = "source") {
  if (is.null(findings) || nrow(findings) == 0L) {
    findings <- empty_findings()
  }

  if (nrow(findings) > 0L) {
    findings$severity <- tolower(findings$severity)
  }

  structure(
    list(
      source = source,
      context = context,
      findings = findings,
      summary = list(
        errors = sum(findings$severity == "error"),
        warnings = sum(findings$severity == "warning"),
        infos = sum(findings$severity == "info")
      )
    ),
    class = "tutorize_lint_report"
  )
}

#' Check whether lint report contains blocking errors
#' @keywords internal
has_lint_errors <- function(report) {
  inherits(report, "tutorize_lint_report") && sum(report$findings$severity == "error") > 0L
}

#' Print method for lint reports
#'
#' @param x A `tutorize_lint_report` object.
#' @param ... Unused.
#'
#' @return `x`, invisibly.
#' @export
print.tutorize_lint_report <- function(x, ...) {
  cat("tutorizeR lint report\n")
  cat("- source:   ", x$source, "\n", sep = "")
  cat("- errors:   ", x$summary$errors, "\n", sep = "")
  cat("- warnings: ", x$summary$warnings, "\n", sep = "")
  cat("- infos:    ", x$summary$infos, "\n", sep = "")

  if (nrow(x$findings) > 0L) {
    cat("\nFindings:\n")
    for (i in seq_len(nrow(x$findings))) {
      row <- x$findings[i, , drop = FALSE]
      where <- if (!is.na(row$line)) paste0("line ", row$line) else "line ?"
      cat("- [", toupper(row$severity), "] ", row$code, " (", where, "): ", row$message, "\n", sep = "")
    }
  }

  invisible(x)
}

#' Detect unknown tutorizeR tags from comment lines
#' @keywords internal
detect_unknown_tags <- function(lines) {
  if (length(lines) == 0L) {
    return(character())
  }

  tag_lines <- grep("^\\s*#\\s*tutorizeR\\s*:", lines, ignore.case = TRUE, value = TRUE)
  if (length(tag_lines) == 0L) {
    return(character())
  }

  known <- c("skip", "exercise-only", "solution-only", "mcq", "narrative-only", "locked", "hints")
  unknown <- character()

  for (line in tag_lines) {
    payload <- sub("^\\s*#\\s*tutorizeR\\s*:\\s*", "", line, ignore.case = TRUE)
    fields <- split_option_tokens(payload)
    for (field in fields) {
      key <- tolower(trimws(sub("=.*$", "", field)))
      if (!(key %in% known)) {
        unknown <- c(unknown, key)
      }
    }
  }

  unique(unknown)
}

#' Detect conflicting tag combinations
#' @keywords internal
detect_tag_conflict <- function(tags) {
  if (is.null(tags) || !is.list(tags)) {
    return(character())
  }

  active <- c(
    if (isTRUE(tags$skip)) "skip" else NULL,
    if (isTRUE(tags$exercise_only)) "exercise-only" else NULL,
    if (isTRUE(tags$solution_only)) "solution-only" else NULL,
    if (isTRUE(tags$mcq)) "mcq" else NULL,
    if (isTRUE(tags$narrative_only)) "narrative-only" else NULL
  )

  conflicts <- character()

  if ("skip" %in% active && length(setdiff(active, "skip")) > 0L) {
    conflicts <- c(conflicts, active)
  }

  if ("exercise-only" %in% active && "solution-only" %in% active) {
    conflicts <- c(conflicts, "exercise-only", "solution-only")
  }

  unique(conflicts)
}
