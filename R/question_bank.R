# Question bank -----------------------------------------------------------

#' Load a reusable MCQ question bank from YAML/JSON files
#'
#' @param path File or directory path(s) containing question bank files.
#' @param recursive Logical; recurse into subdirectories when `path` is a directory.
#' @param format Input format: `"auto"`, `"yaml"`, or `"json"`.
#' @param strict Logical; if `TRUE`, validation errors stop execution.
#' @param language Language for validation messages (`"en"` or `"fr"`).
#'
#' @return A `tutorize_question_bank` object.
#' @export
load_question_bank <- function(
  path,
  recursive = TRUE,
  format = c("auto", "yaml", "json"),
  strict = TRUE,
  language = c("en", "fr")
) {
  format <- match.arg(format)
  language <- match.arg(language)

  if (missing(path) || is.null(path) || length(path) == 0L) {
    rlang::abort(
      tr("errors.qb_not_found", path = "<missing>", language = language),
      class = c("tutorizeR_error_validation", "tutorizeR_error")
    )
  }

  files <- resolve_question_bank_files(path, recursive = recursive, format = format)

  if (length(files) == 0L) {
    rlang::abort(
      tr("errors.qb_not_found", path = paste(path, collapse = ", "), language = language),
      class = c("tutorizeR_error_validation", "tutorizeR_error")
    )
  }

  items <- list()
  for (file in files) {
    parsed <- parse_question_bank_file(file)
    if (length(parsed) > 0L) {
      items <- c(items, parsed)
    }
  }

  bank <- structure(
    list(
      questions = items,
      source_files = files,
      loaded_at = Sys.time()
    ),
    class = "tutorize_question_bank"
  )

  report <- validate_question_bank(bank, strict = strict, language = language)
  bank$validation <- report
  bank
}

#' Validate a `tutorize_question_bank` object
#'
#' @param bank A question bank object from [load_question_bank()].
#' @param strict Logical; if `TRUE`, stop on validation errors.
#' @param language Language for validation messages (`"en"` or `"fr"`).
#'
#' @return A `tutorize_lint_report` object.
#' @export
validate_question_bank <- function(bank, strict = TRUE, language = c("en", "fr")) {
  language <- match.arg(language)

  if (!inherits(bank, "tutorize_question_bank")) {
    rlang::abort(
      tr("errors.qb_invalid_object", language = language),
      class = c("tutorizeR_error_validation", "tutorizeR_error")
    )
  }

  findings <- list()
  ids <- character()

  for (q in bank$questions) {
    q_findings <- validate_question_item(q, language = language)
    if (nrow(q_findings) > 0L) {
      findings[[length(findings) + 1L]] <- q_findings
    }

    ids <- c(ids, q$id %||% "")
  }

  duplicated_ids <- unique(ids[duplicated(ids) & nzchar(ids)])
  if (length(duplicated_ids) > 0L) {
    for (id in duplicated_ids) {
      findings[[length(findings) + 1L]] <- data.frame(
        code = "TRZQB007",
        severity = "error",
        message = paste0("Duplicate question id: ", id),
        line = NA_integer_,
        chunk_label = id,
        stringsAsFactors = FALSE
      )
    }
  }

  findings_df <- if (length(findings) == 0L) {
    empty_findings()
  } else {
    do.call(rbind, findings)
  }

  report <- new_tutorize_lint_report(
    source = paste(bank$source_files, collapse = ","),
    findings = findings_df,
    context = "question_bank"
  )

  if (isTRUE(strict) && has_lint_errors(report)) {
    rlang::abort(
      tr("messages.lint_blocking", n = sum(report$findings$severity == "error"), language = language),
      class = c("tutorizeR_error_validation", "tutorizeR_error")
    )
  }

  report
}

#' Select question(s) from a loaded question bank
#' @keywords internal
select_questions_from_bank <- function(
  bank,
  ids = NULL,
  n = NULL,
  strategy = c("ordered", "random"),
  tags = NULL,
  difficulty = NULL,
  language = NULL
) {
  strategy <- match.arg(strategy)
  language <- if (is.null(language)) NULL else resolve_language(language)

  questions <- bank$questions
  if (length(questions) == 0L) {
    return(list())
  }

  keep <- rep(TRUE, length(questions))

  if (!is.null(ids) && length(ids) > 0L) {
    ids <- as.character(ids)
    keep <- keep & vapply(questions, function(q) q$id %in% ids, logical(1))
  }

  if (!is.null(tags) && length(tags) > 0L) {
    tags <- tolower(as.character(tags))
    keep <- keep & vapply(questions, function(q) {
      any(tolower(q$tags %||% character()) %in% tags)
    }, logical(1))
  }

  if (!is.null(difficulty) && nzchar(difficulty)) {
    difficulty <- tolower(as.character(difficulty)[1])
    keep <- keep & vapply(questions, function(q) {
      identical(tolower(q$difficulty %||% ""), difficulty)
    }, logical(1))
  }

  if (!is.null(language) && nzchar(language)) {
    keep <- keep & vapply(questions, function(q) {
      identical(tolower(q$language %||% "en"), language)
    }, logical(1))
  }

  selected <- questions[keep]

  if (length(selected) == 0L) {
    return(list())
  }

  if (!is.null(ids) && length(ids) > 0L) {
    selected <- selected[match(ids, vapply(selected, `[[`, character(1), "id"), nomatch = 0L)]
    selected <- selected[lengths(selected) > 0L]
  }

  if (!is.null(n) && is.finite(n) && n > 0L && length(selected) > n) {
    n <- as.integer(n)
    if (strategy == "random") {
      selected <- sample(selected, n)
    } else {
      selected <- selected[seq_len(n)]
    }
  }

  selected
}

#' Resolve file list for question-bank loading
#' @keywords internal
resolve_question_bank_files <- function(path, recursive = TRUE, format = "auto") {
  paths <- unique(as.character(path))
  files <- character()

  for (p in paths) {
    if (dir.exists(p)) {
      pattern <- switch(
        format,
        yaml = "\\.(yml|yaml)$",
        json = "\\.json$",
        auto = "\\.(yml|yaml|json)$"
      )
      files <- c(files, list.files(p, pattern = pattern, recursive = recursive, full.names = TRUE, ignore.case = TRUE))
    } else if (file.exists(p)) {
      files <- c(files, p)
    }
  }

  unique(files)
}

#' Parse a single question-bank file
#' @keywords internal
parse_question_bank_file <- function(file) {
  ext <- tolower(tools::file_ext(file))

  raw <- if (ext %in% c("yml", "yaml")) {
    yaml::yaml.load_file(file, eval.expr = FALSE)
  } else if (ext == "json") {
    jsonlite::fromJSON(file, simplifyVector = FALSE)
  } else {
    list()
  }

  if (is.null(raw)) {
    return(list())
  }

  entries <- normalize_question_entries(raw)
  lapply(entries, normalize_question_item, source_file = file)
}

#' Normalize root object to list of question entries
#' @keywords internal
normalize_question_entries <- function(raw) {
  if (is.list(raw) && !is.null(raw$questions) && is.list(raw$questions)) {
    return(raw$questions)
  }

  if (is.list(raw) && length(raw) > 0L && all(vapply(raw, is.list, logical(1)))) {
    return(raw)
  }

  if (is.list(raw) && !is.null(raw$id)) {
    return(list(raw))
  }

  list()
}

#' Normalize one question entry to a canonical list
#' @keywords internal
normalize_question_item <- function(x, source_file = "") {
  answers <- x$answers %||% x$choices

  if (is.null(answers)) {
    answers <- list()
  }

  answers <- lapply(answers, function(ans) {
    if (is.character(ans) && length(ans) == 1L) {
      list(text = ans, correct = FALSE)
    } else {
      list(
        text = ans$text %||% "",
        correct = isTRUE(ans$correct)
      )
    }
  })

  tags <- x$tags %||% character()
  if (is.character(tags) && length(tags) == 1L && grepl(",", tags, fixed = TRUE)) {
    tags <- trimws(strsplit(tags, ",", fixed = TRUE)[[1]])
  }

  hints <- x$hints %||% character()
  if (is.character(hints) && length(hints) == 1L && grepl("\\|", hints)) {
    hints <- trimws(strsplit(hints, "\\|", perl = TRUE)[[1]])
  }

  list(
    id = as.character(x$id %||% ""),
    question = as.character(x$question %||% ""),
    answers = answers,
    tags = as.character(tags),
    difficulty = as.character(x$difficulty %||% ""),
    language = as.character(x$language %||% "en"),
    explanation = as.character(x$explanation %||% ""),
    hints = as.character(hints),
    source_file = source_file
  )
}

#' Validate one normalized question item
#' @keywords internal
validate_question_item <- function(q, language = "en") {
  findings <- list()

  if (!nzchar(q$id)) {
    findings[[length(findings) + 1L]] <- lint_row("TRZQB001", "error", "Missing required field: id", q$id)
  }

  if (!nzchar(q$question)) {
    findings[[length(findings) + 1L]] <- lint_row("TRZQB002", "error", "Missing required field: question", q$id)
  }

  if (length(q$answers) < 2L) {
    findings[[length(findings) + 1L]] <- lint_row("TRZQB003", "error", "At least two answers are required", q$id)
  } else {
    has_correct <- FALSE
    for (ans in q$answers) {
      if (!nzchar(ans$text %||% "")) {
        findings[[length(findings) + 1L]] <- lint_row("TRZQB004", "error", "Answer text is required", q$id)
      }
      has_correct <- has_correct || isTRUE(ans$correct)
    }
    if (!has_correct) {
      findings[[length(findings) + 1L]] <- lint_row("TRZQB005", "error", "At least one answer must be correct", q$id)
    }
  }

  if (!nzchar(q$difficulty)) {
    findings[[length(findings) + 1L]] <- lint_row("TRZQB006", "error", "Missing required field: difficulty", q$id)
  }

  if (length(q$tags) == 0L) {
    findings[[length(findings) + 1L]] <- lint_row("TRZQB009", "error", "Missing required field: tags", q$id)
  }

  if (!nzchar(q$language)) {
    findings[[length(findings) + 1L]] <- lint_row("TRZQB008", "error", "Missing required field: language", q$id)
  }

  if (length(findings) == 0L) {
    return(empty_findings())
  }

  do.call(rbind, findings)
}
