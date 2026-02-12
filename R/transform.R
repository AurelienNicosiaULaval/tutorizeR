# Internal transformation helpers ----------------------------------------

#' Build converted tutorial lines from parsed document blocks
#' @keywords internal
build_tutorial_lines <- function(parsed, format, assessment, language = "en", seed = NULL) {
  output <- character()
  registry <- new.env(parent = emptyenv())

  stats <- list(
    exercises = 0L,
    solutions = 0L,
    mcq = 0L,
    skipped = 0L,
    setup_chunks = 0L,
    preserved_chunks = 0L
  )

  has_setup <- FALSE
  chunk_index <- 0L

  for (block in parsed$blocks) {
    if (identical(block$type, "text")) {
      output <- c(output, block$lines)
      next
    }

    if (!identical(block$type, "chunk")) {
      next
    }

    chunk_index <- chunk_index + 1L

    if (is_mcq_block(block)) {
      if (assessment %in% c("mcq", "both")) {
        mcq_lines <- transform_mcq_block(block, format = format, index = chunk_index)
        output <- c(output, mcq_lines, "")
        stats$mcq <- stats$mcq + 1L
      }
      next
    }

    if (!is_r_chunk(block)) {
      output <- c(output, block$raw_lines, "")
      if (!is.null(block$label) && nzchar(block$label)) {
        register_label(block$label, registry)
      }
      stats$preserved_chunks <- stats$preserved_chunks + 1L
      next
    }

    if (is_setup_chunk(block)) {
      has_setup <- TRUE
      setup_lines <- transform_setup_chunk(
        block,
        format = format,
        language = language,
        seed = seed,
        registry = registry
      )
      output <- c(output, setup_lines, "")
      stats$setup_chunks <- stats$setup_chunks + 1L
      next
    }

    tags <- block$tags

    if (isTRUE(tags$skip) || isTRUE(tags$narrative_only)) {
      output <- c(output, block$raw_lines, "")
      if (!is.null(block$label) && nzchar(block$label)) {
        register_label(block$label, registry)
      }
      stats$skipped <- stats$skipped + 1L
      next
    }

    mode <- derive_assessment_mode(assessment = assessment, tags = tags)

    label_base <- sanitize_label(block$label %||% sprintf("chunk-%03d", chunk_index))

    if (mode$exercise) {
      ex_label <- make_unique_label(paste0("trz-ex-", label_base), registry)
      sol_label <- make_unique_label(paste0("trz-sol-", label_base), registry)

      exercise_lines <- build_exercise_lines(
        format = format,
        exercise_label = ex_label,
        source_chunk = block,
        language = language,
        tags = tags
      )
      output <- c(output, exercise_lines, "")
      stats$exercises <- stats$exercises + 1L

      if (!isTRUE(tags$exercise_only)) {
        solution_lines <- build_solution_lines(
          format = format,
          solution_label = sol_label,
          source_chunk = block,
          language = language
        )
        output <- c(output, solution_lines, "")
        stats$solutions <- stats$solutions + 1L
      }
    }

    if (mode$solution_only && !mode$exercise) {
      sol_label <- make_unique_label(paste0("trz-sol-", label_base), registry)
      solution_lines <- build_solution_lines(
        format = format,
        solution_label = sol_label,
        source_chunk = block,
        language = language
      )
      output <- c(output, solution_lines, "")
      stats$solutions <- stats$solutions + 1L
    }

    if (mode$mcq) {
      mcq_label <- make_unique_label(paste0("trz-mcq-", label_base), registry)
      output <- c(
        output,
        build_placeholder_mcq(
          format = format,
          mcq_label = mcq_label,
          source_chunk = block,
          language = language
        ),
        ""
      )
      stats$mcq <- stats$mcq + 1L
    }

    if (!mode$exercise && !mode$mcq && !mode$solution_only) {
      output <- c(output, block$raw_lines, "")
      if (!is.null(block$label) && nzchar(block$label)) {
        register_label(block$label, registry)
      }
      stats$preserved_chunks <- stats$preserved_chunks + 1L
    }
  }

  if (format == "learnr" && !has_setup) {
    output <- c(build_default_setup_chunk(language = language, seed = seed), "", output)
    stats$setup_chunks <- stats$setup_chunks + 1L
  }

  output <- trim_trailing_blank_lines(output)

  list(lines = output, stats = stats)
}

#' Build YAML header lines for target format
#' @keywords internal
build_output_yaml <- function(source_yaml, format, language = "en") {
  yaml <- source_yaml
  if (is.null(yaml) || !is.list(yaml)) {
    yaml <- list()
  }

  if (is.null(yaml$title) || !nzchar(as.character(yaml$title))) {
    yaml$title <- if (language == "fr") "Tutoriel interactif" else "Interactive tutorial"
  }

  if (format == "learnr") {
    yaml$format <- NULL
    yaml$engine <- NULL
    yaml$output <- "learnr::tutorial"
    yaml$runtime <- "shiny_prerendered"
  } else {
    yaml$output <- NULL
    yaml$runtime <- NULL
    yaml$format <- "live-html"
    yaml$engine <- "knitr"
  }

  yaml_text <- yaml::as.yaml(yaml, indent.mapping.sequence = TRUE)
  yaml_lines <- strsplit(yaml_text, "\n", fixed = TRUE)[[1]]
  yaml_lines <- yaml_lines[nzchar(yaml_lines)]

  c("---", yaml_lines, "---")
}

#' Convert a parsed MCQ block to output lines
#' @keywords internal
transform_mcq_block <- function(block, format, index) {
  definition <- parse_mcq_yaml(block)
  validate_mcq_definition(definition, index = index)

  question <- definition$question
  answers <- definition$answers %||% definition$choices

  if (format == "learnr") {
    label <- sprintf("trz-mcq-explicit-%03d", index)
    lines <- c(
      sprintf("```{r %s, echo=FALSE}", label),
      "learnr::question(",
      sprintf("  %s,", deparse1(question))
    )

    answer_lines <- vapply(
      answers,
      FUN.VALUE = character(1),
      FUN = function(answer) {
        answer_text <- answer$text %||% ""
        is_correct <- isTRUE(answer$correct)
        sprintf(
          "  learnr::answer(%s, correct = %s),",
          deparse1(answer_text),
          if (is_correct) "TRUE" else "FALSE"
        )
      }
    )

    lines <- c(lines, answer_lines)

    if (!is.null(definition$allow_retry)) {
      lines <- c(lines, sprintf("  allow_retry = %s", if (isTRUE(definition$allow_retry)) "TRUE" else "FALSE"))
    } else {
      lines <- c(lines, "  allow_retry = TRUE")
    }

    lines <- c(lines, ")", "```")
    return(lines)
  }

  c(
    "::: {.callout-note}",
    "## Quiz",
    question,
    "",
    sprintf("- [ ] %s", answers[[1]]$text %||% ""),
    if (length(answers) >= 2L) sprintf("- [ ] %s", answers[[2]]$text %||% "") else "- [ ]",
    if (length(answers) >= 3L) sprintf("- [ ] %s", answers[[3]]$text %||% "") else NULL,
    ":::"
  )
}

#' Parse YAML from a tutorizeR-mcq block
#' @keywords internal
parse_mcq_yaml <- function(block) {
  if (length(block$body_lines) == 0L) {
    rlang::abort(
      "MCQ block is empty. Provide YAML content.",
      class = c("tutorizeR_error_validation", "tutorizeR_error")
    )
  }

  definition <- tryCatch(
    yaml::yaml.load(paste(block$body_lines, collapse = "\n"), eval.expr = FALSE),
    error = function(e) {
      rlang::abort(
        message = paste0("Invalid MCQ YAML: ", conditionMessage(e)),
        class = c("tutorizeR_error_validation", "tutorizeR_error")
      )
    }
  )

  if (is.null(definition)) {
    definition <- list()
  }

  definition
}

#' Validate expected MCQ fields
#' @keywords internal
validate_mcq_definition <- function(definition, index) {
  question <- definition$question
  answers <- definition$answers %||% definition$choices

  if (is.null(question) || !nzchar(trimws(as.character(question)))) {
    rlang::abort(
      sprintf("MCQ block #%d is missing a non-empty 'question' field.", index),
      class = c("tutorizeR_error_validation", "tutorizeR_error")
    )
  }

  if (is.null(answers) || length(answers) < 2L) {
    rlang::abort(
      sprintf("MCQ block #%d must contain at least two answers.", index),
      class = c("tutorizeR_error_validation", "tutorizeR_error")
    )
  }

  answer_ok <- vapply(answers, function(ans) {
    is.list(ans) && !is.null(ans$text) && nzchar(trimws(as.character(ans$text)))
  }, logical(1))

  if (!all(answer_ok)) {
    rlang::abort(
      sprintf("MCQ block #%d has one or more answers without text.", index),
      class = c("tutorizeR_error_validation", "tutorizeR_error")
    )
  }

  has_correct <- any(vapply(answers, function(ans) isTRUE(ans$correct), logical(1)))
  if (!has_correct) {
    rlang::abort(
      sprintf("MCQ block #%d must mark at least one answer with correct: true.", index),
      class = c("tutorizeR_error_validation", "tutorizeR_error")
    )
  }

  invisible(TRUE)
}

#' Decide effective conversion mode for one source chunk
#' @keywords internal
derive_assessment_mode <- function(assessment, tags) {
  mode <- list(
    exercise = assessment %in% c("code", "both"),
    mcq = assessment %in% c("mcq", "both"),
    solution_only = FALSE
  )

  if (isTRUE(tags$exercise_only)) {
    mode$exercise <- TRUE
    mode$mcq <- FALSE
  }

  if (isTRUE(tags$solution_only)) {
    mode$exercise <- FALSE
    mode$mcq <- FALSE
    mode$solution_only <- TRUE
  }

  if (isTRUE(tags$mcq)) {
    mode$mcq <- TRUE
  }

  mode
}

#' Build exercise lines from source R chunk
#' @keywords internal
build_exercise_lines <- function(format, exercise_label, source_chunk, language, tags) {
  hint_lines <- character()
  if (length(tags$hints) > 0L) {
    hint_lines <- c(
      if (language == "fr") "**Indice(s)**" else "**Hint(s)**",
      sprintf("- %s", tags$hints),
      ""
    )
  }

  lock_line <- character()
  if (isTRUE(tags$locked)) {
    lock_line <- if (language == "fr") {
      "_Exercice verrouille: activez ce bloc seulement pour demonstration._"
    } else {
      "_Locked exercise: enable this block only for demonstration._"
    }
  }

  source_options <- split_option_tokens(source_chunk$options_raw %||% "")
  source_options <- drop_options(source_options, c("include", "echo", "eval", "label", "engine"))

  if (format == "learnr") {
    options <- c(
      sprintf("exercise.lines=%d", max(length(strip_tag_lines(source_chunk$body_lines)), 3L)),
      "exercise=TRUE",
      source_options
    )

    header <- build_r_chunk_header(label = exercise_label, options = options)
    body <- c(
      if (language == "fr") "# Ecrivez votre code ici" else "# Write your code below"
    )

    return(c(hint_lines, lock_line, header, body, "```"))
  }

  c(
    hint_lines,
    lock_line,
    "```{webr}",
    if (language == "fr") "# Ecrivez votre code ici" else "# Write your code below",
    "```"
  )
}

#' Build solution lines from source R chunk
#' @keywords internal
build_solution_lines <- function(format, solution_label, source_chunk, language) {
  code <- strip_tag_lines(source_chunk$body_lines)
  if (length(code) == 0L) {
    code <- if (language == "fr") "# Solution a completer" else "# TODO: complete solution"
  }

  solution_options <- split_option_tokens(source_chunk$options_raw %||% "")
  solution_options <- drop_options(solution_options, c("label", "engine", "include"))
  solution_options <- unique(c("include=FALSE", solution_options))

  if (format == "learnr") {
    header <- build_r_chunk_header(label = solution_label, options = solution_options)
    return(c(header, code, "```"))
  }

  c(
    "::: {.callout-tip collapse='true'}",
    if (language == "fr") "## Solution" else "## Solution",
    "```r",
    code,
    "```",
    ":::"
  )
}

#' Build placeholder MCQ for chunk-level MCQ generation
#' @keywords internal
build_placeholder_mcq <- function(format, mcq_label, source_chunk, language) {
  label_txt <- source_chunk$label %||% "chunk"
  question <- if (language == "fr") {
    sprintf("Que fait le code du chunk '%s'?", label_txt)
  } else {
    sprintf("What does the code in chunk '%s' do?", label_txt)
  }

  if (format == "learnr") {
    return(c(
      sprintf("```{r %s, echo=FALSE}", mcq_label),
      "learnr::question(",
      sprintf("  %s,", deparse1(question)),
      sprintf("  learnr::answer(%s, correct = TRUE),", deparse1(if (language == "fr") "Reponse A (a adapter)" else "Answer A (edit me)")),
      sprintf("  learnr::answer(%s, correct = FALSE),", deparse1(if (language == "fr") "Reponse B (a adapter)" else "Answer B (edit me)")),
      sprintf("  learnr::answer(%s, correct = FALSE),", deparse1(if (language == "fr") "Reponse C (a adapter)" else "Answer C (edit me)")),
      "  allow_retry = TRUE",
      ")",
      "```"
    ))
  }

  c(
    "::: {.callout-note}",
    "## Quiz",
    question,
    if (language == "fr") "- [ ] Reponse A (a adapter)" else "- [ ] Answer A (edit me)",
    if (language == "fr") "- [ ] Reponse B (a adapter)" else "- [ ] Answer B (edit me)",
    if (language == "fr") "- [ ] Reponse C (a adapter)" else "- [ ] Answer C (edit me)",
    ":::"
  )
}

#' Transform existing setup chunk and inject required lines for learnr
#' @keywords internal
transform_setup_chunk <- function(block, format, language, seed, registry) {
  label <- block$label %||% "setup"
  label <- make_unique_label(label, registry)

  options <- split_option_tokens(block$options_raw %||% "")

  if (format == "learnr") {
    if (!has_option(options, "include")) {
      options <- c(options, "include=FALSE")
    }

    body <- block$body_lines
    required <- c("library(learnr)", "library(gradethis)", "gradethis_setup()")

    for (line in required) {
      if (!any(trimws(body) == line)) {
        body <- c(body, line)
      }
    }

    if (!is.null(seed)) {
      seed_line <- sprintf("set.seed(%s)", as.integer(seed))
      if (!any(trimws(body) == seed_line)) {
        body <- c(body, seed_line)
      }
    }

    header <- build_r_chunk_header(label = label, options = options)
    return(c(header, body, "```"))
  }

  header <- build_engine_chunk_header(
    engine = block$engine %||% "r",
    label = label,
    options = options
  )
  c(header, block$body_lines, "```")
}

#' Build default setup chunk when none is present
#' @keywords internal
build_default_setup_chunk <- function(language, seed = NULL) {
  title <- if (language == "fr") {
    "# Chunk setup ajoute par tutorizeR"
  } else {
    "# Setup chunk added by tutorizeR"
  }

  seed_line <- if (is.null(seed)) {
    character()
  } else {
    sprintf("set.seed(%s)", as.integer(seed))
  }

  c(
    "```{r setup, include=FALSE}",
    title,
    "library(learnr)",
    "library(gradethis)",
    "gradethis_setup()",
    seed_line,
    "```"
  )
}

#' Build knitr R chunk header
#' @keywords internal
build_r_chunk_header <- function(label = NULL, options = character()) {
  build_engine_chunk_header(engine = "r", label = label, options = options)
}

#' Build generic engine chunk header
#' @keywords internal
build_engine_chunk_header <- function(engine, label = NULL, options = character()) {
  inside <- engine
  if (!is.null(label) && nzchar(label)) {
    inside <- paste(inside, label)
  }

  options <- options[nzchar(trimws(options))]
  if (length(options) > 0L) {
    inside <- paste0(inside, ", ", paste(options, collapse = ", "))
  }

  sprintf("```{%s}", inside)
}

#' Strip teacher tag comments from chunk body
#' @keywords internal
strip_tag_lines <- function(lines) {
  if (length(lines) == 0L) {
    return(lines)
  }

  lines[!grepl("^\\s*#\\s*tutorizeR\\s*:", lines, ignore.case = TRUE)]
}

#' Drop option tokens by key name
#' @keywords internal
drop_options <- function(option_tokens, keys) {
  if (length(option_tokens) == 0L) {
    return(option_tokens)
  }

  keep <- vapply(option_tokens, function(token) {
    key <- tolower(trimws(sub("=.*$", "", token)))
    !(key %in% tolower(keys))
  }, logical(1))

  option_tokens[keep]
}

#' Check whether a chunk option already exists
#' @keywords internal
has_option <- function(option_tokens, key) {
  if (length(option_tokens) == 0L) {
    return(FALSE)
  }

  key <- tolower(key)
  any(vapply(option_tokens, function(token) {
    token_key <- tolower(trimws(sub("=.*$", "", token)))
    identical(token_key, key)
  }, logical(1)))
}

#' Determine if chunk is R engine
#' @keywords internal
is_r_chunk <- function(block) {
  tolower(block$engine %||% "") %in% c("r", "{r}")
}

#' Determine if chunk is setup chunk
#' @keywords internal
is_setup_chunk <- function(block) {
  label <- tolower(block$label %||% "")
  grepl("setup", label, fixed = TRUE)
}

#' Determine if block is explicit MCQ definition block
#' @keywords internal
is_mcq_block <- function(block) {
  engine <- tolower(block$engine %||% "")
  engine %in% c("tutorizer-mcq")
}

#' Register and generate unique deterministic labels
#' @keywords internal
make_unique_label <- function(base, registry) {
  base <- sanitize_label(base)
  candidate <- base
  k <- 1L

  while (exists(candidate, envir = registry, inherits = FALSE)) {
    k <- k + 1L
    candidate <- sprintf("%s-%d", base, k)
  }

  assign(candidate, TRUE, envir = registry)
  candidate
}

#' Register external label without modification
#' @keywords internal
register_label <- function(label, registry) {
  if (is.null(label) || !nzchar(label)) {
    return(invisible(NULL))
  }
  assign(label, TRUE, envir = registry)
  invisible(NULL)
}

#' Sanitize chunk labels to portable deterministic strings
#' @keywords internal
sanitize_label <- function(x) {
  x <- tolower(x %||% "chunk")
  x <- gsub("[^a-z0-9_-]+", "-", x)
  x <- gsub("-+", "-", x)
  x <- gsub("(^-|-$)", "", x)

  if (!nzchar(x)) {
    x <- "chunk"
  }

  x
}

#' Trim trailing blank lines from final output
#' @keywords internal
trim_trailing_blank_lines <- function(lines) {
  if (length(lines) == 0L) {
    return(lines)
  }

  idx <- length(lines)
  while (idx > 1L && !nzchar(lines[idx])) {
    idx <- idx - 1L
  }

  lines[seq_len(idx)]
}
