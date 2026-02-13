# Internal transformation helpers ----------------------------------------

#' Build converted tutorial lines from parsed document blocks
#' @keywords internal
build_tutorial_lines <- function(
  parsed,
  format,
  assessment,
  language = "en",
  seed = NULL,
  question_bank = NULL,
  mcq_source = c("inline", "bank", "mixed")
) {
  mcq_source <- match.arg(mcq_source)

  output <- character()
  registry <- new.env(parent = emptyenv())

  stats <- list(
    exercises = 0L,
    solutions = 0L,
    mcq = 0L,
    mcq_explicit = 0L,
    mcq_from_bank = 0L,
    skipped = 0L,
    setup_chunks = 0L,
    preserved_chunks = 0L,
    sections = 0L,
    chunks_total = 0L,
    estimated_minutes = 0
  )

  has_setup <- FALSE
  chunk_index <- 0L

  for (block in parsed$blocks) {
    if (identical(block$type, "text")) {
      output <- c(output, block$lines)
      stats$sections <- stats$sections + sum(grepl("^#{1,6}\\s+", block$lines))
      next
    }

    if (!identical(block$type, "chunk")) {
      next
    }

    stats$chunks_total <- stats$chunks_total + 1L
    chunk_index <- chunk_index + 1L

    if (is_mcq_ref_block(block)) {
      if (!(assessment %in% c("mcq", "both"))) {
        next
      }

      mcq_lines <- transform_mcq_ref_block(
        block = block,
        format = format,
        index = chunk_index,
        question_bank = question_bank,
        language = language
      )
      output <- c(output, mcq_lines, "")
      stats$mcq <- stats$mcq + count_generated_mcq(mcq_lines, format)
      stats$mcq_from_bank <- stats$mcq_from_bank + count_generated_mcq(mcq_lines, format)
      next
    }

    if (is_mcq_block(block)) {
      if (assessment %in% c("mcq", "both")) {
        mcq_lines <- transform_mcq_block(block, format = format, index = chunk_index)
        output <- c(output, mcq_lines, "")
        stats$mcq <- stats$mcq + 1L
        stats$mcq_explicit <- stats$mcq_explicit + 1L
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
        block = block,
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
      mcq_lines <- build_mcq_for_chunk_mode(
        source_chunk = block,
        format = format,
        mcq_label_base = paste0("trz-mcq-", label_base),
        registry = registry,
        language = language,
        question_bank = question_bank,
        mcq_source = mcq_source,
        index = chunk_index
      )
      output <- c(output, mcq_lines$lines, "")
      stats$mcq <- stats$mcq + mcq_lines$count
      if (isTRUE(mcq_lines$from_bank)) {
        stats$mcq_from_bank <- stats$mcq_from_bank + mcq_lines$count
      }
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

  stats$estimated_minutes <- estimate_completion_minutes(stats)
  output <- trim_trailing_blank_lines(output)

  list(lines = output, stats = stats)
}

#' Estimate expected completion time in minutes
#' @keywords internal
estimate_completion_minutes <- function(stats) {
  round(stats$exercises * 4 + stats$mcq * 1.5 + stats$sections * 0.5, 1)
}

#' Count generated MCQ blocks in already rendered lines
#' @keywords internal
count_generated_mcq <- function(lines, format) {
  if (format == "learnr") {
    return(sum(grepl("^```\\{r trz-mcq", lines)))
  }
  sum(grepl("^::: \\{\\.callout-note\\}$", lines))
}

#' Build YAML header lines for target format
#' @keywords internal
build_output_yaml <- function(source_yaml, format, language = "en") {
  yaml <- source_yaml
  if (is.null(yaml) || !is.list(yaml)) {
    yaml <- list()
  }

  if (is.null(yaml$title) || !nzchar(as.character(yaml$title))) {
    yaml$title <- tr("labels.interactive_tutorial", language = language)
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
    return(build_learnr_question_lines(
      question = question,
      answers = answers,
      label = sprintf("trz-mcq-explicit-%03d", index),
      allow_retry = definition$allow_retry %||% TRUE
    ))
  }

  build_callout_question_lines(question = question, answers = answers, language = "en")
}

#' Convert a parsed MCQ reference block to output lines
#' @keywords internal
transform_mcq_ref_block <- function(block, format, index, question_bank, language = "en") {
  if (is.null(question_bank)) {
    rlang::abort(
      tr("errors.qb_ref_requires_bank", language = language),
      class = c("tutorizeR_error_validation", "tutorizeR_error")
    )
  }

  definition <- parse_mcq_ref_yaml(block)
  validate_mcq_ref_definition(definition, language = language)

  selected <- select_questions_from_bank(
    bank = question_bank,
    ids = definition$ids,
    n = definition$n,
    strategy = definition$strategy,
    tags = definition$tags,
    difficulty = definition$difficulty,
    language = definition$language
  )

  if (length(selected) == 0L) {
    rlang::abort(
      tr("errors.qb_no_match", language = language),
      class = c("tutorizeR_error_validation", "tutorizeR_error")
    )
  }

  lines <- character()

  for (i in seq_along(selected)) {
    q <- selected[[i]]

    if (isTRUE(definition$shuffle_answers) && length(q$answers) > 1L) {
      q$answers <- sample(q$answers)
    }

    if (format == "learnr") {
      lines <- c(
        lines,
        build_learnr_question_lines(
          question = q$question,
          answers = q$answers,
          label = sprintf("trz-mcq-bank-%03d-%03d", index, i),
          allow_retry = TRUE
        )
      )
    } else {
      lines <- c(lines, build_callout_question_lines(
        question = q$question,
        answers = q$answers,
        language = language
      ))
    }

    lines <- c(lines, "")
  }

  trim_trailing_blank_lines(lines)
}

#' Build learnr::question chunk lines
#' @keywords internal
build_learnr_question_lines <- function(question, answers, label, allow_retry = TRUE) {
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
  lines <- c(lines, sprintf("  allow_retry = %s", if (isTRUE(allow_retry)) "TRUE" else "FALSE"))
  c(lines, ")", "```")
}

#' Build callout-style MCQ lines
#' @keywords internal
build_callout_question_lines <- function(question, answers, language = "en") {
  lines <- c(
    "::: {.callout-note}",
    paste0("## ", tr("labels.quiz", language = language)),
    question,
    ""
  )

  for (ans in answers) {
    lines <- c(lines, sprintf("- [ ] %s", ans$text %||% ""))
  }

  c(lines, ":::")
}

#' Parse YAML from a tutorizeR-mcq block
#' @keywords internal
parse_mcq_yaml <- function(block) {
  if (length(block$body_lines) == 0L) {
    rlang::abort(
      tr("errors.mcq_empty"),
      class = c("tutorizeR_error_validation", "tutorizeR_error")
    )
  }

  definition <- tryCatch(
    yaml::yaml.load(paste(block$body_lines, collapse = "\n"), eval.expr = FALSE),
    error = function(e) {
      rlang::abort(
        tr("errors.mcq_yaml", message = conditionMessage(e)),
        class = c("tutorizeR_error_validation", "tutorizeR_error")
      )
    }
  )

  if (is.null(definition)) {
    definition <- list()
  }

  definition
}

#' Parse YAML from a tutorizeR-mcq-ref block
#' @keywords internal
parse_mcq_ref_yaml <- function(block) {
  if (length(block$body_lines) == 0L) {
    return(list(ids = NULL, n = NULL, strategy = "ordered", shuffle_answers = FALSE))
  }

  definition <- tryCatch(
    yaml::yaml.load(paste(block$body_lines, collapse = "\n"), eval.expr = FALSE),
    error = function(e) {
      rlang::abort(
        tr("errors.mcq_yaml", message = conditionMessage(e)),
        class = c("tutorizeR_error_validation", "tutorizeR_error")
      )
    }
  )

  if (is.null(definition)) {
    definition <- list()
  }

  definition$ids <- as.character(definition$ids %||% character())
  definition$tags <- as.character(definition$tags %||% character())
  definition$difficulty <- as.character(definition$difficulty %||% "")
  definition$language <- as.character(definition$language %||% "")
  definition$n <- if (is.null(definition$n)) NULL else as.integer(definition$n)
  definition$strategy <- as.character(definition$strategy %||% "ordered")
  definition$shuffle_answers <- isTRUE(definition$shuffle_answers)
  definition
}

#' Validate expected MCQ fields
#' @keywords internal
validate_mcq_definition <- function(definition, index) {
  question <- definition$question
  answers <- definition$answers %||% definition$choices

  if (is.null(question) || !nzchar(trimws(as.character(question)))) {
    rlang::abort(
      tr("errors.mcq_question", index = index),
      class = c("tutorizeR_error_validation", "tutorizeR_error")
    )
  }

  if (is.null(answers) || length(answers) < 2L) {
    rlang::abort(
      tr("errors.mcq_answers_min", index = index),
      class = c("tutorizeR_error_validation", "tutorizeR_error")
    )
  }

  answer_ok <- vapply(answers, function(ans) {
    is.list(ans) && !is.null(ans$text) && nzchar(trimws(as.character(ans$text)))
  }, logical(1))

  if (!all(answer_ok)) {
    rlang::abort(
      tr("errors.mcq_answer_text", index = index),
      class = c("tutorizeR_error_validation", "tutorizeR_error")
    )
  }

  has_correct <- any(vapply(answers, function(ans) isTRUE(ans$correct), logical(1)))
  if (!has_correct) {
    rlang::abort(
      tr("errors.mcq_answer_correct", index = index),
      class = c("tutorizeR_error_validation", "tutorizeR_error")
    )
  }

  invisible(TRUE)
}

#' Validate expected MCQ ref fields
#' @keywords internal
validate_mcq_ref_definition <- function(definition, language = "en") {
  has_id <- length(definition$ids %||% character()) > 0L
  has_tag <- length(definition$tags %||% character()) > 0L
  has_n <- !is.null(definition$n) && is.finite(definition$n) && definition$n > 0

  if (!(has_id || has_tag || has_n)) {
    rlang::abort(
      tr("errors.qb_missing_ids", language = language),
      class = c("tutorizeR_error_validation", "tutorizeR_error")
    )
  }

  if (!(definition$strategy %in% c("ordered", "random"))) {
    definition$strategy <- "ordered"
  }

  invisible(TRUE)
}

#' Build MCQ lines according to selected mode
#' @keywords internal
build_mcq_for_chunk_mode <- function(
  source_chunk,
  format,
  mcq_label_base,
  registry,
  language,
  question_bank,
  mcq_source,
  index
) {
  if (mcq_source == "inline") {
    mcq_label <- make_unique_label(mcq_label_base, registry)
    return(list(
      lines = build_placeholder_mcq(format, mcq_label, source_chunk, language),
      from_bank = FALSE,
      count = 1L
    ))
  }

  if (is.null(question_bank)) {
    if (mcq_source == "bank") {
      rlang::abort(
        tr("errors.qb_ref_requires_bank", language = language),
        class = c("tutorizeR_error_validation", "tutorizeR_error")
      )
    }

    mcq_label <- make_unique_label(mcq_label_base, registry)
    return(list(
      lines = build_placeholder_mcq(format, mcq_label, source_chunk, language),
      from_bank = FALSE,
      count = 1L
    ))
  }

  selected <- select_questions_from_bank(
    bank = question_bank,
    n = 1,
    strategy = "random",
    tags = source_chunk$tags$hints,
    language = language
  )

  if (length(selected) == 0L) {
    if (mcq_source == "bank") {
      rlang::abort(
        tr("errors.qb_no_match", language = language),
        class = c("tutorizeR_error_validation", "tutorizeR_error")
      )
    }

    mcq_label <- make_unique_label(mcq_label_base, registry)
    return(list(
      lines = build_placeholder_mcq(format, mcq_label, source_chunk, language),
      from_bank = FALSE,
      count = 1L
    ))
  }

  q <- selected[[1]]

  if (format == "learnr") {
    lines <- build_learnr_question_lines(
      question = q$question,
      answers = q$answers,
      label = make_unique_label(mcq_label_base, registry),
      allow_retry = TRUE
    )
  } else {
    lines <- build_callout_question_lines(
      question = q$question,
      answers = q$answers,
      language = language
    )
  }

  list(lines = lines, from_bank = TRUE, count = 1L)
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
      tr("labels.hint_title", language = language),
      sprintf("- %s", tags$hints),
      ""
    )
  }

  lock_line <- character()
  if (isTRUE(tags$locked)) {
    lock_line <- tr("labels.locked_notice", language = language)
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
    body <- c(tr("labels.write_code", language = language))

    return(c(hint_lines, lock_line, header, body, "```"))
  }

  c(
    hint_lines,
    lock_line,
    "```{webr}",
    tr("labels.write_code", language = language),
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
    paste0("## ", tr("labels.solution", language = language)),
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

  answers <- list(
    list(text = if (language == "fr") "Reponse A (a adapter)" else "Answer A (edit me)", correct = TRUE),
    list(text = if (language == "fr") "Reponse B (a adapter)" else "Answer B (edit me)", correct = FALSE),
    list(text = if (language == "fr") "Reponse C (a adapter)" else "Answer C (edit me)", correct = FALSE)
  )

  if (format == "learnr") {
    return(build_learnr_question_lines(
      question = question,
      answers = answers,
      label = mcq_label,
      allow_retry = TRUE
    ))
  }

  build_callout_question_lines(question = question, answers = answers, language = language)
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
  seed_line <- if (is.null(seed)) character() else sprintf("set.seed(%s)", as.integer(seed))

  c(
    "```{r setup, include=FALSE}",
    tr("labels.setup_added", language = language),
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

#' Determine if block is MCQ reference block
#' @keywords internal
is_mcq_ref_block <- function(block) {
  engine <- tolower(block$engine %||% "")
  engine %in% c("tutorizer-mcq-ref")
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
