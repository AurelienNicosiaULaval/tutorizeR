# Internal parser helpers -------------------------------------------------

#' Parse an input R Markdown / Quarto document into structured blocks
#' @keywords internal
parse_input_document <- function(input_file) {
  lines <- readLines(input_file, warn = FALSE)
  yaml_split <- split_front_matter(lines)
  validate_markdown_structure(yaml_split$body_lines)

  list(
    input_file = input_file,
    extension = tolower(tools::file_ext(input_file)),
    front_matter = yaml_split,
    blocks = parse_markdown_blocks(yaml_split$body_lines)
  )
}

#' Run a lightweight markdown parse validation when commonmark is available
#' @keywords internal
validate_markdown_structure <- function(lines) {
  if (!requireNamespace("commonmark", quietly = TRUE)) {
    return(invisible(NULL))
  }

  if (length(lines) == 0L) {
    return(invisible(NULL))
  }

  try(
    commonmark::markdown_xml(paste(lines, collapse = "\n")),
    silent = TRUE
  )

  invisible(NULL)
}

#' Split optional YAML front matter from body lines
#' @keywords internal
split_front_matter <- function(lines) {
  if (length(lines) == 0L) {
    return(list(yaml = list(), yaml_lines = character(), body_lines = lines))
  }

  first_non_empty <- which(trimws(lines) != "")[1]
  if (is.na(first_non_empty) || trimws(lines[first_non_empty]) != "---") {
    return(list(yaml = list(), yaml_lines = character(), body_lines = lines))
  }

  if (first_non_empty == length(lines)) {
    return(list(yaml = list(), yaml_lines = character(), body_lines = lines))
  }

  yaml_tail <- lines[(first_non_empty + 1L):length(lines)]
  end_rel <- which(trimws(yaml_tail) %in% c("---", "..."))[1]

  if (is.na(end_rel)) {
    return(list(yaml = list(), yaml_lines = character(), body_lines = lines))
  }

  end_idx <- first_non_empty + end_rel

  if (end_idx <= first_non_empty + 1L) {
    yaml_data <- list()
    yaml_lines <- character()
  } else {
    yaml_lines <- lines[(first_non_empty + 1L):(end_idx - 1L)]
    yaml_data <- tryCatch(
      yaml::yaml.load(paste(yaml_lines, collapse = "\n"), eval.expr = FALSE),
      error = function(e) list()
    )
    if (is.null(yaml_data)) {
      yaml_data <- list()
    }
  }

  body_idx <- seq.int(end_idx + 1L, length(lines))
  body_lines <- if (length(body_idx) == 0L) character() else lines[body_idx]

  list(
    yaml = yaml_data,
    yaml_lines = yaml_lines,
    body_lines = body_lines
  )
}

#' Parse markdown body into text/chunk blocks
#' @keywords internal
parse_markdown_blocks <- function(lines) {
  blocks <- list()
  i <- 1L
  n <- length(lines)

  while (i <= n) {
    line <- lines[i]

    inline_match <- regexec(
      "^([`~]{3,})\\s*\\{([^}]*)\\}\\s*(.*?)\\s*\\1\\s*$",
      line,
      perl = TRUE
    )
    inline_capture <- regmatches(line, inline_match)[[1]]

    if (length(inline_capture) > 0L) {
      spec <- inline_capture[3]
      body <- inline_capture[4]
      blocks[[length(blocks) + 1L]] <- build_chunk_block(
        spec = spec,
        body_lines = if (nzchar(body)) body else character(),
        fence = inline_capture[2],
        raw_lines = line,
        inline = TRUE
      )
      i <- i + 1L
      next
    }

    open_match <- regexec(
      "^([`~]{3,})\\s*\\{([^}]*)\\}\\s*$",
      line,
      perl = TRUE
    )
    open_capture <- regmatches(line, open_match)[[1]]

    if (length(open_capture) > 0L) {
      fence <- open_capture[2]
      spec <- open_capture[3]
      start_idx <- i
      i <- i + 1L
      body_lines <- character()
      closed <- FALSE

      while (i <= n) {
        candidate <- lines[i]
        if (trimws(candidate) == fence) {
          closed <- TRUE
          close_line <- candidate
          i <- i + 1L
          break
        }
        body_lines <- c(body_lines, candidate)
        i <- i + 1L
      }

      if (!closed) {
        # Fallback: keep malformed chunk as plain text from start line onward.
        blocks[[length(blocks) + 1L]] <- list(
          type = "text",
          lines = lines[start_idx:n]
        )
        break
      }

      raw_lines <- c(line, body_lines, close_line)
      blocks[[length(blocks) + 1L]] <- build_chunk_block(
        spec = spec,
        body_lines = body_lines,
        fence = fence,
        raw_lines = raw_lines,
        inline = FALSE
      )

      next
    }

    text_buf <- line
    i <- i + 1L
    while (i <= n) {
      look_ahead <- lines[i]
      is_chunk_start <- grepl("^([`~]{3,})\\s*\\{[^}]*\\}", look_ahead, perl = TRUE)
      if (is_chunk_start) {
        break
      }
      text_buf <- c(text_buf, look_ahead)
      i <- i + 1L
    }

    blocks[[length(blocks) + 1L]] <- list(
      type = "text",
      lines = text_buf
    )
  }

  blocks
}

#' Build normalized chunk block object
#' @keywords internal
build_chunk_block <- function(spec, body_lines, fence, raw_lines, inline = FALSE) {
  chunk_spec <- parse_chunk_spec(spec)

  list(
    type = "chunk",
    engine = chunk_spec$engine,
    label = chunk_spec$label,
    options_raw = chunk_spec$options_raw,
    spec_raw = spec,
    body_lines = body_lines,
    raw_lines = raw_lines,
    fence = fence,
    inline = inline,
    tags = parse_tutorizer_tags(body_lines)
  )
}

#' Parse knitr/quarto chunk specification
#' @keywords internal
parse_chunk_spec <- function(spec) {
  spec <- trimws(spec)

  if (!nzchar(spec)) {
    return(list(engine = "", label = NULL, options_raw = NULL))
  }

  first_comma <- regexpr(",", spec, fixed = TRUE)[1]
  if (first_comma > 0L) {
    head <- trimws(substr(spec, 1L, first_comma - 1L))
    options_raw <- trimws(substr(spec, first_comma + 1L, nchar(spec)))
    if (!nzchar(options_raw)) {
      options_raw <- NULL
    }
  } else {
    head <- spec
    options_raw <- NULL
  }

  tokens <- strsplit(head, "\\s+", perl = TRUE)[[1]]
  tokens <- tokens[nzchar(tokens)]

  engine <- if (length(tokens) >= 1L) tokens[1] else ""
  label <- if (length(tokens) >= 2L) tokens[2] else NULL

  list(engine = engine, label = label, options_raw = options_raw)
}

#' Parse teacher tags declared in code comments
#' @keywords internal
parse_tutorizer_tags <- function(lines) {
  tags <- list(
    skip = FALSE,
    exercise_only = FALSE,
    solution_only = FALSE,
    mcq = FALSE,
    narrative_only = FALSE,
    locked = FALSE,
    hints = character()
  )

  if (length(lines) == 0L) {
    return(tags)
  }

  tag_lines <- grep("^\\s*#\\s*tutorizeR\\s*:", lines, ignore.case = TRUE, value = TRUE)
  if (length(tag_lines) == 0L) {
    return(tags)
  }

  for (line in tag_lines) {
    payload <- sub("^\\s*#\\s*tutorizeR\\s*:\\s*", "", line, ignore.case = TRUE)
    fields <- split_option_tokens(payload)
    for (field in fields) {
      key <- tolower(trimws(field))

      if (grepl("^hints?\\s*=", key)) {
        hint_value <- trimws(sub("^hints?\\s*=", "", field, ignore.case = TRUE))
        hints <- strsplit(hint_value, "\\|", perl = TRUE)[[1]]
        hints <- trimws(hints)
        tags$hints <- c(tags$hints, hints[nzchar(hints)])
        next
      }

      if (key == "skip") tags$skip <- TRUE
      if (key == "exercise-only") tags$exercise_only <- TRUE
      if (key == "solution-only") tags$solution_only <- TRUE
      if (key == "mcq") tags$mcq <- TRUE
      if (key == "narrative-only") tags$narrative_only <- TRUE
      if (key == "locked") tags$locked <- TRUE
    }
  }

  tags
}

#' Split a comma-separated option string while preserving quoted commas
#' @keywords internal
split_option_tokens <- function(x) {
  x <- trimws(x)
  if (!nzchar(x)) {
    return(character())
  }

  chars <- strsplit(x, "", fixed = TRUE)[[1]]
  tokens <- character()
  current <- ""
  in_single <- FALSE
  in_double <- FALSE
  depth <- 0L

  for (ch in chars) {
    if (ch == "'" && !in_double) {
      in_single <- !in_single
      current <- paste0(current, ch)
      next
    }

    if (ch == '"' && !in_single) {
      in_double <- !in_double
      current <- paste0(current, ch)
      next
    }

    if (!in_single && !in_double && ch %in% c("(", "[", "{")) {
      depth <- depth + 1L
      current <- paste0(current, ch)
      next
    }

    if (!in_single && !in_double && ch %in% c(")", "]", "}")) {
      depth <- max(depth - 1L, 0L)
      current <- paste0(current, ch)
      next
    }

    if (!in_single && !in_double && depth == 0L && ch == ",") {
      token <- trimws(current)
      if (nzchar(token)) {
        tokens <- c(tokens, token)
      }
      current <- ""
      next
    }

    current <- paste0(current, ch)
  }

  token <- trimws(current)
  if (nzchar(token)) {
    tokens <- c(tokens, token)
  }

  tokens
}
