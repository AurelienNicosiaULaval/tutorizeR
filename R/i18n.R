# Internationalization ----------------------------------------------------

.i18n_cache <- new.env(parent = emptyenv())

#' Translate a tutorizeR message key
#'
#' Looks up a message key in language dictionaries and interpolates named
#' placeholders (for example, \code{"{name}"}).
#'
#' @param key Dot-separated translation key (e.g. `"messages.render_ok"`).
#' @param ... Named interpolation values.
#' @param language Optional language (`"en"` or `"fr"`).
#'
#' @return A translated character string.
#' @export
tr <- function(key, ..., language = NULL) {
  lang <- resolve_language(language)

  value <- lookup_i18n_key(get_i18n_dictionary(lang), key)
  if (is.null(value)) {
    value <- lookup_i18n_key(get_i18n_dictionary("en"), key)
  }

  if (is.null(value)) {
    value <- key
  }

  args <- list(...)
  if (length(args) == 0L) {
    return(as.character(value))
  }

  out <- as.character(value)
  nms <- names(args)
  for (i in seq_along(args)) {
    nm <- nms[i]
    if (is.null(nm) || !nzchar(nm)) {
      next
    }
    out <- gsub(
      pattern = paste0("\\{", nm, "\\}"),
      replacement = as.character(args[[i]]),
      x = out,
      fixed = FALSE
    )
  }

  out
}

#' Resolve effective language for tutorizeR
#' @keywords internal
resolve_language <- function(language = NULL) {
  if (is.null(language) || !nzchar(language)) {
    language <- getOption("tutorizeR.language", "en")
  }

  language <- tolower(as.character(language)[1])
  if (!(language %in% c("en", "fr"))) {
    language <- "en"
  }

  language
}

#' Load i18n dictionary from YAML with cache
#' @keywords internal
get_i18n_dictionary <- function(language) {
  language <- resolve_language(language)

  if (exists(language, envir = .i18n_cache, inherits = FALSE)) {
    return(get(language, envir = .i18n_cache, inherits = FALSE))
  }

  path <- find_i18n_file(language)

  dict <- if (nzchar(path)) {
    yaml::yaml.load_file(path, eval.expr = FALSE)
  } else {
    list()
  }

  if (is.null(dict)) {
    dict <- list()
  }

  assign(language, dict, envir = .i18n_cache)
  dict
}

#' Locate an i18n YAML file for the given language
#' @keywords internal
find_i18n_file <- function(language) {
  path <- system.file("i18n", paste0(language, ".yml"), package = "tutorizeR")
  if (nzchar(path) && file.exists(path)) {
    return(path)
  }

  root_opt <- getOption("tutorizeR.root", "")
  if (nzchar(root_opt)) {
    candidate <- file.path(root_opt, "inst", "i18n", paste0(language, ".yml"))
    if (file.exists(candidate)) {
      return(candidate)
    }
  }

  # Walk up from current working directory to find package source root.
  current <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  seen <- character()
  while (nzchar(current) && !(current %in% seen)) {
    seen <- c(seen, current)
    candidate <- file.path(current, "inst", "i18n", paste0(language, ".yml"))
    if (file.exists(candidate)) {
      return(candidate)
    }

    parent <- dirname(current)
    if (identical(parent, current)) {
      break
    }
    current <- parent
  }

  ""
}

#' Lookup nested translation key in dictionary
#' @keywords internal
lookup_i18n_key <- function(dict, key) {
  if (is.null(dict) || !is.list(dict) || !nzchar(key)) {
    return(NULL)
  }

  parts <- strsplit(key, "\\.", perl = TRUE)[[1]]
  node <- dict

  for (p in parts) {
    if (!is.list(node) || is.null(node[[p]])) {
      return(NULL)
    }
    node <- node[[p]]
  }

  node
}
