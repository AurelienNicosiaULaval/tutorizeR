#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

print_usage <- function() {
  cat(
"tutorizeR CLI\n\n",
"Usage:\n",
"  Rscript inst/scripts/tutorizeR-cli.R --input=FILE [options]\n",
"  Rscript inst/scripts/tutorizeR-cli.R --dir=FOLDER [options]\n\n",
"Options:\n",
"  --input=FILE            Source .Rmd/.qmd file\n",
"  --dir=FOLDER            Source folder for batch conversion\n",
"  --output-dir=DIR        Destination directory\n",
"  --format=learnr|quarto-live\n",
"  --assessment=code|mcq|both\n",
"  --question-bank=PATH    Question bank file or directory\n",
"  --mcq-source=inline|bank|mixed\n",
"  --lint-strict=true|false\n",
"  --pattern=REGEX         File pattern for folder mode\n",
"  --recursive=true|false  Recursive scan for folder mode\n",
"  --overwrite=true|false\n",
"  --language=en|fr\n",
"  --seed=INTEGER\n",
"  --verbose=true|false\n",
"  --help\n",
sep = ""
  )
}

parse_args <- function(x) {
  out <- list()
  for (a in x) {
    if (!startsWith(a, "--")) {
      next
    }

    kv <- strsplit(sub("^--", "", a), "=", fixed = TRUE)[[1]]
    key <- kv[1]
    val <- if (length(kv) > 1L) paste(kv[-1], collapse = "=") else "true"
    out[[key]] <- val
  }
  out
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

to_bool <- function(x, default = FALSE) {
  if (is.null(x)) {
    return(default)
  }
  tolower(x) %in% c("true", "1", "yes", "y")
}

opts <- parse_args(args)

if (isTRUE(to_bool(opts$help))) {
  print_usage()
  quit(status = 0)
}

if (!requireNamespace("tutorizeR", quietly = TRUE)) {
  stop("Package 'tutorizeR' must be installed before using the CLI.", call. = FALSE)
}

format <- opts$format %||% "learnr"
assessment <- opts$assessment %||% "both"
output_dir <- opts[["output-dir"]]
pattern <- opts$pattern %||% "\\.(Rmd|qmd)$"
language <- opts$language %||% "en"
question_bank <- opts[["question-bank"]]
mcq_source <- opts[["mcq-source"]] %||% "inline"
lint_strict <- to_bool(opts[["lint-strict"]], default = FALSE)
seed <- if (!is.null(opts$seed)) as.integer(opts$seed) else NULL
overwrite <- to_bool(opts$overwrite, default = FALSE)
recursive <- to_bool(opts$recursive, default = FALSE)
verbose <- to_bool(opts$verbose, default = TRUE)

if (!is.null(opts$input)) {
  rep <- tutorizeR::tutorize(
    input = opts$input,
    output_dir = output_dir,
    format = format,
    assessment = assessment,
    overwrite = overwrite,
    language = language,
    seed = seed,
    verbose = verbose,
    question_bank = question_bank,
    mcq_source = mcq_source,
    lint_strict = lint_strict
  )
  print(rep)
  quit(status = 0)
}

if (!is.null(opts$dir)) {
  rep <- tutorizeR::convert_folder(
    dir = opts$dir,
    pattern = pattern,
    recursive = recursive,
    output_dir = output_dir,
    format = format,
    assessment = assessment,
    overwrite = overwrite,
    language = language,
    seed = seed,
    verbose = verbose,
    question_bank = question_bank,
    mcq_source = mcq_source,
    lint_strict = lint_strict
  )
  print(rep)
  quit(status = 0)
}

print_usage()
quit(status = 1)
