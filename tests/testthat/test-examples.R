local_edition(3)

example_module_dir <- function() {
  source_dir <- testthat::test_path("..", "..", "inst", "examples", "example_course_module")
  installed_dir <- system.file("examples", "example_course_module", package = "tutorizeR")

  if (dir.exists(source_dir)) {
    return(source_dir)
  }

  installed_dir
}

copy_example_to_temp <- function(example_dir) {
  work_dir <- file.path(tempdir(check = TRUE), paste0("tutorizeR-example-", as.integer(runif(1, 1, 1e8))))
  dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)

  file.copy(file.path(example_dir, "lesson-source.qmd"), work_dir, overwrite = TRUE)
  file.copy(file.path(example_dir, "student_activity.csv"), work_dir, overwrite = TRUE)

  work_dir
}

cleanup_example_packages <- function() {
  for (pkg in c("package:readr", "package:ggplot2", "package:dplyr")) {
    if (pkg %in% search()) {
      detach(pkg, character.only = TRUE, unload = FALSE)
    }
  }
}

declared_description_packages <- function() {
  desc_file <- testthat::test_path("..", "..", "DESCRIPTION")
  desc <- if (file.exists(desc_file)) {
    read.dcf(desc_file)[1, ]
  } else {
    utils::packageDescription("tutorizeR")
  }
  fields <- c("Depends", "Imports", "Suggests", "Enhances")
  raw <- vapply(fields, function(field) {
    value <- if (field %in% names(desc)) desc[[field]] else NULL
    if (is.null(value)) "" else value
  }, character(1))
  raw <- paste(raw, collapse = ",")
  packages <- trimws(unlist(strsplit(raw, ",", fixed = TRUE), use.names = FALSE))
  packages <- sub("\\s*\\(.*\\)$", "", packages)
  unique(packages[nzchar(packages)])
}

extract_example_packages <- function(files) {
  lines <- unlist(lapply(files, readLines, warn = FALSE), use.names = FALSE)
  library_matches <- regmatches(lines, gregexpr(
    "(library|require)\\([[:space:]]*[A-Za-z][A-Za-z0-9.]*",
    lines,
    perl = TRUE
  ))
  namespace_matches <- regmatches(lines, gregexpr(
    "\\b[A-Za-z][A-Za-z0-9.]*::",
    lines,
    perl = TRUE
  ))

  packages <- c(
    sub(".*\\([[:space:]]*", "", unlist(library_matches, use.names = FALSE)),
    sub("::$", "", unlist(namespace_matches, use.names = FALSE))
  )

  unique(packages[nzchar(packages)])
}

test_that("example course module has required installable structure", {
  example_dir <- example_module_dir()

  expect_true(dir.exists(example_dir))
  expect_true(file.exists(file.path(example_dir, "README.md")))
  expect_true(file.exists(file.path(example_dir, "lesson-source.qmd")))
  expect_true(file.exists(file.path(example_dir, "question-bank", "questions.yml")))
  expect_true(file.exists(file.path(example_dir, "expected", "lesson-source-tutorial.Rmd")))
  expect_true(file.exists(file.path(example_dir, "expected", "lesson-source-live.qmd")))
  expect_true(file.exists(file.path(example_dir, "expected", "conversion-report.json")))
  expect_true(file.exists(file.path(example_dir, "run-example.R")))
})

test_that("example question bank loads and validates", {
  example_dir <- example_module_dir()

  bank <- load_question_bank(file.path(example_dir, "question-bank"))
  report <- validate_question_bank(bank, strict = FALSE)

  expect_s3_class(bank, "tutorize_question_bank")
  expect_gte(length(bank$questions), 4L)
  expect_false(any(report$findings$severity == "error"))
})

test_that("example module dependencies are declared", {
  example_dir <- example_module_dir()
  example_files <- c(
    file.path(example_dir, "lesson-source.qmd"),
    file.path(example_dir, "run-example.R"),
    list.files(
      file.path(example_dir, "expected"),
      pattern = "\\.(Rmd|qmd)$",
      full.names = TRUE
    )
  )

  used_packages <- setdiff(extract_example_packages(example_files), "tutorizeR")
  declared_packages <- declared_description_packages()

  expect_setequal(
    intersect(used_packages, c("dplyr", "ggplot2", "readr", "learnr", "gradethis")),
    c("dplyr", "ggplot2", "readr", "learnr", "gradethis")
  )
  expect_true(all(used_packages %in% declared_packages))
})

test_that("example module does not expose placeholder MCQs", {
  example_dir <- example_module_dir()
  jose_files <- c(
    file.path(example_dir, "lesson-source.qmd"),
    file.path(example_dir, "question-bank", "questions.yml"),
    list.files(file.path(example_dir, "expected"), full.names = TRUE)
  )
  text <- unlist(lapply(jose_files, readLines, warn = FALSE), use.names = FALSE)

  expect_false(any(grepl("Answer A \\(edit me\\)", text)))
  expect_false(any(grepl("\\bedit me\\b", text, ignore.case = TRUE)))
})

test_that("example module converts to learnr and writes JSON report", {
  on.exit(cleanup_example_packages(), add = TRUE)

  example_dir <- example_module_dir()
  work_dir <- copy_example_to_temp(example_dir)
  bank <- load_question_bank(file.path(example_dir, "question-bank"))

  report <- tutorize(
    input = file.path(work_dir, "lesson-source.qmd"),
    output_dir = work_dir,
    format = "learnr",
    assessment = "both",
    question_bank = bank,
    mcq_source = "mixed",
    overwrite = TRUE,
    verbose = FALSE
  )

  report_file <- file.path(work_dir, "conversion-report.json")
  write_tutorize_report(report, report_file, format = "json")

  lines <- readLines(report$output_file, warn = FALSE)
  report_json <- jsonlite::fromJSON(report_file)

  expect_true(file.exists(report$output_file))
  expect_true(file.exists(report_file))
  expect_true(any(grepl("learnr::tutorial", lines, fixed = TRUE)))
  expect_true(any(grepl("gradethis_setup()", lines, fixed = TRUE)))
  expect_true(any(grepl("trz-mcq-bank", lines, fixed = TRUE)))
  expect_false(any(grepl("Answer A \\(edit me\\)", lines)))
  expect_equal(report_json$format, "learnr")
})

test_that("example module converts to quarto-live", {
  on.exit(cleanup_example_packages(), add = TRUE)

  example_dir <- example_module_dir()
  work_dir <- copy_example_to_temp(example_dir)
  bank <- load_question_bank(file.path(example_dir, "question-bank"))

  report <- tutorize(
    input = file.path(work_dir, "lesson-source.qmd"),
    output_dir = work_dir,
    format = "quarto-live",
    assessment = "both",
    question_bank = bank,
    mcq_source = "mixed",
    overwrite = TRUE,
    verbose = FALSE
  )

  lines <- readLines(report$output_file, warn = FALSE)

  expect_true(file.exists(report$output_file))
  expect_true(any(grepl("format: live-html", lines, fixed = TRUE)))
  expect_true(any(grepl("```\\{webr\\}", lines)))
  expect_true(any(grepl("::: \\{\\.callout-note\\}", lines)))
  expect_false(any(grepl("Answer A \\(edit me\\)", lines)))
})

test_that("run-example.R works from installed-style example path", {
  on.exit(cleanup_example_packages(), add = TRUE)

  example_dir <- example_module_dir()
  env <- new.env(parent = globalenv())

  result <- source(file.path(example_dir, "run-example.R"), local = env)

  expect_true(dir.exists(result$value$work_dir))
  expect_true(file.exists(result$value$learnr_report$output_file))
  expect_true(file.exists(result$value$live_report$output_file))
  expect_true(file.exists(result$value$conversion_report))
})

test_that("README user smoke tests do not use internal test fixtures", {
  readme_file <- testthat::test_path("..", "..", "README.md")
  skip_if_not(file.exists(readme_file), "README.md is not available in installed-package tests.")

  readme <- readLines(readme_file, warn = FALSE)

  expect_false(any(grepl("tests/testthat/fixtures", readme, fixed = TRUE)))
})
