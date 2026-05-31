local_edition(3)

test_that("learnr conversion injects gradethis setup and exercise scaffolds", {
  input <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "---",
    "title: feedback test",
    "---",
    "",
    "```{r setup}",
    "x <- 1",
    "```",
    "",
    "```{r add-one}",
    "# tutorizeR: hints=Use x + 1",
    "x + 1",
    "```"
  ), input)

  report <- tutorize(
    input = input,
    output_dir = tempdir(check = TRUE),
    format = "learnr",
    assessment = "code",
    overwrite = TRUE,
    verbose = FALSE
  )

  lines <- readLines(report$output_file, warn = FALSE)
  expect_true(any(grepl("library\\(learnr\\)", lines)))
  expect_true(any(grepl("requireNamespace\\(\"gradethis\"", lines)))
  expect_true(any(grepl("library\\(gradethis\\)", lines)))
  expect_true(any(grepl("gradethis_setup\\(\\)", lines)))
  expect_true(any(grepl("exercise=TRUE", lines, fixed = TRUE)))
  expect_true(any(grepl("trz-sol-add-one", lines, fixed = TRUE)))
  expect_true(any(grepl("Use x \\+ 1", lines)))
})

test_that("quarto-live conversion creates webr exercises and solution callouts", {
  input <- tempfile(fileext = ".qmd")
  writeLines(c(
    "---",
    "title: quarto live test",
    "---",
    "",
    "```{r summarize}",
    "mean(1:5)",
    "```"
  ), input)

  report <- tutorize(
    input = input,
    output_dir = tempdir(check = TRUE),
    format = "quarto-live",
    assessment = "both",
    overwrite = TRUE,
    verbose = FALSE
  )

  lines <- readLines(report$output_file, warn = FALSE)
  expect_true(any(grepl("format: live-html", lines, fixed = TRUE)))
  expect_true(any(grepl("_extensions/r-wasm/live/_knitr.qmd", lines, fixed = TRUE)))
  expect_true(any(grepl("```\\{webr\\}", lines)))
  expect_true(any(grepl("::: \\{\\.callout-tip collapse='true'\\}", lines)))
  expect_true(any(grepl("::: \\{\\.callout-note\\}", lines)))
})

test_that("MCQ references require an explicit question bank", {
  input <- tempfile(fileext = ".qmd")
  writeLines(c(
    "---",
    "title: bank test",
    "---",
    "",
    "```{tutorizeR-mcq-ref}",
    "ids: [missing-id]",
    "strategy: ordered",
    "```"
  ), input)

  expect_error(
    tutorize(
      input = input,
      output_dir = tempdir(check = TRUE),
      format = "learnr",
      assessment = "both",
      overwrite = TRUE,
      verbose = FALSE
    ),
    class = "tutorizeR_error_validation"
  )
})

test_that("example course module is packaged with expected teaching artifacts", {
  source_example_dir <- testthat::test_path(
    "..", "..", "inst", "examples", "example_course_module"
  )
  installed_example_dir <- system.file(
    "examples", "example_course_module", package = "tutorizeR"
  )

  example_dir <- if (dir.exists(source_example_dir)) {
    source_example_dir
  } else {
    installed_example_dir
  }

  expect_true(dir.exists(example_dir))
  expect_true(file.exists(file.path(example_dir, "lesson-source.qmd")))
  expect_true(file.exists(file.path(example_dir, "question-bank", "questions.yml")))
  expect_true(file.exists(file.path(example_dir, "expected", "lesson-source-tutorial.Rmd")))
  expect_true(file.exists(file.path(example_dir, "expected", "lesson-source-live.qmd")))
  expect_true(file.exists(file.path(example_dir, "expected", "conversion-report.json")))
  expect_true(file.exists(file.path(example_dir, "run-example.R")))
  expect_true(file.exists(file.path(example_dir, "student_activity.csv")))
})
