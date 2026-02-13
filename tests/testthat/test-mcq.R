local_edition(3)

fixture <- function(...) {
  testthat::test_path("fixtures", ...)
}

test_that("explicit tutorizeR-mcq block is converted in learnr output", {
  rep <- tutorize(
    input = fixture("qmd", "with_mcq_block.qmd"),
    output_dir = tempdir(check = TRUE),
    format = "learnr",
    assessment = "both",
    overwrite = TRUE,
    verbose = FALSE
  )

  lines <- readLines(rep$output_file, warn = FALSE)
  expect_true(any(grepl("learnr::question", lines, fixed = TRUE)))
  expect_true(any(grepl("learnr::answer", lines, fixed = TRUE)))
})

test_that("invalid MCQ block raises explicit validation error", {
  input <- tempfile(fileext = ".qmd")
  writeLines(c(
    "---",
    "title: bad mcq",
    "---",
    "",
    "```{tutorizeR-mcq}",
    "question: ''",
    "answers:",
    "  - text: ''",
    "    correct: false",
    "```"
  ), input)

  expect_error(
    tutorize(
      input = input,
      output_dir = tempdir(check = TRUE),
      format = "learnr",
      assessment = "mcq",
      overwrite = TRUE,
      verbose = FALSE
    ),
    class = "tutorizeR_error_validation"
  )
})
