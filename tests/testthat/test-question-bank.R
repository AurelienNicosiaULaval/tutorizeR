local_edition(3)

fixture <- function(...) testthat::test_path("fixtures", ...)

test_that("question bank loads YAML and JSON and validates", {
  bank <- load_question_bank(
    path = fixture("question-bank"),
    recursive = TRUE,
    format = "auto",
    strict = TRUE
  )

  expect_s3_class(bank, "tutorize_question_bank")
  expect_true(length(bank$questions) >= 3)

  rep <- validate_question_bank(bank, strict = FALSE)
  expect_s3_class(rep, "tutorize_lint_report")
  expect_equal(rep$summary$errors, 0)
})

test_that("mcq-ref block consumes question bank ids deterministically", {
  bank <- load_question_bank(fixture("question-bank"), strict = TRUE)

  rep <- tutorize(
    input = fixture("qmd", "with_mcq_ref.qmd"),
    output_dir = tempdir(check = TRUE),
    format = "learnr",
    assessment = "both",
    question_bank = bank,
    mcq_source = "mixed",
    overwrite = TRUE,
    verbose = FALSE
  )

  lines <- readLines(rep$output_file, warn = FALSE)
  expect_true(any(grepl("mean\\(c\\(1,2,3\\)\\)", lines, fixed = FALSE)))
  expect_true(any(grepl("learnr::question", lines, fixed = TRUE)))
  expect_true(rep$mcq_from_bank >= 1)
})

test_that("mcq-ref without bank fails with explicit error", {
  expect_error(
    tutorize(
      input = fixture("qmd", "with_mcq_ref.qmd"),
      output_dir = tempdir(check = TRUE),
      format = "learnr",
      assessment = "both",
      overwrite = TRUE,
      verbose = FALSE
    ),
    class = "tutorizeR_error_validation"
  )
})
