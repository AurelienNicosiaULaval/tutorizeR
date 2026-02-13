local_edition(3)

test_that("tutorize report includes lint summary and prints details", {
  lint <- new_tutorize_lint_report(
    source = "source.Rmd",
    findings = data.frame(
      code = c("TRZ001", "TRZ002"),
      severity = c("error", "warning"),
      message = c("duplicate label", "unknown tag"),
      line = c(10L, 12L),
      chunk_label = c("ex1", "ex2"),
      stringsAsFactors = FALSE
    ),
    context = "source"
  )

  report <- new_tutorize_report(
    input_file = "source.Rmd",
    output_file = "source-tutorial.Rmd",
    format = "learnr",
    assessment = "both",
    stats = list(
      exercises = 2L,
      solutions = 2L,
      mcq = 1L,
      sections = 3L,
      chunks_total = 5L,
      mcq_explicit = 1L,
      mcq_from_bank = 0L,
      estimated_minutes = 10.5
    ),
    render_result = list(ok = FALSE, message = "render fail", result = NULL),
    warnings = c("warn-a", "warn-b"),
    lint_report = lint
  )

  expect_s3_class(report, "tutorize_report")
  expect_equal(report$lint_summary$errors, 1L)
  expect_equal(report$lint_summary$warnings, 1L)
  expect_false(report$render_ok)

  printed <- capture_output(print(report))
  expect_true(any(grepl("render fail", printed, fixed = TRUE)))
  expect_true(any(grepl("Warnings:", printed, fixed = TRUE)))
})

test_that("write_tutorize_report supports yaml and rejects invalid input", {
  report <- new_tutorize_report(
    input_file = "in.qmd",
    output_file = "out.Rmd",
    format = "learnr",
    assessment = "code",
    stats = list(
      exercises = 1L,
      solutions = 1L,
      mcq = 0L,
      sections = 1L,
      chunks_total = 2L,
      mcq_explicit = 0L,
      mcq_from_bank = 0L,
      estimated_minutes = 4
    ),
    render_result = list(ok = TRUE, message = "", result = "ok"),
    warnings = character(),
    lint_report = NULL
  )

  out_yaml <- tempfile(fileext = ".yml")
  out_json <- tempfile(fileext = ".json")

  expect_equal(write_tutorize_report(report, out_yaml, format = "yaml"), out_yaml)
  expect_equal(write_tutorize_report(report, out_json, format = "json"), out_json)
  expect_true(file.exists(out_yaml))
  expect_true(file.exists(out_json))
  expect_true(any(grepl("schema_version", readLines(out_yaml, warn = FALSE), fixed = TRUE)))

  expect_error(
    write_tutorize_report(list(not = "a report"), out_json, format = "json"),
    class = "tutorizeR_error_validation"
  )
})

test_that("folder report print includes failed files when present", {
  rows <- data.frame(
    input = c("a.Rmd", "b.qmd"),
    output = c("a-tutorial.Rmd", ""),
    status = c("ok", "error"),
    message = c("", "boom"),
    stringsAsFactors = FALSE
  )

  rep <- new_tutorize_folder_report(rows)
  expect_s3_class(rep, "tutorize_folder_report")

  printed <- capture_output(print(rep))
  expect_true(any(grepl("failed", printed, fixed = TRUE)))
  expect_true(any(grepl("b.qmd", printed, fixed = TRUE)))
  expect_true(any(grepl("boom", printed, fixed = TRUE)))
})
