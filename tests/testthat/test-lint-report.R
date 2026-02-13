local_edition(3)

test_that("lint_source detects unknown/conflicting tags", {
  input <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "---",
    "title: lint",
    "---",
    "```{r c1}",
    "# tutorizeR: skip, exercise-only, strange-tag",
    "1 + 1",
    "```"
  ), input)

  rep <- lint_source(input, strict = FALSE)

  expect_s3_class(rep, "tutorize_lint_report")
  expect_true(any(rep$findings$code == "TRZ002"))
  expect_true(any(rep$findings$code == "TRZ003"))
})

test_that("lint strict mode blocks conversion", {
  input <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "---",
    "title: lint strict",
    "---",
    "```{r c1}",
    "# tutorizeR: skip, solution-only",
    "1 + 1",
    "```"
  ), input)

  expect_error(
    tutorize(
      input = input,
      output_dir = tempdir(check = TRUE),
      format = "learnr",
      assessment = "both",
      lint_strict = TRUE,
      overwrite = TRUE,
      verbose = FALSE
    ),
    class = "tutorizeR_error_validation"
  )
})

test_that("write_tutorize_report exports JSON schema", {
  input <- tempfile(fileext = ".Rmd")
  writeLines(c("---", "title: rep", "---", "```{r}", "1+1", "```"), input)

  rep <- tutorize(
    input = input,
    output_dir = tempdir(check = TRUE),
    format = "learnr",
    assessment = "code",
    overwrite = TRUE,
    verbose = FALSE
  )

  out <- tempfile(fileext = ".json")
  path <- write_tutorize_report(rep, out, format = "json", pretty = TRUE)
  expect_true(file.exists(path))

  parsed <- jsonlite::fromJSON(path)
  expect_equal(parsed$schema_version, "1.0")
  expect_true("estimated_minutes" %in% names(parsed))
})

test_that("i18n fallback works and french conversion writes french setup label", {
  expect_true(grepl("Tutoriel", tr("labels.interactive_tutorial", language = "fr"), fixed = TRUE))
  expect_equal(tr("unknown.key", language = "fr"), "unknown.key")

  input <- tempfile(fileext = ".Rmd")
  writeLines(c("---", "title: x", "---", "```{r}", "1+1", "```"), input)

  rep <- tutorize(
    input = input,
    output_dir = tempdir(check = TRUE),
    format = "learnr",
    assessment = "code",
    language = "fr",
    overwrite = TRUE,
    verbose = FALSE
  )

  lines <- readLines(rep$output_file, warn = FALSE)
  expect_true(any(grepl("Chunk setup ajoute", lines, fixed = TRUE)))
})
