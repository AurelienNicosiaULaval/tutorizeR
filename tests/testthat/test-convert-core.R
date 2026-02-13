local_edition(3)

fixture <- function(...) {
  testthat::test_path("fixtures", ...)
}

test_that("tutorize converts 3 Rmd fixtures without manual intervention", {
  files <- c(
    fixture("rmd", "basic_code.Rmd"),
    fixture("rmd", "setup_and_options.Rmd"),
    fixture("rmd", "non_r_fence.Rmd")
  )

  out_dir <- tempdir(check = TRUE)

  for (f in files) {
    rep <- tutorize(
      input = f,
      output_dir = out_dir,
      format = "learnr",
      assessment = "both",
      overwrite = TRUE,
      verbose = FALSE
    )

    expect_s3_class(rep, "tutorize_report")
    expect_true(file.exists(rep$output_file))

    lines <- readLines(rep$output_file, warn = FALSE)
    expect_true(any(grepl("output: learnr::tutorial", lines, fixed = TRUE)))
  }
})

test_that("tutorize converts 3 qmd fixtures", {
  files <- c(
    fixture("qmd", "basic_code.qmd"),
    fixture("qmd", "with_mcq_block.qmd"),
    fixture("qmd", "skip_and_tags.qmd")
  )

  out_dir <- tempdir(check = TRUE)

  for (f in files) {
    rep <- tutorize(
      input = f,
      output_dir = out_dir,
      format = "learnr",
      assessment = "both",
      overwrite = TRUE,
      verbose = FALSE
    )

    expect_true(file.exists(rep$output_file))
    lines <- readLines(rep$output_file, warn = FALSE)
    expect_true(any(grepl("runtime: shiny_prerendered", lines, fixed = TRUE)))
  }
})

test_that("non-r fenced blocks are preserved", {
  rep <- tutorize(
    input = fixture("rmd", "non_r_fence.Rmd"),
    output_dir = tempdir(check = TRUE),
    format = "learnr",
    assessment = "code",
    overwrite = TRUE,
    verbose = FALSE
  )

  lines <- readLines(rep$output_file, warn = FALSE)
  expect_true(any(grepl("```python", lines, fixed = TRUE)))
  expect_true(any(grepl("print(\"hello\")", lines, fixed = TRUE)))
})

test_that("inline one-line chunks are not dropped", {
  rep <- tutorize(
    input = fixture("edge", "inline_chunk.Rmd"),
    output_dir = tempdir(check = TRUE),
    format = "learnr",
    assessment = "code",
    overwrite = TRUE,
    verbose = FALSE
  )

  lines <- readLines(rep$output_file, warn = FALSE)
  expect_true(any(grepl("trz-ex-", lines, fixed = TRUE)))
  expect_true(any(grepl("trz-sol-", lines, fixed = TRUE)))
})

test_that("duplicate labels are normalized to avoid collisions", {
  rep <- tutorize(
    input = fixture("edge", "duplicate_labels.Rmd"),
    output_dir = tempdir(check = TRUE),
    format = "learnr",
    assessment = "code",
    overwrite = TRUE,
    verbose = FALSE
  )

  lines <- readLines(rep$output_file, warn = FALSE)
  ex_labels <- grep("^```\\{r trz-ex-", lines, value = TRUE)
  expect_true(length(ex_labels) >= 1)
  expect_equal(length(unique(ex_labels)), length(ex_labels))
})

test_that("quarto-live output contains required include and format", {
  rep <- tutorize(
    input = fixture("qmd", "basic_code.qmd"),
    output_dir = tempdir(check = TRUE),
    format = "quarto-live",
    assessment = "both",
    overwrite = TRUE,
    verbose = FALSE
  )

  lines <- readLines(rep$output_file, warn = FALSE)
  expect_true(any(grepl("format: live-html", lines, fixed = TRUE)))
  expect_true(any(grepl("_extensions/r-wasm/live/_knitr.qmd", lines, fixed = TRUE)))
  expect_true(any(grepl("```\\{webr\\}", lines)))
})

test_that("learnr smoke render succeeds when dependencies are available", {
  skip_if_not_installed("learnr")
  skip_if_not_installed("gradethis")

  rep <- tutorize(
    input = fixture("rmd", "basic_code.Rmd"),
    output_dir = tempdir(check = TRUE),
    format = "learnr",
    assessment = "code",
    overwrite = TRUE,
    verbose = FALSE
  )

  expect_true(isTRUE(rep$render_ok))
})

test_that("convert_folder returns per-file status report", {
  dir <- file.path(tempdir(check = TRUE), "tutorize-batch")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  writeLines(
    c(
      "---",
      "title: ok",
      "---",
      "```{r chunk_ok}",
      "1 + 1",
      "```"
    ),
    file.path(dir, "ok.Rmd")
  )

  writeLines(
    c(
      "---",
      "title: bad",
      "---",
      "```{tutorizeR-mcq}",
      "question: ''",
      "answers:",
      "  - text: ''",
      "    correct: false",
      "```"
    ),
    file.path(dir, "bad.qmd")
  )

  rep <- convert_folder(
    dir = dir,
    recursive = FALSE,
    format = "learnr",
    assessment = "both",
    overwrite = TRUE,
    verbose = FALSE
  )

  expect_s3_class(rep, "tutorize_folder_report")
  expect_true(any(rep$results$status == "ok"))
  expect_true(any(rep$results$status == "error"))
})
