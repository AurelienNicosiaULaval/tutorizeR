local_edition(3)

reg_fixture <- function(...) testthat::test_path("regression-fixtures", ...)

test_that("regression fixtures keep stable structural output", {
  src_dir <- reg_fixture("sources")
  expected_dir <- reg_fixture("expected")
  out_dir <- tempfile("reg-out")
  dir.create(out_dir)

  rep <- convert_folder(
    dir = src_dir,
    recursive = FALSE,
    output_dir = out_dir,
    format = "learnr",
    assessment = "both",
    overwrite = TRUE,
    verbose = FALSE
  )

  expect_true(all(rep$results$status == "ok"))

  out_files <- list.files(out_dir, full.names = TRUE)

  expect_true(length(out_files) >= 2)

  for (f in out_files) {
    lines <- readLines(f, warn = FALSE)
    expect_true(any(grepl("output: learnr::tutorial", lines, fixed = TRUE)))
    expect_false(any(grepl("Duplicate chunk label", lines, fixed = TRUE)))

    base <- sub("-tutorial$", "", tools::file_path_sans_ext(basename(f)))
    expected <- file.path(expected_dir, paste0(base, "-expected.Rmd"))
    expect_true(file.exists(expected))
    expect_equal(lines, readLines(expected, warn = FALSE))
  }
})

test_that("stress batch conversion handles volume", {
  root <- tempfile("stress")
  dir.create(root)

  for (i in 1:20) {
    ext <- if (i %% 2 == 0) ".Rmd" else ".qmd"
    file <- file.path(root, sprintf("doc_%02d%s", i, ext))
    writeLines(c(
      "---",
      sprintf("title: 'doc %02d'", i),
      "---",
      "```{r ex}",
      "1 + 1",
      "```"
    ), file)
  }

  rep <- convert_folder(
    dir = root,
    recursive = FALSE,
    format = "learnr",
    assessment = "code",
    overwrite = TRUE,
    verbose = FALSE
  )

  expect_equal(sum(rep$results$status == "ok"), 20)
  expect_equal(sum(rep$results$status == "error"), 0)
})
