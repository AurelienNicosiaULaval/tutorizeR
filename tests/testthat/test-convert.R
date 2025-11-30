test_that("convert_to_tutorial works for learnr", {
    # Create a dummy Rmd file
    tmp_dir <- tempdir()
    input_file <- file.path(tmp_dir, "test.Rmd")
    writeLines(
        c(
            "---",
            "title: 'Test'",
            "---",
            "",
            "```{r setup}",
            "library(learnr)",
            "```",
            "",
            "```{r chunk1}",
            "1 + 1",
            "```"
        ),
        input_file
    )

    output_file <- file.path(tmp_dir, "test-tutorial.Rmd")

    expect_message(convert_to_tutorial(
        input_file,
        output_file,
        add_mcq = FALSE
    ))

    expect_true(file.exists(output_file))

    lines <- readLines(output_file)

    # Check header
    expect_true(any(grepl("runtime: shiny_prerendered", lines)))

    # Check setup chunk preserved
    expect_true(any(grepl("```{r setup}", lines, fixed = TRUE)))

    # Check exercise created
    expect_true(any(grepl("exercise=TRUE", lines)))
    expect_true(any(grepl("ex1-solution", lines)))
})

test_that("convert_to_tutorial works for quarto-live", {
    # Create a dummy Rmd file
    tmp_dir <- tempdir()
    input_file <- file.path(tmp_dir, "test_live.qmd")
    writeLines(
        c(
            "---",
            "title: 'Test Live'",
            "---",
            "",
            "```{r chunk1}",
            "1 + 1",
            "```"
        ),
        input_file
    )

    output_file <- file.path(tmp_dir, "test-live.qmd")

    expect_message(convert_to_tutorial(
        input_file,
        output_file,
        format = "quarto-live"
    ))

    expect_true(file.exists(output_file))

    lines <- readLines(output_file)

    # Check header
    expect_true(any(grepl("format: live-html", lines)))
    expect_true(any(grepl("r-wasm/live/_knitr.qmd", lines)))

    # Check webr chunk created
    expect_true(any(grepl("{webr}", lines, fixed = TRUE)))

    # Check solution callout
    expect_true(any(grepl("callout-tip", lines)))
})

test_that("assessment argument works", {
    tmp_dir <- tempdir()
    input_file <- file.path(tmp_dir, "test_assessment.Rmd")
    writeLines(
        c(
            "---",
            "title: 'Test Assessment'",
            "---",
            "",
            "```{r chunk1}",
            "1 + 1",
            "```"
        ),
        input_file
    )

    output_file <- file.path(tmp_dir, "test-assessment.Rmd")

    # Test assessment = "mcq"
    expect_message(convert_to_tutorial(
        input_file,
        output_file,
        assessment = "mcq"
    ))
    lines <- readLines(output_file)
    expect_false(any(grepl("exercise=TRUE", lines))) # No exercise
    expect_true(any(grepl("learnr::question", lines))) # MCQ present

    # Test assessment = "both"
    expect_message(convert_to_tutorial(
        input_file,
        output_file,
        assessment = "both"
    ))
    lines <- readLines(output_file)
    expect_true(any(grepl("exercise=TRUE", lines))) # Exercise present
    expect_true(any(grepl("learnr::question", lines))) # MCQ present
})

test_that("skip chunk works", {
    tmp_dir <- tempdir()
    input_file <- file.path(tmp_dir, "test_skip.Rmd")
    writeLines(
        c(
            "---",
            "title: 'Test Skip'",
            "---",
            "",
            "```{r chunk1}",
            "# tutorizeR: skip",
            "1 + 1",
            "```"
        ),
        input_file
    )

    output_file <- file.path(tmp_dir, "test-skip.Rmd")

    expect_message(convert_to_tutorial(input_file, output_file))
    lines <- readLines(output_file)
    expect_false(any(grepl("exercise=TRUE", lines))) # No exercise
    expect_true(any(grepl("1 \\+ 1", lines))) # Code preserved
})

test_that("convert_folder works", {
    tmp_dir <- tempdir()
    dir.create(file.path(tmp_dir, "batch"))

    f1 <- file.path(tmp_dir, "batch", "doc1.Rmd")
    f2 <- file.path(tmp_dir, "batch", "doc2.qmd")

    writeLines(c("---", "title: 'Doc 1'", "---", "```{r} 1+1 ```"), f1)
    writeLines(c("---", "title: 'Doc 2'", "---", "```{r} 2+2 ```"), f2)

    expect_message(convert_folder(file.path(tmp_dir, "batch")))

    expect_true(file.exists(file.path(tmp_dir, "batch", "doc1-tutorial.Rmd")))
    expect_true(file.exists(file.path(tmp_dir, "batch", "doc2-tutorial.Rmd")))
})
