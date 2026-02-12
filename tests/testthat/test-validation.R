local_edition(3)

test_that("validate_input enforces existing source file and extension", {
  expect_error(
    validate_input("does-not-exist.Rmd"),
    class = "tutorizeR_error_validation"
  )

  tmp <- tempfile(fileext = ".txt")
  writeLines("text", tmp)

  expect_error(
    validate_input(tmp),
    class = "tutorizeR_error_validation"
  )
})

test_that("validate_output prevents overwrite by default", {
  input <- tempfile(fileext = ".Rmd")
  writeLines("---\ntitle: x\n---", input)

  output <- tempfile(fileext = ".Rmd")
  writeLines("existing", output)

  expect_error(
    validate_output(output_file = output, input_file = input, overwrite = FALSE),
    class = "tutorizeR_error_validation"
  )

  expect_no_error(
    validate_output(output_file = output, input_file = input, overwrite = TRUE)
  )
})

test_that("validate_output forbids input/output identity", {
  input <- tempfile(fileext = ".Rmd")
  writeLines("---\ntitle: x\n---", input)

  expect_error(
    validate_output(output_file = input, input_file = input, overwrite = TRUE),
    class = "tutorizeR_error_validation"
  )
})
