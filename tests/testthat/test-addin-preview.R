local_edition(3)

test_that("preview addin is guarded outside RStudio", {
  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    skip("rstudioapi not available")
  }

  if (!rstudioapi::isAvailable()) {
    expect_error(
      launch_tutorizeR_preview_addin(),
      class = "tutorizeR_error_validation"
    )
  } else {
    skip("RStudio is available in this environment")
  }
})
