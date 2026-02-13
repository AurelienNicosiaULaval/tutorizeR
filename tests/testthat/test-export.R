local_edition(3)

fixture <- function(...) testthat::test_path("fixtures", ...)

test_that("export_lms_manifest writes profile-aware files", {
  in_file <- fixture("rmd", "basic_code.Rmd")

  m1 <- export_lms_manifest(in_file, profile = "generic", language = "en")
  m2 <- export_lms_manifest(in_file, profile = "canvas", language = "en")
  m3 <- export_lms_manifest(in_file, profile = "moodle", language = "en")

  expect_s3_class(m1, "tutorize_lms_manifest")
  expect_true(file.exists(m1$output_file))
  expect_true(file.exists(m2$output_file))
  expect_true(file.exists(m3$output_file))
  expect_true(length(m2$manifest$items) >= 1)
})

test_that("export_tutorial_package creates opt-in tutorial package scaffold", {
  in_file <- fixture("rmd", "basic_code.Rmd")
  root <- tempfile("pkg-root")
  dir.create(root)

  pkg <- export_tutorial_package(
    input = in_file,
    path = root,
    package_name = "DemoTutorialPkg",
    overwrite = TRUE
  )

  expect_true(dir.exists(pkg))
  expect_true(file.exists(file.path(pkg, "DESCRIPTION")))
  expect_true(length(list.files(file.path(pkg, "inst", "tutorials"), pattern = "\\.Rmd$")) >= 1)
})
