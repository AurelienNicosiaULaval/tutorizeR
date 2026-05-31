# Reproduce the example course module after package installation.

required_packages <- c(
  "tutorizeR",
  "dplyr",
  "ggplot2",
  "readr",
  "learnr",
  "gradethis"
)

missing_packages <- required_packages[!vapply(
  required_packages,
  requireNamespace,
  quietly = TRUE,
  FUN.VALUE = logical(1)
)]

if (length(missing_packages) > 0L) {
  stop(
    paste0(
      "The example course module requires these installed R packages: ",
      paste(missing_packages, collapse = ", "),
      ". Install the package dependencies declared in DESCRIPTION before ",
      "running this script. gradethis is available from ",
      "https://rstudio.r-universe.dev."
    ),
    call. = FALSE
  )
}

library(tutorizeR)

example_dir <- system.file(
  "examples",
  "example_course_module",
  package = "tutorizeR"
)

if (!nzchar(example_dir)) {
  stop("Could not locate the installed example module.", call. = FALSE)
}

work_dir <- file.path(tempdir(), "tutorizeR-example-course-module")
if (dir.exists(work_dir)) {
  unlink(work_dir, recursive = TRUE)
}
dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)

file.copy(
  from = file.path(example_dir, "lesson-source.qmd"),
  to = file.path(work_dir, "lesson-source.qmd"),
  overwrite = TRUE
)
file.copy(
  from = file.path(example_dir, "student_activity.csv"),
  to = file.path(work_dir, "student_activity.csv"),
  overwrite = TRUE
)

if (!file.exists(file.path(work_dir, "lesson-source.qmd")) ||
    !file.exists(file.path(work_dir, "student_activity.csv"))) {
  stop("Could not copy the example source files to the temporary directory.", call. = FALSE)
}

question_bank <- load_question_bank(file.path(example_dir, "question-bank"))

learnr_report <- tutorize(
  input = file.path(work_dir, "lesson-source.qmd"),
  output_dir = work_dir,
  format = "learnr",
  assessment = "both",
  question_bank = question_bank,
  mcq_source = "mixed",
  overwrite = TRUE,
  verbose = FALSE
)

write_tutorize_report(
  report = learnr_report,
  file = file.path(work_dir, "conversion-report.json"),
  format = "json"
)

live_report <- tutorize(
  input = file.path(work_dir, "lesson-source.qmd"),
  output_dir = work_dir,
  format = "quarto-live",
  assessment = "both",
  question_bank = question_bank,
  mcq_source = "mixed",
  overwrite = TRUE,
  verbose = FALSE
)

message("Example files written to: ", work_dir)
message("learnr output: ", learnr_report$output_file)
message("quarto-live output: ", live_report$output_file)
message("conversion report: ", file.path(work_dir, "conversion-report.json"))

invisible(list(
  work_dir = work_dir,
  learnr_report = learnr_report,
  live_report = live_report,
  conversion_report = file.path(work_dir, "conversion-report.json")
))
