# Optional gradethis checks for the example course module.
# These checks are illustrative and should be reviewed by the instructor.

summarize_program_check <- gradethis::grade_this({
  gradethis::pass_if(
    ~ all(c("program", "mean_hours", "mean_score") %in% names(.result)),
    "The summary contains the expected columns."
  )

  gradethis::fail("Compute mean_hours and mean_score by program.")
})

plot_study_score_check <- gradethis::grade_this({
  gradethis::pass_if(
    ~ inherits(.result, "ggplot"),
    "The result is a ggplot object."
  )

  gradethis::fail("Create a ggplot object mapping study_hours and quiz_score.")
})
