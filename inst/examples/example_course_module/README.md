# Example Course Module

This module demonstrates a small data science lesson that can be converted with `tutorizeR`.

Files:

- `source_lesson.qmd`: instructor-maintained Quarto source.
- `student_activity.csv`: local teaching dataset.
- `transformed_tutorial.Rmd`: example `learnr` output after conversion.
- `generated_exercises.Rmd`: extracted exercise examples.
- `grading_examples.R`: optional `gradethis` checks that an instructor can adapt.

Example conversion:

```r
library(tutorizeR)

module_dir <- system.file(
  "examples",
  "example_course_module",
  package = "tutorizeR"
)

report <- tutorize(
  input = file.path(module_dir, "source_lesson.qmd"),
  output_dir = tempdir(),
  format = "learnr",
  assessment = "both",
  overwrite = TRUE
)

print(report)
```

The dataset is synthetic and intended only for documentation and testing of teaching workflows.

