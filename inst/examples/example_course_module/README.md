# Example Course Module: Data Visualization and Summarisation

This module demonstrates a small, generic data science lesson that can be converted with `tutorizeR`. It is designed for instructors who want to see a source-first workflow using Quarto, `learnr`, `quarto-live`, MCQs, a reusable question bank, and conversion reports.

The dataset is synthetic and intended only for documentation and testing of teaching workflows.

Educational example materials in this directory are released under CC-BY 4.0 unless otherwise specified. See `LICENSE-CONTENT.md` in the repository root.

## Teaching context

The example represents a short undergraduate activity on data summarisation and visualization with R. Students inspect a small table of weekly study activity, compute grouped summaries, and create a scatterplot relating study time to quiz score.

This module demonstrates an adoption pattern. It does not provide evidence of classroom deployment or student learning gains. Not verifiable from repository contents.

## Learning objectives

After completing the activity, students should be able to:

- import a local CSV file;
- compute grouped summaries with `dplyr`;
- create a basic visualization with `ggplot2`;
- interpret a small exploratory graph cautiously;
- answer conceptual questions about variables and summaries.

Files:

- `lesson-source.qmd`: instructor-maintained Quarto source.
- `student_activity.csv`: local teaching dataset.
- `question-bank/questions.yml`: reusable MCQ question bank.
- `expected/lesson-source-tutorial.Rmd`: expected `learnr` output scaffold.
- `expected/lesson-source-live.qmd`: expected `quarto-live` output scaffold.
- `expected/conversion-report.json`: expected conversion report for the `learnr` conversion.
- `run-example.R`: reproducible script that runs the example after package installation.

## Reproduce the example

From an installed package:

```r
library(tutorizeR)

module_dir <- system.file(
  "examples",
  "example_course_module",
  package = "tutorizeR"
)

source(file.path(module_dir, "run-example.R"))
```

The script copies the module to a temporary directory, loads the question bank, converts the lesson to `learnr`, converts the lesson to `quarto-live`, writes a JSON conversion report, and prints the output paths.

## What tutorizeR automates

The package automates:

- transformation of ordinary R chunks into student exercise areas;
- generation of solution chunks;
- conversion of explicit MCQ blocks;
- insertion of question-bank MCQs;
- insertion of `learnr` setup code and optional `gradethis` setup code;
- generation of conversion reports.

## What instructors still need to review

Instructors should manually verify:

- grading checks and feedback language;
- dataset availability in the deployment environment;
- optional package availability, especially `learnr`, `dplyr`, `ggplot2`, `readr`, and `gradethis` for teacher-authored feedback checks;
- accessibility and clarity of generated prompts;
- final suitability for the course context.

Formal learning-outcome evaluation: Not verifiable from repository contents.
