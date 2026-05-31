# Educational Use Cases for tutorizeR

## Purpose

`tutorizeR` is educational infrastructure for instructors who already write course material in R Markdown or Quarto and want to convert those source documents into interactive learning experiences. The package supports a source-first workflow: instructors maintain one reproducible lesson source, then generate student-facing tutorials, exercises, feedback scaffolds, and reporting artifacts.

## Classroom Usage

Typical classroom settings include undergraduate statistics, data science, and R programming courses where students learn by reading short explanations, running code, editing code, answering conceptual questions, and receiving structured feedback.

Relevant classroom activities include:

- converting lecture notes into interactive pre-class tutorials;
- converting laboratory handouts into `learnr` tutorials;
- generating `quarto-live` activities for browser-based experimentation;
- inserting student answer areas into existing code examples;
- adding MCQ prompts for conceptual checks;
- creating reproducible assignment templates with consistent structure;
- exporting conversion reports for teaching assistants or course coordination.

Specific course adoption: Not verifiable from repository contents.

## Instructor Workflows

An instructor can prepare a source lesson as a `.qmd` or `.Rmd` file:

```r
library(tutorizeR)

report <- tutorize(
  input = "lessons/week03-data-visualization.qmd",
  output_dir = "tutorials",
  format = "learnr",
  assessment = "both",
  overwrite = TRUE
)

print(report)
```

For a course folder, the instructor can process all lessons consistently:

```r
library(tutorizeR)

folder_report <- convert_folder(
  dir = "lessons",
  recursive = TRUE,
  output_dir = "tutorials",
  format = "learnr",
  assessment = "both",
  overwrite = TRUE
)

print(folder_report)
```

Before distributing materials, the instructor can lint a source file and inspect the conversion report:

```r
library(tutorizeR)

lint <- lint_source("lessons/week03-data-visualization.qmd", strict = FALSE)
print(lint)
```

## Student Workflows

Students interact with the converted output rather than the conversion pipeline. In a generated tutorial, a student can:

- read the original lesson narrative;
- run setup code prepared by the instructor;
- complete generated code exercises;
- compare their answer to hidden or collapsible solution material;
- answer MCQs generated from inline blocks or question banks;
- work with the same source datasets and code patterns used in the original lesson.

For large classes, this workflow can reduce inconsistencies between lecture notes, labs, and assignments because the student-facing tutorial remains derived from a single source file.

## Learning Objectives

Examples of learning objectives supported by the package include:

- write reproducible R code from a partially completed scaffold;
- apply tidy data transformations to realistic teaching data;
- construct and interpret `ggplot2` visualizations;
- identify common errors in data manipulation pipelines;
- connect conceptual questions to executable code;
- practice iterative programming with immediate feedback;
- understand the relationship between source documents and generated learning resources.

## Example Lesson Pattern

A realistic data science lesson can be organized as follows:

1. Short conceptual introduction.
2. Dataset import.
3. Data cleaning with `dplyr`.
4. Visualization with `ggplot2`.
5. Interpretation prompt.
6. MCQ conceptual check.
7. Reproducible summary exercise.

The example module in `inst/examples/example_course_module/` follows this pattern with a small, local dataset and generated tutorial artifacts.

## Evidence Boundaries

The repository demonstrates educational intent, package functionality, examples, tests, and reviewer-facing documentation. It does not contain verifiable classroom outcome data, anonymized student analytics, external adoption records, or formal evaluation results. Therefore, any claim about actual classroom adoption should be written as:

Not verifiable from repository contents.
