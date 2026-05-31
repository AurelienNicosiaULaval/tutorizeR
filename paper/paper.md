---
title: 'tutorizeR: Converting Reproducible Data Science Lessons into Interactive Learning Experiences'
short-title: tutorizeR
tags:
  - R
  - education
  - learnr
  - quarto
  - gradethis
  - reproducibility
authors:
  - name: Aurélien Nicosia
    orcid: null
    affiliation: 1
    equal-contrib: true
    corresponding: true
  - name: "tutorizeR contributors"
    affiliation: 1
    equal-contrib: false
affiliations:
  - name: Université Laval
    index: 1
date: 2026-05-31
output:
  md_document:
    variant: gfm
    preserve_yaml: true
    md_extensions: +smart
bibliography: paper.bib
---

# Summary

Many instructors already prepare data science, statistics, and R programming lessons in R Markdown or Quarto. These formats are well suited to reproducible teaching because narrative, code, output, and figures can live in one source document. However, turning an existing lesson into an interactive tutorial often requires repetitive manual work: duplicating code chunks, creating exercise areas, writing solution chunks, adding conceptual questions, and maintaining separate versions for students and instructors.

`tutorizeR` automates this conversion workflow. The package converts `.Rmd` and `.qmd` source documents into `learnr` tutorials or `quarto-live` resources, while preserving narrative text, setup chunks, selected chunk options, and instructor annotations. It supports automatic exercise transformation, insertion of student answer areas, reusable MCQ question banks, `gradethis` setup for feedback, linting, conversion reports, and LMS-oriented manifest export. The package is implemented in R and integrates with established tools for reproducible teaching, including R, R Markdown, Quarto, `learnr`, and `gradethis` [@rCoreTeam2026; @rmarkdown2026; @quarto2026; @learnr2026; @gradethis2026].

# Statement of Need

Interactive tutorials can support active learning by asking students to edit code, inspect outputs, answer conceptual questions, and receive feedback. For instructors, the practical barrier is maintenance. A course may already have dozens of lecture notes, laboratories, and assignments written as `.Rmd` or `.qmd` files. Rebuilding those materials by hand as `learnr` tutorials or browser-executable Quarto activities duplicates effort and increases the chance that instructor notes, student tutorials, and solutions drift apart.

`tutorizeR` addresses this need by keeping the source lesson as the authoritative document. Instructors can update a lesson once, rerun the conversion pipeline, and regenerate student-facing materials. This is especially relevant in scalable teaching contexts where multiple instructors or teaching assistants must coordinate reproducible materials across weeks, sections, and cohorts.

The need is educational rather than only technical. The package is intended to help instructors create active learning experiences from existing reproducible documents, reduce maintenance burden, and make iterative course updates more reliable. Actual classroom adoption is not verifiable from repository contents.

# Functionality

The package provides a high-level `tutorize()` function and compatibility wrappers for existing workflows. Core functionality includes:

- parsing `.Rmd` and `.qmd` source files into text, setup chunks, R chunks, non-R chunks, MCQ blocks, and question-bank references;
- generating `learnr` exercise chunks and solution chunks;
- generating `quarto-live` compatible exercise scaffolds;
- inserting `learnr`, `gradethis`, and `gradethis_setup()` into generated `learnr` setup chunks;
- preserving non-R fenced blocks rather than converting them incorrectly;
- normalizing generated labels to reduce chunk-label collisions;
- supporting instructor tags such as `skip`, `exercise-only`, `solution-only`, `mcq`, `narrative-only`, `locked`, and `hints`;
- loading reusable YAML or JSON question banks;
- linting source documents before conversion;
- writing JSON or YAML conversion reports;
- exporting LMS-oriented manifests for downstream course workflows;
- providing RStudio addins and a command-line script for instructor use.

# Educational Use

The package supports workflows common in undergraduate statistics, data science, and R programming courses. A typical use case starts with an instructor-maintained Quarto lesson that imports data, performs a data manipulation task, builds a visualization, and asks students to interpret the result. `tutorizeR` converts that lesson into a tutorial with exercise areas, solutions, and MCQs.

The repository includes an example course module in `inst/examples/example_course_module/`. The module uses a small synthetic dataset to demonstrate a realistic lesson on study patterns and quiz performance. It includes a source Quarto document, a transformed `learnr` tutorial, generated exercises, grading examples, and a local CSV file. The example is suitable for demonstrating the package to instructors without relying on private course data.

The design is inspired by workflows that occur in introductory and advanced undergraduate statistics or data science courses, including course-code contexts such as STT-1100 and STT-4230. No private course material is included. Specific deployment in those courses is not verifiable from repository contents.

# Availability

The package source is available at:

`https://github.com/AurelienNicosiaULaval/tutorizeR`

The repository includes installation instructions, tests, vignettes, examples, contribution guidelines, a code of conduct, citation metadata, and reviewer-facing JOSE preparation documents.

# Quality Assurance

The package includes `testthat` tests for parsing, conversion, exercise generation, MCQ handling, question-bank loading, validation errors, report writing, LMS manifest export, regression fixtures, `gradethis` setup generation, `quarto-live` output, and publication readiness assets. GitHub Actions are configured to run `R CMD check`, tests, linting, and coverage.

Before submission, the maintainers should run a fresh local check on the current tree:

```bash
Rscript -e "testthat::test_local()"
Rscript -e "lintr::lint_package()"
R CMD build .
R CMD check --as-cran --no-manual tutorizeR_*.tar.gz
```

# AI Usage Disclosure

Codex using GPT-5 was used on 2026-05-31 to assist with repository audit text, reviewer documentation, example files, tests, and manuscript revisions. The maintainer is responsible for reviewing, validating, and approving all content before submission.

# References
