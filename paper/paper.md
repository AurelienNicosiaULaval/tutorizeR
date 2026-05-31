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
    affiliation: 1
    corresponding: true
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

## Summary

`tutorizeR` is open-source educational infrastructure for instructors who prepare statistics, data science, or R programming lessons in R Markdown or Quarto and want to turn those source documents into interactive learning resources. The package converts `.Rmd` and `.qmd` files into `learnr` tutorials or `quarto-live` resources, while preserving a source-first workflow: instructors maintain one reproducible lesson and generate student-facing exercise material from it. It is built in R [@rCoreTeam2025] and works with the R Markdown, Quarto, `learnr`, and `gradethis` ecosystems [@rmarkdownDocs; @quartoDocs; @learnrDocs; @gradethisDocs].

The educational problem is practical and recurring. Many instructors already write lessons as computational narratives, where text, code, figures, and interpretation are interleaved. This approach has roots in literate programming [@knuth1984] and is consistent with current recommendations for computational notebooks and reproducible analyses [@rule2019; @wilson2017]. However, preparing an interactive tutorial from an existing source lesson often requires manual duplication of chunks, insertion of exercise areas, management of solutions, creation of conceptual checks, and coordination of feedback scaffolds. `tutorizeR` automates much of that transformation while leaving final pedagogical judgement to the instructor.

## Statement of Need

JOSE accepts open-source software that supports teaching and learning or makes an educational process better, easier, simpler, or faster. `tutorizeR` fits this software-submission category as infrastructure for computational education. It does not replace course design, teaching expertise, or assessment design. Instead, it reduces the mechanical cost of converting reproducible teaching documents into interactive activities.

The need is especially visible in courses where instructors maintain weekly laboratories, tutorials, and assignments. A single update to a dataset, plot, or explanation can otherwise require changes in multiple versions of the same lesson. `tutorizeR` supports a workflow in which the instructor revises the source `.qmd` or `.Rmd` file, reruns the conversion pipeline, reviews the generated output, and distributes the result. The package also supports formative practice by generating code exercises, MCQs, and `gradethis`-ready `learnr` tutorials. This design is aligned with broad evidence that active learning and formative feedback can be valuable in STEM education [@freeman2014; @black1998], but this repository does not claim that `tutorizeR` itself improves grades, engagement, or learning outcomes.

## Educational context and target users

The primary users are instructors, teaching assistants, and course teams who create computational teaching materials in R. Target contexts include introductory data science, applied statistics, R programming, and methods courses where students learn through executable examples and short practice tasks. The package is also relevant to open educational resource workflows because its example materials can be reused and adapted under an open content license, consistent with open education principles [@unesco2019].

The repository includes a complete installable example module on data summarisation and visualization with R. The example is generic and synthetic. It is designed to show how another instructor could adopt the package without access to private course material.

## Functionality

`tutorizeR` parses source documents, preserves narrative text, detects setup and R chunks, preserves non-R fenced blocks, generates exercise and solution areas, transforms explicit MCQ blocks, resolves references to local question banks, adds `learnr` and `gradethis` setup code when needed, and writes conversion reports. The package includes validation and linting helpers so instructors can inspect source documents before conversion, plus batch conversion for folders of lessons.

The detailed API is documented in the package reference, vignettes, and examples. The JOSE paper intentionally does not duplicate that documentation.

## Teaching and adoption workflow

A typical adoption workflow has five steps. First, an instructor writes a lesson in Quarto or R Markdown with learning objectives, narrative explanation, executable R chunks, and optional instructor tags. Second, the instructor adds MCQ blocks or question-bank references where conceptual checks are useful. Third, the instructor runs `tutorize()` to generate a `learnr` or `quarto-live` resource. Fourth, the instructor reviews the generated tutorial, adjusts wording or grading checks, and confirms that datasets and optional dependencies are available. Fifth, the resulting file can be distributed through the course platform, rendered locally, or incorporated into a broader teaching workflow.

The example module in `inst/examples/example_course_module/` demonstrates this workflow with a source lesson, local question bank, expected converted outputs, JSON report, and reproducible run script.

## Experience of use and current limitations

The repository demonstrates intended use through tests, vignettes, and an installable example. The following claims are not made because supporting evidence is not present in the repository.

- Formal learning-outcome evaluation: Not verifiable from repository contents.
- Actual classroom deployment: Not verifiable from repository contents.
- Broad external adoption: Not verifiable from repository contents.

Current limitations are also part of the educational story. `quarto-live` output requires the relevant Quarto live extension in the teaching project. `learnr` rendering requires optional runtime dependencies. LMS support is currently manifest-oriented rather than direct LMS publication. Generated material should always be reviewed by an instructor before release, especially grading logic and feedback wording.

## Availability and licensing

The package source is available at `https://github.com/AurelienNicosiaULaval/tutorizeR`. The package code is distributed under the MIT license. Educational examples and graphical documentation assets are released under CC-BY 4.0 unless otherwise specified. The repository includes tests, continuous integration configuration, contribution guidelines, a code of conduct, vignettes, reviewer documentation, and citation metadata.

Final release DOI: Not verifiable from repository contents.

## AI usage disclosure

Generative AI tools were used during planning, review, or documentation-support stages for this repository. The author reviewed, edited, tested, and takes responsibility for all submitted code, documentation, and manuscript text.

The package itself does not depend on any generative AI model. Its conversion pipeline is deterministic and script-driven.

## References
