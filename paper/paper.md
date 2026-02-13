---
title: 'tutorizeR: Convert teaching materials in R Markdown and Quarto into interactive, assessable tutorials'
short-title: tutorizeR
tags:
  - R
  - education
  - learnr
  - quarto
  - gradethis
  - lms
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
date: 2026-02-13
output:
  md_document:
    variant: gfm
    preserve_yaml: true
    md_extensions: +smart
bibliography: paper.bib
---

## Summary

Many teachers build course content with `.Rmd` or `.qmd` documents and need to quickly deliver interactive learning experiences to students. `tutorizeR` converts these source materials into `learnr` tutorials and `quarto-live` resources while preserving narrative text, setup chunks, and key execution options. It integrates with the broader R-based ecosystem for education and reproducible publishing, including `R` itself, `learnr`, `gradethis`, and `Quarto` for teaching and delivery workflows (@rCoreTeam2025; @learnr2025; @gradethis2025; @quarto2025).

The package adds structured pedagogical primitives: automatic exercise/solution chunk generation, MCQ extraction (inline or reference bank-driven), linting checks for teaching-oriented issues, conversion report export (JSON/YAML), and LMS-ready manifest export for generic Canvas/Moodle workflows.

## Statement of need

Existing workflows for turning `.Rmd`/`.qmd` teaching documents into interactive materials are often manual: instructors duplicate code chunks, handcraft MCQ syntax, and maintain separate scripts for grading scaffolds. This is especially costly for large course folders with mixed formats and heterogeneous chunk options.

`tutorizeR` addresses this by offering a stable conversion pipeline with: 

- conversion of both `.Rmd` and `.qmd` inputs,
- deterministic label management to avoid render collisions,
- optional strict linting before rendering,
- reusable question-bank integration (YAML/JSON),
- and reporting artifacts suitable for CI.

## Functionality

The package exposes a canonical `tutorize()` API and keeps backward-compatible wrappers (`convert_to_tutorial()`, `convert_folder()`). Additional authoring tools include:

- `lint_source()` for pedagogy-focused validations,
- `load_question_bank()` / `validate_question_bank()` for external banked MCQs,
- `write_tutorize_report()` for machine-readable conversion summaries,
- `export_lms_manifest()` and `export_tutorial_package()` for downstream sharing.

For workflow robustness, `tutorizeR` provides both interactive UI paths (RStudio addins) and non-interactive CLI execution.

## Quality assurance

The package includes tests and linting, with automated checks covering core parser/transform/validation/report paths. New/changed features are documented via roxygen and vignettes, and conversion edge cases are regression tested (including non-R chunks, inline code fences, duplicated labels, and Quarto-only fixtures).

## Availability and contribution

The package source is available on GitHub at:
`https://github.com/AurelienNicosiaULaval/tutorizeR`.

Contributions and bug reports are welcome via GitHub Issues/PRs with the repository’s contribution guide and code of conduct.

## References
