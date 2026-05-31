# JOSE Release Bundle

Date: 2026-05-31

This file lists the repository artifacts to prepare before a possible JOSE submission.

## Core Submission Files

- `paper/paper.md`
- `paper/paper.bib`
- `README.md`
- `DESCRIPTION`
- `LICENSE`
- `LICENSE.md`
- `LICENSE-CONTENT.md`
- `CITATION.cff`

## Reviewer Documentation

- `docs/jose_submission_guide.md`
- `docs/jose_review_checklist.md`
- `docs/jose_blockers_report.md`
- `docs/educational_use_evidence.md`
- `docs/jose_pr_final_report.md`

## Teaching Demonstration

- `inst/examples/example_course_module/README.md`
- `inst/examples/example_course_module/lesson-source.qmd`
- `inst/examples/example_course_module/question-bank/questions.yml`
- `inst/examples/example_course_module/expected/lesson-source-tutorial.Rmd`
- `inst/examples/example_course_module/expected/lesson-source-live.qmd`
- `inst/examples/example_course_module/expected/conversion-report.json`
- `inst/examples/example_course_module/run-example.R`

## Validation Commands

Run immediately before submission:

```bash
Rscript -e "testthat::test_local('.')"
Rscript -e "lintr::lint_package()"
R CMD build .
R CMD check --as-cran --no-manual tutorizeR_0.4.4.tar.gz
```

## Remaining Before a Real Submission

- Confirm remote GitHub Actions status.
- Confirm final version number and release tag.
- Confirm archive DOI.
- Confirm ORCID metadata.
- Add classroom-use evidence only if it can be documented in the repository.

