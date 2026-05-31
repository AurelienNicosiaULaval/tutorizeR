# Educational Use Evidence

Date: 2026-05-31

This document separates demonstrated repository evidence from plausible but unevaluated educational claims.

## What Is Currently Demonstrated by the Repository

The repository demonstrates:

- an R package that converts `.Rmd` and `.qmd` teaching documents into `learnr` or `quarto-live` resources;
- parser, transformation, validation, linting, question-bank, reporting, export, and addin functionality;
- test fixtures covering conversion, MCQ handling, question-bank validation, reports, exports, and edge cases;
- an installable example module in `inst/examples/example_course_module/`;
- vignettes describing instructor workflows and limitations;
- reviewer-facing JOSE documentation;
- open-source governance files, including contribution guide and code of conduct.

## What Is Plausible but Not Yet Formally Evaluated

It is plausible that `tutorizeR` may help instructors:

- reduce manual duplication between source lessons and tutorials;
- maintain reproducible teaching materials more consistently;
- create more frequent formative practice opportunities;
- review generated learning resources with teaching assistants;
- adapt source-first workflows across multiple course modules.

These are workflow plausibility statements, not measured educational outcomes.

Formal learning-outcome evaluation: Not verifiable from repository contents.

## What Is Not Claimed

The repository does not claim:

- improved student grades;
- improved student engagement;
- improved learning outcomes;
- reduced teaching time measured in controlled conditions;
- documented classroom deployment;
- broad adoption by external instructors;
- JOSE submission, review, or acceptance;
- CRAN publication.

## Evidence Available to Reviewers

Reviewers can inspect:

- `inst/examples/example_course_module/README.md`;
- `inst/examples/example_course_module/run-example.R`;
- `vignettes/teaching-workflow-scenario.Rmd`;
- `tests/testthat/test-examples.R`;
- `tests/testthat/test-jose-readiness.R`;
- `paper/paper.md`;
- `docs/jose_review_checklist.md`;
- `docs/jose_submission_guide.md`.

## Evidence Still Missing Before a Stronger JOSE Submission

- Actual classroom deployment: Not verifiable from repository contents.
- Instructor adoption outside the maintainer: Not verifiable from repository contents.
- Formal student feedback: Not verifiable from repository contents.
- Student outcome data: Not verifiable from repository contents.
- Teaching assistant workflow evidence: Not verifiable from repository contents.
- Long-term use across multiple cohorts: Not verifiable from repository contents.

## Claims that must not be made

- Do not claim improved student grades unless measured.
- Do not claim improved engagement unless measured.
- Do not claim classroom deployment unless documented.
- Do not claim broad adoption unless usage evidence exists.
- Do not claim JOSE readiness solely because R CMD check passes.
- Do not claim direct LMS publishing, because current support is manifest-oriented.
- Do not claim current CRAN availability unless verified.
