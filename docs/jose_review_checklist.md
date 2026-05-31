# JOSE Review Checklist

Date: 2026-05-31

Status labels:

- READY: supported by repository evidence.
- PARTIAL: partially supported, but incomplete.
- MISSING: expected evidence is absent.
- NOT VERIFIABLE: cannot be verified from repository contents.

Official JOSE checklist source: https://openjournals.readthedocs.io/en/jose/review_checklist.html

## License

| Item | Status | Evidence |
|---|---|---|
| Code has an OSI-approved license. | READY | `DESCRIPTION`, `LICENSE`, `LICENSE.md`. |
| Educational content license is clear. | READY | `LICENSE-CONTENT.md`, README, example README. |
| Repository is public. | NOT VERIFIABLE | Public visibility cannot be proven from local files alone. |

## Statement of Need

| Item | Status | Evidence |
|---|---|---|
| Paper explains the educational need. | READY | `paper/paper.md`. |
| Paper explains why software is needed. | READY | `paper/paper.md`. |
| Paper avoids being API documentation. | READY | Detailed API remains in README, vignettes, and man pages. |

## Community Guidelines

| Item | Status | Evidence |
|---|---|---|
| Contribution guide exists. | READY | `CONTRIBUTING.md`. |
| Code of conduct exists. | READY | `CODE_OF_CONDUCT.md`. |
| Issue and PR templates exist. | READY | `.github/ISSUE_TEMPLATE/`, `.github/PULL_REQUEST_TEMPLATE.md`. |

## Documentation

| Item | Status | Evidence |
|---|---|---|
| Installation instructions exist. | READY | `README.md`. |
| User workflow is documented. | READY | `README.md`, vignettes. |
| Example module is documented. | READY | `inst/examples/example_course_module/README.md`. |
| Limitations are documented. | READY | README, paper, `docs/educational_use_evidence.md`. |

## Tests

| Item | Status | Evidence |
|---|---|---|
| Automated tests exist. | READY | `tests/testthat/`. |
| Example module tests exist. | READY | `tests/testthat/test-examples.R`. |
| Local tests pass. | READY | `testthat::test_local('.')` passed locally with 144 tests on 2026-05-31. |
| Remote CI passes. | READY | GitHub Actions run `26715564901` passed on PR #5 for commit `864c489`. |

## Examples

| Item | Status | Evidence |
|---|---|---|
| Installable example exists. | READY | `inst/examples/example_course_module/`. |
| Example includes source lesson. | READY | `lesson-source.qmd`. |
| Example includes question bank. | READY | `question-bank/questions.yml`. |
| Example includes expected outputs. | READY | `expected/`. |
| Example can be run with installed package. | PARTIAL | `run-example.R` exists; final run should be verified after package install. |

## Functionality

| Item | Status | Evidence |
|---|---|---|
| Feature-complete conversion workflow exists. | READY | `R/`, tests, examples. |
| `.Rmd` and `.qmd` inputs supported. | READY | tests and vignettes. |
| `learnr` and `quarto-live` targets supported. | READY | tests and examples. |
| Question bank support exists. | READY | `R/question_bank.R`, tests, example bank. |
| LMS direct publishing exists. | MISSING | Current export is manifest-only. This is documented as a limitation. |

## Educational Contribution

| Item | Status | Evidence |
|---|---|---|
| Educational technology contribution is explained. | READY | README, paper, docs. |
| Teaching workflow is demonstrated. | READY | example module and `teaching-workflow-case-study` vignette. |
| Learning-outcome evidence exists. | NOT VERIFIABLE | Formal evaluation not present. |

## Adoption by Instructors

| Item | Status | Evidence |
|---|---|---|
| Adoption workflow for other instructors is documented. | READY | README, example README, vignettes. |
| Actual classroom deployment is documented. | NOT VERIFIABLE | No repository evidence. |
| Broad external adoption is documented. | NOT VERIFIABLE | No repository evidence. |

## Paper Readiness

| Item | Status | Evidence |
|---|---|---|
| JOSE-oriented paper exists. | READY | `paper/paper.md`. |
| Bibliography exists. | READY | `paper/paper.bib`. |
| Pedagogical references are cited. | READY | paper and bibliography. |
| AI usage disclosure is cautious. | READY | `paper/paper.md`. |
| Final metadata is complete. | PARTIAL | ORCID and final release DOI should be confirmed. |
