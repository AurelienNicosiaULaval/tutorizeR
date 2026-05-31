# JOSE Review Checklist

Checklist date: 2026-05-31

Status labels:

- READY: evidence is present in the repository.
- PARTIAL: evidence is present but incomplete or not externally verifiable.
- MISSING: no evidence was found in the repository.

## Checklist

| Item | Status | Repository evidence | Notes |
|---|---:|---|---|
| Open source license | READY | `LICENSE`, `LICENSE.md`, `DESCRIPTION` | MIT license present. |
| Open repository | PARTIAL | `DESCRIPTION` URL | Public visibility is not verifiable from local contents. |
| Educational contribution | READY | README, vignettes, `docs/educational_use_cases.md`, `paper/paper.md` | Focus is educational technology for R-based teaching. |
| Installation instructions | READY | `README.md` | GitHub and r-universe instructions present. |
| Basic usage examples | READY | `README.md`, vignettes, `inst/examples/` | Includes single-file and folder conversion workflows. |
| Tests | READY | `tests/testthat/` | Parser, conversion, MCQ, question-bank, export, validation, regression, and publication readiness tests. |
| Continuous integration | READY | `.github/workflows/r.yml` | R CMD check, tests, lint, and coverage jobs configured. |
| Coverage reporting | READY | `.github/workflows/r.yml` | `covr::package_coverage()` configured with core threshold. |
| Documentation | READY | `README.md`, `man/`, vignettes, docs | Reviewer-facing docs added. |
| Vignettes | READY | `vignettes/` | Includes JOSE-oriented educational vignettes, including a teaching workflow case study. |
| Reproducibility | READY | examples, tests, workflow, local dataset | Source-first conversion workflow documented. |
| Educational adoption evidence | PARTIAL | package examples and use cases | Actual classroom adoption is not verifiable from repository contents. |
| JOSE paper | READY | `paper/paper.md` | Title and required sections present. |
| Bibliography | READY | `paper/paper.bib` | Key references present. |
| Code of conduct | READY | `CODE_OF_CONDUCT.md` | Present. |
| Contribution guide | READY | `CONTRIBUTING.md` | Present. |
| Citation metadata | READY | `CITATION.cff`, `CITATION` | Present. |
| Issue templates | READY | `.github/ISSUE_TEMPLATE/` | Bug, feature, and JOSE readiness templates present. |
| Pull request template | READY | `.github/PULL_REQUEST_TEMPLATE.md` | Present and expanded. |
| CRAN readiness evidence | READY | `cran-comments.md`, tests, DESCRIPTION | Clean temporary source check on 2026-05-31 returned Status: 1 NOTE for new submission. |
| Screenshots and workflow visuals | READY | `man/figures/` | Workflow diagram and report screenshot asset present. |

## Remaining JOSE Blockers

- Public GitHub history, issue activity, and repository visibility are not verifiable from local contents.
- Actual classroom adoption is not verifiable from repository contents.
- The clean check should be rerun immediately before submission because package and system state can change.

## Recommended Pre-Submission Commands

```bash
Rscript -e "testthat::test_local()"
Rscript -e "lintr::lint_package()"
R CMD build .
R CMD check --as-cran --no-manual tutorizeR_*.tar.gz
```
