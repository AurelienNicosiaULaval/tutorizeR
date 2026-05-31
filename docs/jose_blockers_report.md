# JOSE Blockers Report

Date: 2026-05-31  
Branch: `review/jose-readiness`  
PR: https://github.com/AurelienNicosiaULaval/tutorizeR/pull/5

## Executive Summary

The repository is a credible candidate for a future JOSE software submission because it is open-source educational infrastructure for converting reproducible R Markdown and Quarto teaching materials into interactive tutorials. The main weakness before this pass was that the PR still read like a JOSS/CRAN readiness pass rather than a JOSE submission package. The central corrections are to make the educational contribution explicit, avoid unverified pedagogical claims, provide an installable teaching example, and document the evidence boundary.

Sources consulted:

- Open Journals. JOSE submitting guide. https://openjournals.readthedocs.io/en/jose/submitting.html
- Open Journals. JOSE review criteria. https://openjournals.readthedocs.io/en/jose/review_criteria.html
- Open Journals. JOSE review checklist. https://openjournals.readthedocs.io/en/jose/review_checklist.html

## Blocking Issues for JOSE

| Severity | Issue | Status | Required action |
|---|---|---|---|
| BLOCKER | JOSE framing was mixed with JOSS framing. | Addressed in PR | README, paper, and docs now make JOSE the primary target and keep JOSS secondary. |
| BLOCKER | Paper needed to explain JOSE eligibility and educational contribution rather than list API details. | Addressed in PR | `paper/paper.md` rewritten as a JOSE-oriented paper. |
| BLOCKER | Educational evidence boundary was not explicit enough. | Addressed in PR | `docs/educational_use_evidence.md` states what is demonstrated, plausible, not claimed, and missing. |
| BLOCKER | Example module was not in the expected installable structure. | Addressed in PR | `inst/examples/example_course_module/` now includes source, question bank, expected outputs, report, and run script. |
| MAJOR | README smoke tests used internal test fixtures. | Addressed in PR | README now uses `system.file("examples", "example_course_module", package = "tutorizeR")`. |
| MAJOR | Content and figure licensing was not explicit. | Addressed in PR | Added `LICENSE-CONTENT.md` and README/example licensing notes. |
| MAJOR | AI usage statement needed cautious wording. | Addressed in PR | Paper now states that generative AI supported planning, review, or documentation-support stages. |
| MAJOR | Need final verification before submission. | Remaining | Rerun tests, lint, source build, R CMD check, and remote CI immediately before an actual JOSE submission. |

## Factual Risks

| Severity | Risk | Required wording |
|---|---|---|
| BLOCKER | Claiming classroom deployment without evidence. | `Not verifiable from repository contents.` |
| BLOCKER | Claiming improved learning outcomes, grades, or engagement without measured evidence. | Do not claim. |
| MAJOR | Claiming JOSE submission, review, or acceptance. | Do not claim until it happens and is documented. |
| MAJOR | Claiming CRAN publication. | Do not claim unless CRAN publication is verified. |
| MAJOR | Claiming CI is green without checking remote Actions. | Addressed during this review; rerun on final release commit. |

## Package Quality Risks

| Severity | Risk | Status |
|---|---|---|
| MAJOR | Example conversion can fail if user examples depend on working-directory-relative data. | Addressed through `run-example.R`, which copies files to a temporary working directory. |
| MAJOR | Coverage job used a fragile `covr::coverage_to_list()` data-frame conversion. | Addressed with direct access to named `filecoverage` values. |
| MINOR | `R CMD build .` can be slow or fail in the live repository because R copies `.git` before applying exclusions. | Documented in final report; clean source copy build succeeds. |
| MINOR | Optional packages `learnr`, `gradethis`, `quarto-live`, and `tidyverse` affect examples. | Documented in README and example files. |

## Documentation Gaps

| Severity | Gap | Status |
|---|---|---|
| MAJOR | Dedicated JOSE submission guide. | Addressed in `docs/jose_submission_guide.md`. |
| MAJOR | Dedicated JOSE review checklist with official categories. | Addressed in `docs/jose_review_checklist.md`. |
| MAJOR | Educational evidence and non-claims document. | Addressed in `docs/educational_use_evidence.md`. |
| MAJOR | Teaching workflow case study vignette. | Addressed in `vignettes/teaching-workflow-case-study.Rmd`. |
| MINOR | Existing vignettes were concise. | Expanded with goals, examples, realistic workflow notes, limits, and reproducibility commands. |

## Evidence Gaps

| Severity | Gap | Current evidence |
|---|---|---|
| BLOCKER | Formal learning-outcome evaluation. | Not verifiable from repository contents. |
| BLOCKER | Documented classroom deployment. | Not verifiable from repository contents. |
| MAJOR | External instructor adoption. | Not verifiable from repository contents. |
| MAJOR | Remote CI status after this commit. | Must be verified after push. |
| MINOR | Long-term maintenance history after release. | Partly visible in Git history, but public activity should be checked on GitHub. |

## Exact Files to Change

Files addressed in this PR:

- `README.md`
- `paper/paper.md`
- `paper/paper.bib`
- `_pkgdown.yml`
- `.github/workflows/r.yml`
- `tests/testthat/test-examples.R`
- `tests/testthat/test-jose-readiness.R`
- `inst/examples/example_course_module/`
- `vignettes/teaching-workflow-case-study.Rmd`
- `vignettes/getting-started.Rmd`
- `vignettes/question-bank.Rmd`
- `vignettes/conversion-rmd-vs-qmd.Rmd`
- `vignettes/mcq-and-assessment.Rmd`
- `vignettes/lint-and-debug.Rmd`
- `docs/jose_submission_guide.md`
- `docs/jose_release_bundle.md`
- `docs/jose_review_checklist.md`
- `docs/educational_use_evidence.md`
- `docs/jose_pr_final_report.md`
- `LICENSE-CONTENT.md`

## Final Checklist Before Submission

- [x] Confirm remote GitHub Actions pass on PR #5 during this review.
- [ ] Rerun `Rscript -e "testthat::test_local('.')"` on a clean checkout.
- [ ] Rerun `Rscript -e "lintr::lint_package()"`.
- [ ] Rerun source build from a clean source copy or another checkout that does not copy `.git`.
- [ ] Rerun `R CMD check --as-cran --no-manual`.
- [ ] Confirm all paper references are cited and BibTeX parses.
- [ ] Confirm the example module runs after package installation.
- [ ] Add documented classroom use only if evidence can be added to the repository.
- [ ] Do not submit until factual claims have been reviewed by the maintainer.
