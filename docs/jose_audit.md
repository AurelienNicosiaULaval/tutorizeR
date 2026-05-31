# tutorizeR JOSE Repository Audit

Audit date: 2026-05-31  
Repository path: `/Users/aureliennicosia/Documents/tutorizeR`

## Evidence sources

This audit is based on repository contents and the current public guidance from Open Journals and R Core:

- Open Journals, 2026. Submitting a paper to JOSE. https://openjournals.readthedocs.io/en/jose/submitting.html
- Open Journals, 2026. Reviewing for JOSE. https://openjournals.readthedocs.io/en/jose/reviewer_guidelines.html
- JOSS, 2026. Review criteria. https://joss.readthedocs.io/en/latest/review_criteria.html
- JOSS, 2026. Submitting a paper to JOSS. https://joss.readthedocs.io/en/latest/submitting.html
- R Core Team, 2026. Writing R Extensions, R version 4.6.0. https://stat.ethz.ch/CRAN/doc/manuals/r-release/R-exts.html

## Scope

The audit inspected the package metadata, source code, tests, vignettes, documentation, continuous integration, paper assets, governance files, templates, and examples currently present in the repository.

## Strengths

- The repository is structured as a conventional R package with `DESCRIPTION`, `NAMESPACE`, `R/`, `man/`, `tests/`, `vignettes/`, and package metadata.
- The package has a clear educational technology focus: converting `.Rmd` and `.qmd` teaching documents into `learnr` or `quarto-live` materials.
- The public API includes conversion, folder processing, validation, linting, question-bank handling, report writing, LMS manifest export, package scaffold export, and RStudio addins.
- Core parser and transform behavior is covered by `testthat` tests with fixtures for `.Rmd`, `.qmd`, MCQ blocks, non-R chunks, duplicate labels, inline chunks, question banks, regression fixtures, exports, validation, and reports.
- GitHub Actions already run multi-platform `R CMD check`, linting, and coverage.
- Governance files already exist: `LICENSE`, `LICENSE.md`, `CONTRIBUTING.md`, `CODE_OF_CONDUCT.md`, `NEWS.md`, `CITATION.cff`, `SECURITY.md`, issue templates, and a pull request template.
- A JOSE/JOSS-style `paper/paper.md` and `paper/paper.bib` are present.
- The package includes teacher-facing documentation through `README.md`, vignettes, templates, and release support documents.
- `DESCRIPTION` declares a standard MIT license and package URLs.

## Weaknesses

- Educational adoption evidence is not directly verifiable from repository contents. The repository supports educational workflows, but it does not currently include anonymized course deployment evidence, classroom evaluation data, instructor testimonials, or external adoption links.
- The README is technically useful but still stronger as developer documentation than as a JOSE reviewer landing page.
- Existing vignettes before this audit were short and did not fully cover the five educational scenarios requested for JOSE positioning.
- `pkgdown` configuration existed but did not yet expose a dedicated JOSE submission section, educational examples section, or visual workflow assets.
- CRAN status is not verifiable from repository contents. The package has CRAN-facing metadata and checks, but no evidence of current CRAN publication was found in the repository.
- The JOSE paper requires careful maintenance of truthful educational use statements. The repository can document intended classroom workflows, but actual deployment in named courses must remain marked as not verifiable unless evidence is added.
- Optional dependencies such as `learnr`, `gradethis`, `quarto-live`, and `tidyverse` affect some example workflows but are not all hard dependencies. This is reasonable for package weight, but reviewer documentation must state when optional packages are required.

## Missing JOSE Requirements

Based on Open Journals guidance, JOSE software submissions should have an open repository, an OSI-approved license, `paper.md`, `paper.bib`, a statement of need, functionality, and recent teaching or learning use where applicable.

Current status:

- Open repository: PARTIAL. Repository URL is present, but public visibility and GitHub history are not verifiable from local repository contents.
- OSI-approved license: READY. MIT license files are present.
- `paper.md`: READY. Present in `paper/paper.md`.
- `paper.bib`: READY. Present in `paper/paper.bib`.
- Statement of Need: READY. Present in `paper/paper.md`, but should emphasize computational teaching and adoption by instructors.
- Functionality: READY. Present in README, man pages, vignettes, and paper.
- Recent use in teaching and learning situations: PARTIAL. Intended use is documented; concrete adoption evidence is not verifiable from repository contents.
- Reviewer-friendly installation and examples: PARTIAL. Installation exists; a dedicated educational example module was missing before this JOSE pass.
- Educational positioning: PARTIAL. The package purpose is educational, but a dedicated use-case document and broader vignettes were needed.

## Missing JOSS Requirements

Based on JOSS review criteria, the repository should include a license, clear documentation, installation instructions, examples, tests, community guidelines, paper, and evidence of scholarly significance.

Current status:

- License file: READY.
- Installation instructions: READY.
- Example usage: READY.
- Tests: READY.
- Automated checks: READY.
- Community guidelines: READY.
- Paper with summary, need, comparison, and references: PARTIAL. Present, but title and JOSE-centered content required revision.
- Development history and open-source practice: PARTIAL. Local repository content cannot verify public timeline, issue activity, or external contributors.
- Scholarly significance outside a single local use case: PARTIAL. The software has credible reuse potential for R-based teaching, but external adoption is not verifiable from repository contents.
- AI usage disclosure: PARTIAL. The paper must be kept current if AI tools are used for code, documentation, or manuscript preparation.

## Missing CRAN Requirements

Based on R Core Team package guidance and standard CRAN checks:

- Standard package structure: READY.
- `DESCRIPTION`: READY, with standard metadata and dependencies.
- `NAMESPACE`: READY.
- Rd documentation: READY.
- Tests: READY.
- Vignettes: READY, with further expansion recommended.
- License declaration: READY.
- `R CMD check --as-cran`: PARTIAL. Prior check artifacts exist, but a fresh check should be run before any release.
- CRAN comments: READY. `cran-comments.md` exists.
- CRAN publication evidence: MISSING. Not verifiable from repository contents.
- Reverse dependency checks: NOT APPLICABLE or not verifiable from repository contents.

## Priority Fixes

1. Create a dedicated JOSE audit, checklist, and submission report.
2. Expand educational documentation with explicit instructor, student, and classroom workflows.
3. Add the five requested vignettes with realistic, pedagogically motivated scenarios.
4. Add a complete teaching demonstration under `inst/examples/example_course_module/`.
5. Add explicit tests for `gradethis` setup generation, `quarto-live` output, MCQ reference error handling, and publication assets.
6. Update GitHub Actions with a separate test job in addition to `R CMD check` and coverage.
7. Update `_pkgdown.yml` with JOSE submission and educational examples navigation.
8. Add visual workflow and screenshot assets for README/pkgdown.
9. Revise `paper/paper.md` with the exact JOSE-oriented title and sections requested in the prompt.
10. Keep all educational adoption claims limited to what is verifiable from repository contents.

