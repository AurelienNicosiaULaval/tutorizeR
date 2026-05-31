# JOSE Submission Report

Report date: 2026-05-31  
Repository: tutorizeR

## Evaluation Basis

This report evaluates only files and evidence present in the repository. It does not assume public GitHub history, external classroom adoption, CRAN publication, or journal acceptance likelihood unless those facts are verifiable from repository contents.

## Current Readiness Score

Current readiness score: 86 / 100

Rationale:

- strong package structure, tests, documentation, governance files, and manuscript assets;
- clear educational infrastructure purpose;
- remaining uncertainty around external adoption evidence, public repository history, and CRAN status.

## JOSE Readiness Score

JOSE readiness score: 87 / 100

Strengths:

- educational technology scope is clear;
- `paper/paper.md` and `paper/paper.bib` are present;
- educational use cases and JOSE-oriented vignettes are present;
- example course module demonstrates instructor adoption workflow;
- governance, contribution, issue, and PR templates are present.

Main limitations:

- actual classroom adoption is not verifiable from repository contents;
- public repository history and public review readiness are not verifiable from local contents;
- the final submission should rerun test, lint, coverage, and R CMD check records immediately before upload.

## JOSS Readiness Score

JOSS readiness score: 76 / 100

Strengths:

- standard open-source software package structure;
- tests, CI, coverage, documentation, and paper assets are present;
- installation and usage instructions are available;
- source code has a clear reusable workflow.

Main limitations:

- JOSS significance outside an educational technology context is less direct than JOSE;
- external research adoption and public development history are not verifiable from repository contents;
- JOSS may be secondary because the project is primarily educational infrastructure.

## CRAN Readiness Score

CRAN readiness score: 83 / 100

Strengths:

- conventional R package structure;
- documented functions and exported namespace;
- tests and vignettes exist;
- MIT license declared;
- `cran-comments.md` exists.

Main limitations:

- current CRAN publication is not verifiable from repository contents;
- a clean temporary source copy produced `R CMD check --as-cran --no-manual` Status: 1 NOTE on 2026-05-31;
- optional teaching dependencies should remain clearly documented so examples do not imply unavailable hard dependencies.

## Verification Performed

On 2026-05-31, the following checks were performed locally:

- Full `testthat` suite: PASS.
- `lintr::lint_package()`: PASS, no lints found.
- Thirteen vignettes rendered to a temporary output directory: PASS.
- `_pkgdown.yml` parsed with `yaml::read_yaml()`: PASS.
- Paper rendered with bibliography resolution: PASS.
- Coverage gate script: PASS, 71.58% total coverage and 93.36% core coverage.
- Clean temporary source copy built with `R CMD build`: PASS.
- `R CMD check --as-cran --no-manual`: PASS with 1 NOTE for new submission.

Direct `R CMD build .` from the live Git checkout was attempted but interrupted because R spent too long copying `.git` into the temporary build directory before applying package build exclusions. Building from a clean source copy without `.git`, `.Rcheck`, or local tarballs succeeded.

## Remaining Blockers

- Not verifiable from repository contents: actual classroom adoption.
- Not verifiable from repository contents: public GitHub issue and pull request activity.
- Not verifiable from repository contents: current CRAN publication.
- Not verifiable from repository contents: current Zenodo release state beyond local README/CFF metadata.
- A fresh full check should be rerun immediately before submission.

## Recommended Next Actions

1. Run tests, linting, coverage, and `R CMD check --as-cran --no-manual` on a clean checkout.
2. Confirm that GitHub Actions pass on the remote repository.
3. Add anonymized, permission-safe teaching evidence if available.
4. Confirm that all paper metadata is final, especially ORCID and release DOI.
5. Create a tagged release before submission.
6. Confirm that `paper/paper.md` truthfully discloses any AI-assisted work if applicable.
7. Submit to JOSE first, with JOSS as a secondary route only if JOSE scope is not pursued.

Recommendation: Not ready for JOSE submission until remote CI is verified and the maintainer decides whether to add documented classroom-use evidence.

## Estimated Probability of Acceptance

JOSE: Not verifiable from repository contents.

JOSS: Not verifiable from repository contents.

No acceptance probability is assigned because acceptance depends on editor scope decisions, reviewer assessment, public repository state, external evidence, and responses during review.
