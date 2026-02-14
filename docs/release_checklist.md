# Release Checklist

## Pre-release

- [x] Run `devtools::document()` and verify generated `man/` + `NAMESPACE`.
- [x] Run tests: `devtools::test()` (98 tests passed).
- [x] Build and run CRAN-style check on a clean tarball: 
  - `R CMD build .`
  - `R CMD check --as-cran --no-manual tutorizeR_*.tar.gz`
- [x] Confirm check status: 0 ERROR, 0 WARNING, 1 NOTE (`New submission`).
- [x] Confirm `.gitignore` excludes build/check artifacts (`.Rcheck`, `tutorizeR_*.tar.gz`, `tutorizeR.Rcheck`, `..Rcheck`).
- [x] Validate README reproducibility section commands:
  - `lintr::lint_package()`
  - `devtools::test()`
  - `R CMD build .`
  - `R CMD check --as-cran --no-manual`
- [x] Run smoke conversion on fixtures (`3` Rmd + `3` qmd): `tutorize(..., format = "learnr")`.
- [x] Run conversion smoke on manifest: output renders without error (`render = OK`).

## CI

- [x] GitHub Actions multi-OS check (`ubuntu-latest`, `macos-latest`, `windows-latest`) passing.
- [x] Lintr job clean.
- [x] Coverage job passes with core coverage `>= 80%` (current core `91.07%`, total `68.98%`).

## CRAN / r-universe / JOSS prep

- [x] Update `DESCRIPTION` metadata (`Version: 0.4.3`, URL, BugReports).
- [x] Update `CITATION.cff`, `codemeta.json`, `NEWS.md`.
- [x] Finalize JOSS manuscript in `paper/paper.md` (Statement of Need, functionality, QA, availability, limitations, references).
- [x] Add reviewer-facing reproducibility path in `README`.
- [x] Confirm `cran-comments.md` and downstream notes remain synchronized with latest run (`cran-comments.md`, `README` smoke command).
- [x] Confirm README/release references reflect current release (`0.4.3`).
- [x] Validate JOSS scope/significance gate:
  - repository age check (`git log --since='6 months ago' --oneline`) includes sustained public development,
  - public issue/PR workflows and contribution templates exist (`.github/ISSUE_TEMPLATE`, `.github/PULL_REQUEST_TEMPLATE.md`),
  - no "thin utility/one-off" framing in manuscript and docs,
  - `paper/paper.md` includes explicit AI usage disclosure if applicable,
  - reproducibility bundle present (`docs/joss_release_bundle.md`).

## Tag and publish

- [x] Bump package version (`0.4.3`).
- [x] Create git tag (`v0.4.3`).
- [x] Open PR against `main` from the release branch and confirm it is merged.
- [x] Draft GitHub release notes from NEWS and include tarball/reproduction artifacts.
- [x] Close JOSS checklist items in `paper/paper.md` and archive build command outputs (`docs/joss_release_bundle`).
