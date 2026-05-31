## Test environments

- Local macOS (aarch64), R 4.5.0
- GitHub Actions: Ubuntu, macOS, Windows (release)

## R CMD check results

- 0 errors
- 0 warnings
- 2 notes on local `R CMD check --as-cran`:
  - New submission. The same incoming-feasibility note records that
    `gradethis` is listed in `Suggests` and is available through
    `Additional_repositories: https://rstudio.r-universe.dev`.
  - HTML manual validation was skipped because the local `tidy` executable is
    not recent enough. This is a local toolchain note.

## Downstream impact

- No known downstream package dependencies currently affected.

## Notes

- This is a new submission.
- `convert_to_tutorial()` remains available for backward compatibility.
- New canonical API is `tutorize()`.
- `gradethis` is suggested because generated `learnr` tutorial scaffolds
  include `gradethis` setup code for teacher-authored feedback checks. It is
  not required to load the package.
