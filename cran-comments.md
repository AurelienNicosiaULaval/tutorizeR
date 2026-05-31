## Test environments

- Local macOS (aarch64), R 4.5.0
- GitHub Actions: Ubuntu, macOS, Windows (release)

## R CMD check results

- 0 errors
- 0 warnings
- 2 notes on local `R CMD check --as-cran`:
  - New submission.
  - HTML manual validation was skipped because the local `tidy` executable is
    not recent enough. This is a local toolchain note.

## Downstream impact

- No known downstream package dependencies currently affected.

## Notes

- This is a new submission.
- `convert_to_tutorial()` remains available for backward compatibility.
- New canonical API is `tutorize()`.
- Generated `learnr` tutorial scaffolds activate `gradethis` setup only when
  `gradethis` is installed. `gradethis` is not required to load, test, or run
  the package.
