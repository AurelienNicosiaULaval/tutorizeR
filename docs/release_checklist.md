# Release Checklist

## Pre-release

- [ ] Run `devtools::document()` and verify generated `man/` + `NAMESPACE`.
- [ ] Run tests: `devtools::test()`.
- [ ] Run checks on a clean tarball: 
  - `R CMD build .`
  - `R CMD check --as-cran --no-manual tutorizeR_*.tar.gz`
- [ ] Confirm coverage command (core target ≥ 80%): `Rscript -e 'covr::package_coverage()'` or CI.
- [ ] Ensure vignettes build cleanly: `R CMD build .` (vignettes included) and check log.
- [ ] Validate README commands compile as part of CI smoke checks (install and convert example).

## CI

- [ ] GitHub Actions green on Ubuntu, macOS, and Windows.
- [ ] `lintr` job green.
- [ ] Coverage job green.

## CRAN / r-universe prep

- [ ] Confirm DESCRIPTION metadata, URLs, and license are valid.
- [ ] Remove or justify remaining CRAN NOTES (target: only allowed “New submission” NOTE if applicable).
- [ ] Update `cran-comments.md` with latest test environments and check summary.

## Tag and publish

- [ ] Bump version in DESCRIPTION.
- [ ] Commit release changes.
- [ ] Create release tag `vX.Y.Z`.
- [ ] Publish release notes from NEWS entries.
