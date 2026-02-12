# Release Checklist

## Pre-release

- [ ] Update `NEWS.md` with user-visible changes.
- [ ] Run `devtools::document()` and verify generated `man/` + `NAMESPACE`.
- [ ] Run tests: `devtools::test()`.
- [ ] Run checks: `R CMD build .` then `R CMD check --as-cran --no-manual tutorizeR_*.tar.gz`.
- [ ] Validate vignettes build cleanly.
- [ ] Ensure README examples still run.

## CI

- [ ] GitHub Actions green on Ubuntu, macOS, and Windows.
- [ ] `lintr` job green.
- [ ] Coverage job green.

## CRAN / r-universe prep

- [ ] Confirm DESCRIPTION metadata, URLs, and license are valid.
- [ ] Remove or justify remaining NOTES.
- [ ] Update `cran-comments.md`.

## Tag and publish

- [ ] Bump version in DESCRIPTION.
- [ ] Commit release changes.
- [ ] Create release tag `vX.Y.Z`.
- [ ] Publish release notes from NEWS entries.
