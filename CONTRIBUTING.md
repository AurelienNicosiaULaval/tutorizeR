# Contributing to tutorizeR

## Development setup

1. Fork and clone the repository.
2. Install dependencies:

```r
install.packages(c("devtools", "roxygen2", "testthat", "lintr"))
```

3. Run checks locally:

```r
devtools::document()
devtools::test()
```

```bash
R CMD build .
R CMD check --as-cran --no-manual tutorizeR_*.tar.gz
```

## Pull request expectations

- Keep PRs small and atomic.
- Add or update tests for every behavior change.
- Update roxygen docs and NEWS entry.
- Include a minimal reproducible example for new features.

## Style

- Use clear teacher-facing error messages.
- Prefer explicit validation and defensive checks.
- Follow tidyverse-style naming and `testthat` edition 3 conventions.
