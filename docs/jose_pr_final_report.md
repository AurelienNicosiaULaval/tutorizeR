# JOSE PR Final Report

Date: 2026-05-31  
Branch: `review/jose-readiness`  
PR: https://github.com/AurelienNicosiaULaval/tutorizeR/pull/5

## What Was Changed

- Recentered the README on JOSE as the primary target and moved JOSS to a secondary-target section.
- Rewrote `paper/paper.md` as a JOSE-oriented software paper, not API documentation.
- Added pedagogical references to `paper/paper.bib` and cited them in the paper.
- Added cautious AI usage disclosure.
- Added explicit educational evidence boundaries.
- Rebuilt the example module as an installable teaching example with source lesson, question bank, expected outputs, JSON report, and run script.
- Added a long teaching workflow case-study vignette.
- Expanded core vignettes with user goals, minimal examples, realistic examples, limitations, and reproducibility checks.
- Added CC-BY 4.0 content licensing for educational examples and figures.
- Added example-module tests and README fixture-path checks.
- Updated GitHub Actions coverage logic to use named coverage values and run on PRs targeting `release/v0.4.4`.

## Why Each Change Matters for JOSE

- JOSE evaluates educational contribution, not only package mechanics.
- The paper now explains the teaching problem, educational infrastructure role, adoption workflow, and limitations.
- The example module gives reviewers an installable artifact they can run without relying on internal test fixtures.
- The evidence document prevents overclaiming about student outcomes or classroom adoption.
- The review checklist maps repository evidence to JOSE-style review concerns.
- The content license clarifies reuse rights for teaching examples and figures.

## Factual Claims Corrected

- Removed framing that treated the branch primarily as JOSS preparation.
- Avoided claims of classroom deployment, improved engagement, improved grades, or improved learning outcomes.
- Replaced any categorical AI non-use framing with a cautious disclosure.
- Clarified that LMS support is manifest-oriented, not direct publishing.
- Clarified that CRAN publication is not claimed.
- Clarified that JOSE submission, review, or acceptance is not claimed.

## Educational Evidence Added

- `docs/educational_use_evidence.md`
- `inst/examples/example_course_module/`
- `vignettes/teaching-workflow-case-study.Rmd`
- Expanded getting started, question-bank, conversion, assessment, and lint/debug vignettes.

## Remaining Limitations

- Formal learning-outcome evaluation has not yet been conducted, unless evidence is added to the repository.
- Actual classroom deployment is not verifiable from repository contents.
- Broad external instructor adoption is not verifiable from repository contents.
- Remote GitHub Actions passed on PR #5 for commit `864c489`.
- ORCID and final release DOI should be confirmed before submission.

## Validation Commands Run

Official JOSE pages reviewed:

- https://openjournals.readthedocs.io/en/jose/submitting.html
- https://openjournals.readthedocs.io/en/jose/review_criteria.html
- https://openjournals.readthedocs.io/en/jose/review_checklist.html

Local validation:

```bash
Rscript -e "testthat::test_local('.')"
```

Result: PASS. 144 tests passed, 0 failures, 0 warnings, 0 skips.

```bash
Rscript -e "lintr::lint_package()"
```

Result: PASS. No lints found.

```bash
Rscript -e "yaml::read_yaml('_pkgdown.yml')"
```

Result: PASS. `_pkgdown.yml` parsed successfully.

```bash
Rscript -e "rmarkdown::render('paper/paper.md', output_format = rmarkdown::md_document(variant = 'gfm'), output_file = tempfile(fileext = '.md'), quiet = TRUE)"
```

Result: PASS. Paper rendered successfully and bibliography citations resolved.

```bash
Rscript - <<'EOF'
files <- list.files('vignettes', pattern = '\\.[Rr]md$', full.names = TRUE)
out_dir <- tempfile('tutorizer-vignettes-')
dir.create(out_dir)
for (file in files) {
  rmarkdown::render(file, output_dir = out_dir, quiet = TRUE)
}
EOF
```

Result: PASS. 13 vignettes rendered successfully.

```bash
Rscript -e "source(system.file('examples', 'example_course_module', 'run-example.R', package = 'tutorizeR'))"
```

Result: PASS after `pkgload::load_all('.')` in the local checkout.

```bash
Rscript - <<'EOF'
cov <- covr::package_coverage(quiet = TRUE)
cov_list <- covr::coverage_to_list(cov)
file_coverage <- cov_list$filecoverage
core_files <- c('R/parser.R', 'R/transform.R', 'R/validation.R', 'R/report.R')
missing <- setdiff(core_files, names(file_coverage))
if (length(missing) > 0L) stop(sprintf('Missing core files: %s', paste(missing, collapse = ', ')))
core_pct <- mean(unname(file_coverage[core_files]))
total_pct <- as.numeric(cov_list$totalcoverage)
cat(sprintf('Coverage total: %.2f%%\n', total_pct))
cat(sprintf('Coverage core: %.2f%%\n', core_pct))
if (core_pct < 80) stop(sprintf('Core coverage threshold not met: %.2f%% < 80%%', core_pct))
EOF
```

Result: PASS. Coverage total: 71.58%. Coverage core: 93.36%.

```bash
R CMD build .
```

Result: attempted directly in the live Git checkout, but interrupted after the command spent too long copying `.git` into the temporary build directory before R package exclusions were applied. This is a local checkout issue, not a package source issue.

Clean source-copy validation:

```bash
tmpdir=$(mktemp -d)
rsync -a --exclude='.git' --exclude='*.Rcheck' --exclude='..Rcheck' --exclude='*.tar.gz' ./ "$tmpdir/tutorizeR/"
cd "$tmpdir/tutorizeR"
R CMD build .
R CMD check --as-cran --no-manual tutorizeR_0.4.4.tar.gz
```

Result: PASS with 1 NOTE for new submission.

## CI Status

GitHub Actions were checked on PR #5 after pushing commit `864c489`.

Run: https://github.com/AurelienNicosiaULaval/tutorizeR/actions/runs/26715564901

Results:

- `coverage`: PASS, 2m21s.
- `lintr`: PASS, 2m24s.
- `testthat`: PASS, 2m36s.
- `r-cmd-check (ubuntu-latest, release)`: PASS, 3m3s.
- `r-cmd-check (macos-latest, release)`: PASS, 3m34s.
- `r-cmd-check (windows-latest, release)`: PASS, 4m26s.

## Files Modified

Key changed or added files:

- `README.md`
- `paper/paper.md`
- `paper/paper.bib`
- `LICENSE-CONTENT.md`
- `_pkgdown.yml`
- `.github/workflows/r.yml`
- `docs/jose_blockers_report.md`
- `docs/jose_submission_guide.md`
- `docs/jose_review_checklist.md`
- `docs/educational_use_evidence.md`
- `docs/jose_release_bundle.md`
- `docs/jose_pr_final_report.md`
- `inst/examples/example_course_module/`
- `tests/testthat/test-examples.R`
- `tests/testthat/test-jose-readiness.R`
- `vignettes/teaching-workflow-case-study.Rmd`
- `vignettes/getting-started.Rmd`
- `vignettes/question-bank.Rmd`
- `vignettes/conversion-rmd-vs-qmd.Rmd`
- `vignettes/mcq-and-assessment.Rmd`
- `vignettes/lint-and-debug.Rmd`

## Remaining Blockers Before Actual JOSE Submission

- Documented classroom deployment: Not verifiable from repository contents.
- Formal learning-outcome evaluation: Not verifiable from repository contents.
- Final release DOI and ORCID metadata should be confirmed.

Recommendation: Not ready for JOSE submission.

The repository is substantially more JOSE-ready, but a real submission should wait until the maintainer confirms final release metadata and decides whether to add documented classroom-use evidence.
