# JOSS submission bundle (v0.4.3)

## Reproducibility log

- Date (UTC): 2026-02-13
- Commit: `b9fde3a` (`feat/max-potentiel-enseignant`)
- Tag: `v0.4.3`
- Branch: `feature/bring-v0.4-into-main`

## Commands run

```bash
Rscript -e "lintr::lint_package()"
Rscript -e "devtools::test()"
R CMD build .
R CMD check --as-cran --no-manual tutorizeR_0.4.3.tar.gz
```

## Expected outcomes recorded

- `lintr::lint_package()` → 0 lints
- `devtools::test()` → 98 tests pass
- `R CMD check --as-cran --no-manual` → 0 errors, 0 warnings, 1 note (`New submission`)
- Core coverage check in CI → 91.07% (overall 68.98%)
- Scope/significance evidence:
  - `git log --since='6 months ago' --oneline` returns sustained commits and tags,
  - public issues/PR workflow exists in GitHub templates,
  - `paper/paper.md` contains AI usage disclosure.

## Smoke conversion evidence

Conversion smoke run performed on 6 fixtures (3 Rmd + 3 qmd):

- `tests/testthat/fixtures/rmd/basic_code.Rmd`
- `tests/testthat/fixtures/rmd/non_r_fence.Rmd`
- `tests/testthat/fixtures/rmd/setup_and_options.Rmd`
- `tests/testthat/fixtures/qmd/basic_code.qmd`
- `tests/testthat/fixtures/qmd/with_mcq_block.qmd`
- `tests/testthat/fixtures/qmd/with_mcq_ref.qmd`

All conversions completed with `render = OK`.
