<p align="center">
  <img src="man/figures/logo.png" alt="tutorizeR hex sticker" height="180">
</p>

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.17771142.svg)](https://doi.org/10.5281/zenodo.17771142)
[![R-CMD-check](https://github.com/AurelienNicosiaULaval/tutorizeR/actions/workflows/r.yml/badge.svg)](https://github.com/AurelienNicosiaULaval/tutorizeR/actions/workflows/r.yml)

# tutorizeR

`tutorizeR` helps teachers convert `.Rmd` and `.qmd` material into interactive
`learnr` or `quarto-live` resources with linting, reusable MCQ banks, and
export/report tooling.

## Installation

```r
# install.packages("remotes")
remotes::install_github("AurelienNicosiaULaval/tutorizeR")
```

To use `r-universe`:

```r
install.packages(
  "tutorizeR",
  repos = c(
    "https://aureliennicosiaulaval.r-universe.dev",
    "https://cloud.r-project.org"
  )
)
```

## End-to-end workflow

```r
library(tutorizeR)

# 1) Load reusable question bank
qb <- load_question_bank("inst/question-bank")

# 2) Lint source before conversion
lint <- lint_source("lesson.qmd", question_bank = qb, strict = FALSE)
print(lint)

# 3) Convert with mixed MCQ strategy (inline + bank)
rep <- tutorize(
  input = "lesson.qmd",
  format = "learnr",
  assessment = "both",
  question_bank = qb,
  mcq_source = "mixed",
  lint_strict = TRUE,
  overwrite = TRUE
)

print(rep)

# 4) Export conversion report JSON for CI tracing
write_tutorize_report(rep, "lesson-report.json", format = "json")

# 5) Export LMS manifest
manifest <- export_lms_manifest("lesson.qmd", profile = "canvas")
print(manifest)
```

## Reproducibility checklist (reviewer/journal-ready)

```bash
# 1) Install dependencies
Rscript -e 'remotes::install_github("AurelienNicosiaULaval/tutorizeR")'

# 2) Lint and tests
Rscript -e "lintr::lint_package()"
Rscript -e "devtools::test()"

# 3) Build and CRAN-style check from a tarball
R CMD build .
R CMD check --as-cran --no-manual tutorizeR_0.4.3.tar.gz

# 4) Manual smoke path (requires learnr in the environment)
Rscript -e "library(tutorizeR); tutorize('tests/testthat/fixtures/rmd/basic_code.Rmd', format = 'learnr', overwrite = TRUE, output_dir = tempdir(), verbose = FALSE)"
```

Expected on this repository:

- `devtools::test()` passes (currently 98 tests + new fixtures).
- `R CMD check --as-cran --no-manual` yields no errors, no warnings; one NOTE for a first submission is acceptable.

## JOSS submission note

For JOSS, you submit the manuscript source (`paper/paper.md`) and bibliography (`paper/paper.bib`).
You do **not** need to attach a PDF in the repository for submission.
If you want a local PDF preview, render it with:

```bash
cd paper
Rscript -e "rmarkdown::render('paper.md', output_format = 'pdf_document', output_file = 'paper.pdf')"
```

For reviewers/authors, full submission steps are in:

- `docs/joss_submission_guide.md`
- `docs/joss_release_bundle.md`

## Main API

- `tutorize()` / `convert_to_tutorial()`
- `convert_folder()`
- `load_question_bank()` / `validate_question_bank()`
- `lint_source()`
- `write_tutorize_report()`
- `export_lms_manifest()`
- `export_tutorial_package()`
- `check_tutorial()`

## Teacher tags

Inside R chunks:

- `# tutorizeR: skip`
- `# tutorizeR: exercise-only`
- `# tutorizeR: solution-only`
- `# tutorizeR: mcq`
- `# tutorizeR: narrative-only`
- `# tutorizeR: locked`
- `# tutorizeR: hints=Hint 1|Hint 2`

## MCQ block schemas

Explicit question block:

```text
```{tutorizeR-mcq}
question: "2 + 2 = ?"
answers:
  - text: "4"
    correct: true
  - text: "5"
    correct: false
```
```

Question-bank reference block:

```text
```{tutorizeR-mcq-ref}
ids: [mean-basic, sum-basic]
strategy: ordered
shuffle_answers: false
```
```

## Addins

- Convert active file
- Convert folder
- Preview conversion (Source / Output / Diff / Lint / Logs)

## CLI mode

```bash
Rscript inst/scripts/tutorizeR-cli.R --input=lesson.qmd --format=learnr --assessment=both --overwrite=true
Rscript inst/scripts/tutorizeR-cli.R --dir=course_material --recursive=true --format=learnr
```

## Known limitations

- `learnr` render checks require `learnr` and `gradethis` installed.
- LMS export is manifest-only in v0.4 (no direct remote publishing API).
- Question bank is local file based (YAML/JSON) in v0.4.

## Documentation

- `vignettes/getting-started.Rmd`
- `vignettes/question-bank.Rmd`
- `vignettes/tags-and-annotations.Rmd`
- `vignettes/conversion-rmd-vs-qmd.Rmd`
- `vignettes/mcq-and-assessment.Rmd`
- `vignettes/lint-and-debug.Rmd`
