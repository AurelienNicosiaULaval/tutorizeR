<p align="center">
  <img src="man/figures/logo.png" alt="tutorizeR hex sticker" height="180">
</p>

[![R-CMD-check](https://github.com/AurelienNicosiaULaval/tutorizeR/actions/workflows/r.yml/badge.svg)](https://github.com/AurelienNicosiaULaval/tutorizeR/actions/workflows/r.yml)
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![JOSE DOI](https://img.shields.io/badge/JOSE%20DOI-pending-lightgrey.svg)](docs/jose_submission_report.md)

# tutorizeR

`tutorizeR` helps instructors convert `.Rmd` and `.qmd` teaching material into interactive `learnr` tutorials or `quarto-live` resources. It supports pedagogical linting, reusable question banks, generated exercise and solution areas, conversion reports, and workflow artifacts for reproducible teaching.

![tutorizeR workflow](man/figures/tutorize-workflow.svg)

## Installation

```r
install.packages("tutorizeR")
```

To install the development version from GitHub:

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

## Educational contribution

`tutorizeR` is being prepared as a potential JOSE software submission: open-source educational infrastructure for computational teaching. It is intended for instructors who already maintain R Markdown or Quarto lessons and want to transform those lessons into interactive learning resources without duplicating source material. The package helps create active coding exercises, conceptual MCQs, reusable question-bank prompts, and feedback-ready `learnr` tutorials while preserving a source-first workflow.

The repository demonstrates the workflow through installable examples, tests, vignettes, and reviewer documentation. It does not claim measured learning gains, improved grades, improved engagement, classroom deployment, or broad adoption unless such evidence is added to the repository.

## Installed example workflow

```r
library(tutorizeR)

example_dir <- system.file(
  "examples",
  "example_course_module",
  package = "tutorizeR"
)

work_dir <- file.path(tempdir(), "tutorizeR-example")
dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)

file.copy(file.path(example_dir, "lesson-source.qmd"), work_dir, overwrite = TRUE)
file.copy(file.path(example_dir, "student_activity.csv"), work_dir, overwrite = TRUE)

source_file <- file.path(work_dir, "lesson-source.qmd")
question_bank <- load_question_bank(file.path(example_dir, "question-bank"))

report <- tutorize(
  input = source_file,
  format = "learnr",
  assessment = "both",
  output_dir = work_dir,
  question_bank = question_bank,
  mcq_source = "mixed",
  overwrite = TRUE,
  verbose = FALSE
)

print(report)
```

The complete example is in `inst/examples/example_course_module/`.

## Reproducibility checklist

Developer checks:

```bash
Rscript -e "testthat::test_local('.')"
Rscript -e "lintr::lint_package()"
tmpdir=$(mktemp -d)
rsync -a --exclude='.git' --exclude='*.Rcheck' --exclude='*.tar.gz' ./ "$tmpdir/tutorizeR/"
cd "$tmpdir/tutorizeR"
Rscript -e "devtools::document()"
R CMD build .
R CMD check --as-cran tutorizeR_0.4.4.tar.gz
```

Installed smoke test:

```bash
Rscript -e "source(system.file('examples', 'example_course_module', 'run-example.R', package = 'tutorizeR'))"
```

## JOSE submission note

`tutorizeR` is a potential JOSE software submission as educational technology and infrastructure. JOSE documentation states that software submissions should support teaching and learning, or make an educational process better, faster, easier, or simpler. The repository currently supports that claim through package functionality, tests, documentation, vignettes, and an installable teaching example.

Reviewer-facing JOSE materials:

- `docs/jose_submission_guide.md`
- `docs/jose_release_bundle.md`
- `docs/jose_review_checklist.md`
- `docs/educational_use_evidence.md`
- `docs/jose_blockers_report.md`
- `docs/jose_pr_final_report.md`
- `inst/examples/example_course_module/README.md`
- `paper/paper.md`
- `paper/paper.bib`

Formal learning-outcome evaluation: Not verifiable from repository contents.
Actual classroom deployment: Not verifiable from repository contents.
Broad instructor adoption: Not verifiable from repository contents.
Final release DOI: Not verifiable from repository contents.
JOSE submission or acceptance: Not verifiable from repository contents.

## Secondary target: JOSS

JOSS remains a secondary possibility if the maintainers decide to emphasize software contribution rather than educational infrastructure. Existing JOSS-oriented documents are kept for that possible route, but the primary preparation in this branch is JOSE.

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

Explicit question blocks use YAML inside a `tutorizeR-mcq` fenced block:

```yaml
question: "2 + 2 = ?"
answers:
  - text: "4"
    correct: true
  - text: "5"
    correct: false
```

Question-bank reference blocks use YAML inside a `tutorizeR-mcq-ref` fenced block:

```yaml
ids: [mean-basic, sum-basic]
strategy: ordered
shuffle_answers: false
```

## Addins

- Convert active file
- Convert folder
- Preview conversion with Source, Output, Diff, Lint, and Logs tabs

## CLI mode

```bash
Rscript inst/scripts/tutorizeR-cli.R --input=lesson.qmd --format=learnr --assessment=both --overwrite=true
Rscript inst/scripts/tutorizeR-cli.R --dir=course_material --recursive=true --format=learnr
```

## Known limitations

- `learnr` render checks require `learnr` and `gradethis` installed.
- `quarto-live` output requires the Quarto live extension in the teaching project.
- LMS export is manifest-only in v0.4, with no direct remote LMS publishing API.
- Question banks are local YAML or JSON files in v0.4.
- Formal learning-outcome evaluation: Not verifiable from repository contents.

## Documentation

- `vignettes/getting-started.Rmd`
- `vignettes/teaching-workflow-scenario.Rmd`
- `vignettes/quarto-lesson-interactive-tutorial.Rmd`
- `vignettes/reproducible-data-science-assignments.Rmd`
- `vignettes/automatic-exercise-generation-feedback.Rmd`
- `vignettes/large-undergraduate-courses.Rmd`
- `vignettes/question-bank.Rmd`
- `vignettes/tags-and-annotations.Rmd`
- `vignettes/conversion-rmd-vs-qmd.Rmd`
- `vignettes/mcq-and-assessment.Rmd`
- `vignettes/lint-and-debug.Rmd`

## Licensing

The package code is released under the MIT license. The CRAN-style license metadata is stored in `LICENSE`, and the full MIT license text is available in `LICENSE.md`.

Educational example materials in `inst/examples/`, generated expected educational outputs, and graphical documentation assets in `man/figures/` are released under CC-BY 4.0 unless otherwise specified. See `LICENSE-CONTENT.md` and `LICENSES.md`.
