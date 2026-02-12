<p align="center">
  <img src="man/figures/logo.png" alt="tutorizeR hex sticker" height="180">
</p>

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.17771142.svg)](https://doi.org/10.5281/zenodo.17771142)

# tutorizeR

`tutorizeR` helps teachers convert existing teaching content (`.Rmd` and `.qmd`)
into interactive tutorials for `learnr` and `quarto-live`.

## Installation

```r
# install.packages("remotes")
remotes::install_github("AurelienNicosiaULaval/tutorizeR")
```

## End-to-end example

```r
library(tutorizeR)

# 1) Convert a single file
rep <- tutorize(
  input = "lesson.qmd",
  format = "learnr",
  assessment = "both",
  overwrite = TRUE
)

print(rep)

# 2) Convert a folder in batch
folder_rep <- convert_folder(
  dir = "course_material",
  recursive = TRUE,
  format = "learnr",
  assessment = "both",
  overwrite = TRUE
)

print(folder_rep)
```

## Main API

- `tutorize()`: canonical conversion API.
- `convert_to_tutorial()`: compatibility wrapper.
- `convert_folder()`: batch conversion with summary report.
- `validate_input()` / `validate_output()`: explicit preflight checks.
- `check_tutorial()`: render check for generated learnr tutorials.

## Feature map

| Feature | How to use |
|---|---|
| Convert one `.Rmd` / `.qmd` | `tutorize(input = "file.qmd")` |
| Choose output format | `format = "learnr"` or `"quarto-live"` |
| Choose assessment mode | `assessment = "code"`, `"mcq"`, `"both"` |
| Batch conversion | `convert_folder("dir", recursive = TRUE)` |
| Skip chunk conversion | `# tutorizeR: skip` inside chunk |
| Exercise-only chunk | `# tutorizeR: exercise-only` |
| Solution-only chunk | `# tutorizeR: solution-only` |
| Force MCQ on chunk | `# tutorizeR: mcq` |
| Add hints | `# tutorizeR: hints=Hint 1|Hint 2` |
| Explicit MCQ schema | fenced block ` ```{tutorizeR-mcq} ` with YAML |

## Explicit MCQ schema

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

## RStudio Addin

Two addins are included:

- Convert active document
- Convert folder

Both expose format/assessment prompts and conversion logs.

## CLI mode

Use the bundled script for CI or non-RStudio workflows:

```bash
Rscript inst/scripts/tutorizeR-cli.R --input=lesson.qmd --format=learnr --assessment=both --overwrite=true
Rscript inst/scripts/tutorizeR-cli.R --dir=course_material --recursive=true --format=learnr
```

## Known limitations

- `learnr` render checks require `learnr` and `gradethis` installed.
- Advanced chunk option expressions are preserved best-effort.
- `quarto-live` output requires `quarto add r-wasm/quarto-live` in project root.

## Documentation

- Vignette: `vignettes/getting-started.Rmd`
- Tag syntax: `vignettes/tags-and-annotations.Rmd`
- Rmd vs qmd conversion: `vignettes/conversion-rmd-vs-qmd.Rmd`
- MCQ and assessments: `vignettes/mcq-and-assessment.Rmd`
- Debugging: `vignettes/debugging-and-errors.Rmd`
