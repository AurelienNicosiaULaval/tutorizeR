<p align="center">
  <img src="man/figures/logo.png" alt="tutorizeR hex sticker" height="180">
</p>

# tutorizeR

Have a Quarto (`.qmd`) or R Markdown (`.Rmd`) data analysis you want to turn into an interactive tutorial with built-in solutions? This package is what you need.
`tutorizeR` converts existing **R Markdown** (`.Rmd`) or **Quarto** (`.qmd`) files into interactive tutorials powered by [`learnr`](https://rstudio.github.io/learnr/). Each code chunk becomes an exercise block followed by a solution chunk ready to be graded with `gradethis`.

## Features (v0.1.0)

- Accepts both `.Rmd` and `.qmd` input files.
- Preserves `setup` chunks exactly as they appear in the source document.
- Automatically strips YAML front matter before processing.
- Optionally inserts a skeleton multiple-choice question after every exercise.
- Checks that the generated tutorial renders successfully.

These features correspond to the first stable release `v0.1.0`. See `NEWS.md` for the full changelog.

## Installation

Install the development version from GitHub with:

```r
# install.packages("remotes")  # if not already installed
remotes::install_github("AurelienNicosiaULaval/tutorizeR")
```

## Usage

```r
library(tutorizeR)

# Convert a Quarto or R Markdown document to a learnr tutorial
convert_to_tutorial("analysis.qmd", add_mcq = TRUE)
```

This will create `analysis-tutorial.Rmd` containing interactive exercises (and optional MCQs). The file is rendered once to ensure it compiles without errors.
