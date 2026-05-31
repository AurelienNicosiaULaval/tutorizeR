# JOSE Submission Guide for tutorizeR

Date: 2026-05-31

This guide prepares `tutorizeR` for a possible Journal of Open Source Education submission. It does not state that the package has been submitted, reviewed, or accepted.

Official JOSE guidance used:

- Submitting guide: https://openjournals.readthedocs.io/en/jose/submitting.html
- Review criteria: https://openjournals.readthedocs.io/en/jose/review_criteria.html
- Review checklist: https://openjournals.readthedocs.io/en/jose/review_checklist.html

## Why tutorizeR Fits JOSE

JOSE accepts software that functions as educational technology or infrastructure. `tutorizeR` fits that category because it helps instructors transform reproducible R Markdown and Quarto teaching documents into interactive tutorials and browser-executable resources. Its contribution is educational infrastructure: reducing manual conversion work and supporting source-first development of computational learning materials.

The repository does not claim measured learning gains, improved grades, improved engagement, classroom deployment, or broad adoption. Not verifiable from repository contents.

## What to Submit

For a JOSE submission, prepare:

- repository URL;
- release tag and archive DOI if available;
- `paper/paper.md`;
- `paper/paper.bib`;
- license information for code and educational content;
- reviewer instructions for installation, tests, and examples.

The paper should explain the educational need, intended teaching context, adoption workflow, current evidence, limitations, licensing, and references. It should not replace package documentation.

## How to Reproduce the Example

After installing the package:

```r
library(tutorizeR)

example_dir <- system.file("examples", "example_course_module", package = "tutorizeR")
source_file <- file.path(example_dir, "lesson-source.qmd")
question_bank <- load_question_bank(file.path(example_dir, "question-bank"))

report <- tutorize(
  input = source_file,
  format = "learnr",
  assessment = "both",
  output_dir = tempdir(),
  question_bank = question_bank,
  mcq_source = "mixed",
  overwrite = TRUE,
  verbose = FALSE
)

print(report)
```

The full script is available in `inst/examples/example_course_module/run-example.R`.

## How a Reviewer Can Test the Package

Developer checks:

```bash
Rscript -e "testthat::test_local('.')"
Rscript -e "lintr::lint_package()"
tmpdir=$(mktemp -d)
rsync -a --exclude='.git' --exclude='*.Rcheck' --exclude='*.tar.gz' ./ "$tmpdir/tutorizeR/"
cd "$tmpdir/tutorizeR"
Rscript -e "devtools::document()"
R CMD build .
R CMD check --as-cran --no-manual tutorizeR_0.4.4.tar.gz
```

Installed example check:

```bash
Rscript -e "source(system.file('examples', 'example_course_module', 'run-example.R', package = 'tutorizeR'))"
```

If optional rendering dependencies are missing, reviewers should still be able to inspect generated output files and conversion reports.

## What Remains to Verify Before Submission

- Remote GitHub Actions status on the final branch.
- Final source build and R CMD check from a clean checkout.
- Current release tag.
- Final release DOI: Not verifiable from repository contents.
- ORCID metadata, if the maintainer wants to publish it.
- Any classroom deployment evidence, if the maintainer wants to claim it.

## Demonstrated Claims

The repository demonstrates:

- conversion of `.Rmd` and `.qmd` sources;
- `learnr` and `quarto-live` output scaffolding;
- exercise and solution generation;
- MCQ block and question-bank handling;
- conversion reports;
- installable educational example;
- local tests and vignettes.

## Claims Not Demonstrated

- Formal learning-outcome evaluation: Not verifiable from repository contents.
- Improved student grades: Not verifiable from repository contents.
- Improved student engagement: Not verifiable from repository contents.
- Documented classroom deployment: Not verifiable from repository contents.
- Broad external adoption: Not verifiable from repository contents.
- JOSE acceptance or current JOSE review status: Not verifiable from repository contents.
- Current CRAN publication: Not verifiable from repository contents.
- Final release DOI: Not verifiable from repository contents.
