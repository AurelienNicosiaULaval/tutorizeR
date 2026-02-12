# tutorizeR 0.3.0

* Added canonical `tutorize()` API with conversion reports (`tutorize_report`).
* Kept backward-compatible `convert_to_tutorial()` wrapper.
* Added explicit validation helpers: `validate_input()` and `validate_output()`.
* Reworked conversion pipeline with structured parsing and deterministic label normalization.
* Preserved narrative blocks and non-R fenced code blocks during conversion.
* Added teacher tags: `skip`, `exercise-only`, `solution-only`, `mcq`, `narrative-only`, `locked`, and `hints`.
* Added explicit MCQ block schema via fenced `{tutorizeR-mcq}` YAML blocks.
* Improved `convert_folder()` with per-file error handling and summary reports.
* Updated RStudio addins with option prompts.
* Added CLI script at `inst/scripts/tutorizeR-cli.R`.
* Added fixtures and extended test suite for Rmd/qmd core and edge scenarios.
* Added vignettes, templates, audit/release docs, and release governance scaffolding.

# tutorizeR 0.1.0

* First stable release - learnr-only output.
* Preserves setup chunks.
* Optional MCQ skeletons.
* Auto-render check after generation.
