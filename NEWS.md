# tutorizeR 0.4.3

## Release stabilization

* Prepared final JOSS/CRAN-style release workflow:
  * pinned package metadata version to `0.4.3`;
  * refreshed release checklist and finalization documentation;
  * validated reproducible validation commands (`R CMD build` + `R CMD check --as-cran --no-manual`).
* Documentation and review support:
  * clarified JOSS reproducibility instructions in README;
  * added “state of the art / alternatives” context and explicit limitations in manuscript.
* Final release metadata:
  * `Description`, `CITATION.cff`, and `codemeta.json` aligned with `0.4.3`.

# tutorizeR 0.4.2

## Major features

* Added reusable local question-bank support (YAML/JSON):
  * `load_question_bank()`
  * `validate_question_bank()`
  * MCQ reference chunks via `{tutorizeR-mcq-ref}`.
* Added pedagogical linting:
  * `lint_source()` with severity levels and rule codes (`TRZ*`).
  * strict gate via `lint_strict = TRUE` in `tutorize()`.
* Added complete FR/EN i18n helper:
  * `tr()` and dictionary files in `inst/i18n/`.
* Extended conversion reports:
  * richer `tutorize_report` fields (`sections`, `chunks_total`,
    `mcq_explicit`, `mcq_from_bank`, `estimated_minutes`, `lint_summary`).
  * report export with `write_tutorize_report()` (JSON/YAML).
* Added preview addin:
  * `launch_tutorizeR_preview_addin()` with Source/Output/Diff/Lint/Logs tabs.
* Added dissemination exports:
  * `export_lms_manifest()` profiles: `generic`, `canvas`, `moodle`.
  * `export_tutorial_package()` opt-in scaffold generation.
* Added industrial hardening assets:
  * regression fixtures + structural snapshot-style tests.
  * stress batch test.
  * CI coverage gate for core modules (`parser`/`transform`/`validation`/`report`) set to 80%.

## Compatibility

* `convert_to_tutorial()` and `convert_folder()` remain backward compatible.

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
