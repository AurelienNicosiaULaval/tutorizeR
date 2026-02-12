# tutorizeR Audit (Phase 0)

## Scope

Repository audited: `/Users/aureliennicosia/Documents/tutorizeR`  
Date: 2026-02-12

This audit covers:

- Source code in `R/`
- Templates in `inst/`
- Documentation in `README.md`, `NEWS.md`, `man/`
- Tests in `tests/`
- CI in `.github/workflows/`

## Current Public API

Exported functions:

- `convert_to_tutorial(input_file, output_file = NULL, assessment, format, add_mcq = NULL)`
- `convert_folder(dir = ".", pattern = "\\.(Rmd|qmd)$", recursive = FALSE, ...)`
- `check_tutorial(file)`
- `launch_tutorizeR_addin()`
- `launch_tutorizeR_folder_addin()`

Observed side effects:

- Writes converted file to disk
- Runs `rmarkdown::render()` automatically for `learnr` output
- Emits CLI messages for success/failure

## Format/Feature Coverage Matrix (Current)

| Capability | `.Rmd` input | `.qmd` input | Notes |
|---|---|---|---|
| Basic code-chunk conversion | Partial | Partial | Ad-hoc fence parsing; can mis-handle non-R blocks |
| Setup chunk preservation | Yes | Yes | Preserved verbatim, but can collide with injected setup label |
| YAML metadata preservation | No | No | Existing YAML stripped and replaced with fixed header |
| Chunk options preservation (`echo`, `eval`, etc.) | No | No | Original options dropped on converted exercise/solution |
| Non-R engines (`python`, `bash`, etc.) | No | No | Can be converted incorrectly to R exercises |
| Inline one-line chunks | No | No | Can be dropped due to parser assumptions |
| Label collision prevention | No | No | Duplicate labels can break render |
| Skip tag (`# tutorizeR: skip`) | Yes | Yes | Basic support implemented |
| Assessment modes (`code`, `mcq`, `both`) | Yes | Yes | Implemented; MCQ is skeleton only |
| Quarto live output | Partial | Partial | Output exists but extension path detection tied to `getwd()` |
| Batch conversion | Yes | Yes | Basic, limited reporting |

## Key Risks

### P0 (High impact for teachers)

1. Fragile parsing logic can lose content or mis-convert non-R code blocks.
2. YAML and chunk option loss reduces fidelity to source pedagogy.
3. Label collisions (`setup`, `ex1`) can generate invalid tutorial documents.
4. Error messages are not consistently action-oriented for time-constrained instructors.

### P1

1. Tests do not cover realistic fixtures or edge cases.
2. Documentation is insufficient for novice instructor onboarding.

### P2

1. CI is single-OS and lacks quality gates (`lintr`, coverage).
2. Release readiness artifacts are missing.

## Security, Reproducibility, and Reliability Notes

- `check_tutorial()` evaluates generated code in a new environment, but still executes arbitrary source code from converted documents.
- Batch mode does not provide structured per-file failure reporting suitable for CI.
- Quarto live extension detection uses relative `getwd()` assumptions instead of input file location.
- Default output-name logic can overwrite input in non-standard extension scenarios.

## Prioritized Roadmap

### 1) Stabilize conversion core (first)

- Structured parser pipeline for fenced chunks and YAML
- Deterministic label normalization with anti-collision
- Preserve narrative and non-R fenced blocks untouched
- Preserve key YAML fields and relevant chunk options

### 2) Validation and teacher-facing errors

- Add `validate_input()` and `validate_output()`
- Introduce typed error classes (`validation`, `parse`, `render`)
- Produce concise actionable failure hints

### 3) Tests and fixtures

- Add fixture corpus (`3 x Rmd`, `3 x qmd`, plus edge cases)
- Add structure tests and at least one render smoke test

### 4) UX API and operations

- Add canonical `tutorize()` API while keeping compatibility wrappers
- Improve `convert_folder()` with summary report and resilient failure handling
- Add CLI script for non-RStudio workflows

### 5) Documentation and publication quality

- Add vignettes + pkgdown
- Expand README with complete end-to-end examples and limits
- Add release checklist and contribution governance files

## Decisions Locked for Implementation

1. Source of truth repo: `/Users/aureliennicosia/Documents/tutorizeR`.
2. Public API compatibility: keep `convert_to_tutorial()` and `convert_folder()`.
3. Canonical API evolution: add `tutorize()` as stable entry point.
4. Parser strategy: robust parsing with focused dependencies (`yaml`, `knitr`, `commonmark`) and defensive fallbacks.
5. Phase 5 remains optional design, clearly separated from committed core deliverables.

## Success Criteria (Implementation Phases)

- Reliable conversion on six representative fixtures (3 `.Rmd`, 3 `.qmd`) without manual intervention.
- Explicit actionable errors in validation/parsing/render failure paths.
- Reproducible onboarding path documented for novice teachers.
- CI checks consistently passing on at least three operating systems.
