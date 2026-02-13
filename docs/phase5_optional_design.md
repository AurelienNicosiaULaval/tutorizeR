# Phase 5 Optional Design (Future)

This document proposes optional post-stabilization features. These are not part
of the core release gate for 0.3.0.

## 1) Reusable question bank

- Storage: YAML/JSON files under `inst/question-bank/`.
- API sketch: `load_question_bank(path)` and `inject_question_bank(input, bank)`.
- Validation: unique IDs, schema checks, at least one correct answer.

## 2) Internationalization (FR/EN)

- Add centralized message dictionary.
- Add template translation support by language code.
- Keep English as fallback.

## 3) Standardized locked exercises and hints

- Formalize tag grammar:
  - `# tutorizeR: locked`
  - `# tutorizeR: hints=Hint1|Hint2`
- Add explicit rendering rules for learnr/quarto-live outputs.

## 4) Conversion analytics report

- Extend `tutorize_report` with:
  - number of sections,
  - number of exercises,
  - number of MCQs,
  - estimated completion time.

## 5) Tutorial package export mode

- Add option to scaffold a package-based tutorial (`usethis::use_tutorial`).
- Keep as opt-in because it introduces package skeleton side effects.

## Non-goals (for now)

- Automatic pedagogical quality scoring.
- Full AST-level transformation of arbitrary non-R engines.
