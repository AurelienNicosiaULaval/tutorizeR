# 📍 Roadmap — tutorizeR

This roadmap outlines the development of the `tutorizeR` package, which converts `.Rmd` and `.qmd` files into interactive `learnr` tutorials.

---

## ✅ Version 0.1 (current)

- 🔄 Converts both `.Rmd` and `.qmd` source files into `.Rmd`-based `learnr` tutorials.
- 🧪 Each original code chunk becomes a **learnr exercise** followed by a **solution** chunk.
- 🧩 Optional `add_mcq = TRUE` adds a skeleton multiple-choice question (MCQ) block after each exercise.
- ⚙️ Automatically preserves setup chunks (e.g. `{r setup}`), keeping them untouched.
- 🧼 Strips YAML front-matter to avoid header duplication.
- ✅ Includes a `check_tutorial()` function to render the tutorial and warn about rendering errors.

---

## 🚧 Proposed Features for Version 0.2

### 📘 1. Full Quarto Live Support

- [ ] Output `.qmd`-based interactive tutorials using `quarto-live` + `webR`.
- [ ] Automatically insert correct YAML with `format: live-html`.
- [ ] Include Quarto extensions like `{{< include ./_extensions/... >}}` only once.
- [ ] Detect and optionally install `r-wasm/live` extension; guide users when needed.

### 🧠 2. Enhanced Pedagogical Features

- [ ] Allow skipping specific chunks using inline comment markers (e.g., `# tutorizeR: skip`).
- [ ] Let users specify `grading = "code"` or `grading = "mcq"` to adjust assessment behavior.
- [ ] Support for custom feedback in MCQs via `learnr::question()` with `feedback()`.

### ⚙️ 3. Usability & Automation

- [ ] Add a `convert_folder()` function to batch convert all `.Rmd`/`.qmd` files in a directory.
- [ ] Provide a simple RStudio add-in (`launch_tutorizeR_addin()`) for interactive file conversion.
- [ ] Allow one-click conversion via `.Rprofile` configuration or RStudio toolbar integration.

### 📤 4. Deployment & Demo Infrastructure

- [ ] Publish a demo site (via GitHub Pages) with example converted tutorials.
- [ ] Add a companion repository with test cases (short, long, broken code, etc.).
- [ ] Improve the README with live screenshots, tutorial badges, and installation guide.

---

## 🔮 Ideas for Version 0.3+

- 📦 Posit Cloud integration (publish converted tutorials directly).
- 💬 Multilingual tutorial conversion (English / French support).
- 🔁 Reverse conversion: rebuild a clean script from a tutorial with solutions.
- 🧪 Shiny interface to preview and edit converted tutorials before exporting.

---

## 📌 Tracking

Stable releases will be tagged (`v0.1`, `v0.2`, …) and milestones will be used for coordination.  
Each feature will be discussed in a GitHub issue with the `enhancement` or `v0.2` label.

