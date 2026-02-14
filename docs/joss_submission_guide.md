# JOSS submission guide for `tutorizeR`

## 1) What to submit

You submit the repository plus the source manuscript in:

- `paper/paper.md`
- `paper/paper.bib`

You **do not** need to commit or upload a PDF in the repository for JOSS.
JOSS will render the paper during the review process.

Required companion files for review:

- `paper/paper.md` (with front matter + markdown references)
- `DESCRIPTION`
- `LICENSE` / `LICENSE.md`
- `README.md`
- `NEWS.md`
- Tests and CI configuration (`tests/`, `.github/workflows/r.yml`, etc.)

## 2) JOSS workflow (high-level)

1. Author opens a JOSS submission on the JOSS site and links the GitHub repository.
2. JOSS creates a review issue in `joss-reviews/joss-reviews`.
3. The editor/checker runs:
   - installability check,
   - repository checks (license, README, CI, tests, docs),
   - functionality spot-check.
4. One or more reviewers run the reproduction commands from the manuscript/review checklist.
5. Revision rounds are handled directly as pull requests / commits to your repository.
6. After acceptance, package metadata is finalized and DOI is minted when the article is published.

This is different from a standard journal where:
- the full narrative manuscript is primary,
- acceptance is mostly text-centric,
- proofs and production are central.

## 2a) Updated JOSS scope/significance gate to verify before submission

Before submitting, verify explicitly that the repository matches the current JOSS criteria:

- The software has a clear research/community use case beyond a one-off utility.
- The package is not a "single-function" or thin wrapper; it is a coherent, maintainable tool with documented testing, docs, and clear contribution path.
- The repository has sustained public history (JOSS asks for evidence of at least ~6 months of open development).
- The issue tracker and PR history are public and usable without manual approvals/payments.
- Features are feature-complete and packaged according to community conventions for R.
- A reviewer can install and reproduce core behaviour from source with documented commands.

From the submission page:
- JOSS explicitly emphasizes clear research impact and credible scholarly significance.
- They ask for evidence of public, sustainable development and meaningful software engineering choices.
- An AI usage disclosure statement is required when AI was used in code/paper/docs creation.

Suggested preflight docs to attach in your JOSS notes:

- `git log --since='6 months ago' --oneline`
- `.github/ISSUE_TEMPLATE` and `PULL_REQUEST_TEMPLATE` presence.
- Release/reproducibility proof: `docs/joss_release_bundle.md`.

For JOSS, the **software + reproducibility + review criteria** are central, and the article is a concise companion.

## 3) Reviewer-critical checks already prepared in this repo

- `lintr::lint_package()`
- `devtools::test()`
- `R CMD build .`
- `R CMD check --as-cran --no-manual tutorizeR_0.4.3.tar.gz`
- smoke conversion on fixtures (3 Rmd + 3 qmd)
- CI matrix (Ubuntu, macOS, Windows) + lintr + coverage gate

## 4) Pre-submission commands to cite (exact)

Run these and keep their outputs in `docs/joss_release_bundle.md` (already present):

```bash
Rscript -e "lintr::lint_package()"
Rscript -e "devtools::test()"
R CMD build .
R CMD check --as-cran --no-manual tutorizeR_0.4.3.tar.gz
```

Optional optional manual smoke:

```bash
Rscript -e "library(tutorizeR); tutorize('tests/testthat/fixtures/rmd/basic_code.Rmd', format = 'learnr', overwrite = TRUE, output_dir = tempdir(), verbose = FALSE)"
```

## 5) What to put in the JOSS submission form

### Required text (copy/paste blocks)

- Repository: `https://github.com/AurelienNicosiaULaval/tutorizeR`
- Paper: `paper/paper.md`
- Short abstract:
  > `tutorizeR` is an R package that converts existing `.Rmd` and `.qmd` teaching materials into `learnr` and `quarto-live` interactive resources with deterministic parsing, linting checks, MCQ workflows, and reporting utilities for teacher-centric workflows.

- Keywords: `R`, `education`, `learnr`, `quarto`, `lms`, `interactive tutorials`

## 6) Expected JOSS outcome

- Decision: acceptance after at least one complete review cycle
- Public review issue referencing repository commits
- DOI assignment at publication
- Review report stored in GitHub issue + metadata updates in `paper/paper.md` if needed

## 7) Final submission checklist

- [x] Branch and release are at `v0.4.3`
- [x] `docs/joss_release_bundle.md` prepared
- [x] Reproducibility commands and smoke results are documented
- [x] CI and release checks are green (or pending but included if known)
- [ ] Create JOSS submission issue and paste the exact git commit/tag references
- [ ] Upload/complete final manuscript review if reviewer asks for edits
