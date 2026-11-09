# CLAUDE.md

QuaNTA: an R Shiny app (with scripts and Quarto reports) for targeted and non-targeted MS-DIAL
data, and a Quarto tutorial website. How the code is organised: `app/CLAUDE.md`.

- `app/` — the app, its scripts (`scripts/`), shared code (`projlib/`), reports (`reports/`) and tests
- `tutorial/` — the tutorial website (Quarto; results stored in `_freeze/`, published by GitHub Actions)
- `example_data/` — the example dataset
- `run_quanta.R` — the whole pipeline without the app

## Working agreements

- **Discuss before changing.** Explain and propose first; edit files only when the user says so.
- **Do not test until the user confirms.** No app runs, report or tutorial renders, test scripts
  or browser checks until the user has said to test, so that several features are tested at once.
  Keep a list of what is changed but not tested, say so in the reply, and ask when it is a good
  time to test. (A quick syntax check, e.g. `parse()` of a changed R file, is fine.)
- **The external (PDF) report** is being reworked by the user: leave it out of all testing, and
  change it only when asked.
- **The tutorial** is rendered by the user (`quarto render` in `tutorial/`); say which pages need it.
- **Commits** are made by the user.

## Terminology

- **Injection type**: QC, blank, calibration standard or sample (not "sample type").
- **Sample**: always a real study sample, nothing else. Use "injection" when QCs, blanks or
  calibration standards are included.
