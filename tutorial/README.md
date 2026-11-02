# QuaNTA tutorial

The source of the QuaNTA tutorial, a [Quarto](https://quarto.org) website published with GitHub
Pages. Read it at: **https://exposomics-scilifelab.github.io/QuaNTA/**

## Writing and updating

- One page per `.qmd` file; the menu is in `_quarto.yml`.
- The figures are made by QuaNTA's own code (`app/projlib/`) from the example data, processed by
  `_prepare.R` with the app's scripts (results in `_data/`, not committed). Delete `_data/` to
  process again, e.g. after a change of the processing.
- Render locally (R with the packages of the app, and Quarto), from this folder:

  ```sh
  quarto render
  ```

  Preview with `quarto preview`. The results of the R code are stored in `_freeze/`: **commit
  `_freeze/`** together with the pages. GitHub publishes from it without running R
  (`.github/workflows/tutorial.yml`), on every push to `main` that changes `tutorial/`.
- A page whose `.qmd` did not change is not run again. After a change of the app that changes the
  figures, delete `_data/` and `_freeze/` and render again, so that all pages are made anew.
- The screenshots of the app (`images/*.png`) are taken by `_screenshots.R` from the running app
  with the example data; rendering does not run it. When the look of the app changed, run it from
  this folder and **commit the new images**:

  ```sh
  Rscript _screenshots.R
  ```

  It needs `shinytest2` (`install.packages("shinytest2")`) and Chrome or Microsoft Edge.
