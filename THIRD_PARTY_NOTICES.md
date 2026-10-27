# Third-party notices and acknowledgements

QuaNTA builds on the work of many people. This file lists the third-party software, fonts, data
formats, methods and services that QuaNTA uses or includes, with their licences. Files included in
this repository or in the QuaNTA Docker image remain under their own licences; QuaNTA's licence
covers only QuaNTA's own code.

If you notice anything missing or incorrect here, please open an issue.

--------------------------------------------------------------------------------

## Included in this repository

### Split.js (v1.6.5)

`app/www/split.min.js`: resizable panes of the app. https://github.com/nathancahill/split

```text
MIT License

Copyright (c) 2020 Nathan Cahill

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```

### Fonts (SIL Open Font License 1.1)

The full licence text is included next to each font.

| Font | Files | Copyright | Licence file |
|---|---|---|---|
| Lato | `app/envs/Lato/`, `app/www/fonts/Lato-*.ttf` | Copyright (c) 2010-2014 by tyPoland Lukasz Dziedzic (team@latofonts.com) with Reserved Font Name "Lato" | `app/envs/Lato/OFL.txt`, `app/www/fonts/OFL.txt` |
| Oswald | `app/www/fonts/oswald/` | Copyright 2016 The Oswald Project Authors (https://github.com/googlefonts/OswaldFont) | `app/www/fonts/oswald/OFL.txt` |
| JetBrains Mono | `app/www/fonts/jetbrainsmono/` | Copyright 2020 The JetBrains Mono Project Authors (https://github.com/JetBrains/JetBrainsMono) | `app/www/fonts/jetbrainsmono/OFL.txt` |

### SumExp (v0.2.1)

`app/envs/SumExp_0.2.1.tar.gz`: a light container for omics data, similar to Bioconductor's
SummarizedExperiment. Author: Mun-Gwan Hong (NBIS). Licence: MIT (see the `LICENSE` file in the
package).

### SciLifeLab logo

`app/www/SciLifeLab_Logotype_Green_POS.*`, `app/reports/logos/SciLifeLab_logo.png`: the SciLifeLab
logotype is a trademark of SciLifeLab, shown to indicate the affiliation and the hosting of the app.
It is not covered by QuaNTA's licence; its use follows the SciLifeLab graphic profile.

--------------------------------------------------------------------------------

## Software QuaNTA runs on

### R packages

QuaNTA uses these R packages directly (versions of the development environment; the exact
environment, including indirect dependencies, is recorded in `renv.lock`). They are installed
from CRAN, not distributed with QuaNTA's source code.

| Package | Version | Licence |
|---|---|---|
| box | 1.2.0 | MIT |
| bslib | 0.9.0 | MIT |
| digest | 0.6.37 | GPL (≥ 2) |
| dplyr | 1.1.4 | MIT |
| DT | 0.34.0 | MIT |
| echarts4r | 0.5.0 | Apache (≥ 2.0) |
| fs | 1.6.6 | MIT |
| ggfortify | 0.4.19 | MIT |
| ggplot2 | 4.0.0 | MIT |
| ggtext | 0.1.2 | GPL-2 |
| glue | 1.8.0 | MIT |
| gridExtra | 2.3 | GPL (≥ 2) |
| htmltools | 0.5.8.1 | GPL (≥ 2) |
| janitor | 2.2.1 | MIT |
| jsonlite | 2.0.0 | MIT |
| kableExtra | 1.4.0 | MIT |
| knitr | 1.50 | GPL |
| labelled | 2.16.0 | GPL (≥ 3) |
| lubridate | 1.9.4 | GPL (≥ 2) |
| openxlsx | 4.2.8 | MIT |
| optparse | 1.7.5 | GPL (≥ 2) |
| patchwork | 1.3.2 | MIT |
| plotly | 4.11.0 | MIT |
| processx | 3.8.6 | MIT |
| purrr | 1.1.0 | MIT |
| quarto (R package) | 1.5.1 | MIT |
| readr | 2.1.5 | MIT |
| renv | 1.2.4 | MIT |
| rlang | 1.1.6 | MIT |
| scales | 1.4.0 | MIT |
| shiny | 1.11.1 | GPL-3 |
| shinycssloaders | 1.1.0 | MIT |
| shinydashboard | 0.7.3 | MIT |
| shinydashboardPlus | 2.0.6 | GPL (≥ 2) |
| shinyFiles | 0.9.3 | GPL (≥ 2) |
| shinyjs | 2.1.0 | MIT |
| shinyWidgets | 0.9.0 | GPL-3 |
| stringr | 1.5.2 | MIT |
| tibble | 3.3.0 | MIT |
| tidyr | 1.3.1 | MIT |
| tidyselect | 1.2.1 | MIT |
| vctrs | 0.6.5 | MIT |
| yaml | 2.3.10 | BSD 3-clause |
| zip | 2.3.3 | MIT |

### JavaScript libraries in the app and the HTML report

Served to the browser through the R packages above:

| Library | Through | Licence |
|---|---|---|
| plotly.js | plotly | MIT |
| DataTables | DT | MIT |
| Bootstrap | bslib, shiny | MIT |
| jQuery | shiny | MIT |
| selectize.js | shiny | Apache 2.0 |
| ion.rangeSlider | shiny | MIT |
| Font Awesome Free 6.5.2 (icons) | fontawesome, shiny | icons CC BY 4.0, fonts SIL OFL 1.1, code MIT |

### Docker image

| Component | Licence |
|---|---|
| R (R Core Team, R Foundation for Statistical Computing) | GPL-2 \| GPL-3 |
| Rocker `rocker/shiny` base image (Rocker Project) | GPL-2.0 |
| Shiny Server, open source edition (Posit) | AGPL-3.0 |
| Quarto (Posit) | MIT; includes Pandoc (GPL-2.0 or later) and other components under their own licences |
| TinyTeX (Yihui Xie) and TeX Live packages | GPL-2.0; TeX Live packages under their own free licences |
| Ubuntu base system and system libraries | their respective licences |

--------------------------------------------------------------------------------

## Data formats, methods and inspiration

* **MS-DIAL** (RIKEN; Tsugawa et al. 2015, *Nature Methods* 12:523–526,
  doi:10.1038/nmeth.3393): QuaNTA reads MS-DIAL alignment exports. MS-DIAL is not included.
* **Methods** (normalization, quality control, calibration, statistics): the publications and
  guidelines QuaNTA follows are listed in `app/reports/bibliography.bib`, and cited in the reports
  and the app's About popup. Among them: Surowiec et al. 2017 (multivariate normalization),
  Cleveland & Devlin 1988 (LOESS), De Livera et al. 2012 (relative log abundance), Broadhurst et
  al. 2018 and Dunn et al. 2011 (quality control), Almeida et al. 2002 (weighted calibration), the
  ICH Q2(R2) and M10 guidelines (LOD, LLOQ), Wilcoxon 1945 and Benjamini & Hochberg 1995
  (statistics).
* **MeTaQuaC** (Kuhring et al. 2020, *Analytical Chemistry* 92:10241–10245,
  doi:10.1021/acs.analchem.0c00136): the layout of QuaNTA's internal quality control report is
  inspired by MeTaQuaC's reports. No MeTaQuaC code is included.

--------------------------------------------------------------------------------

## People, projects and services

* **NBIS** (National Bioinformatics Infrastructure Sweden): the original pipeline (reading,
  processing, calibration and reports) was developed by Mun-Gwan Hong in NBIS support project
  #7579, "Building on open source tools for targeted and untargeted metabolomics and exposomics".
* **SciLifeLab Serve** (https://serve.scilifelab.se; RRID:SCR_027820), developed and supported by
  SciLifeLab Data Centre: hosting of the QuaNTA app.
* **Example data** (`example_data/`): entirely simulated by `example_data/make_example_data.R`.
  The design of its calibration standards, QCs and blanks, and their typical signal levels per
  compound, are modelled on a real ESI+ targeted exposomics run (Expo00027), but no measured value
  or spectrum of that run is included.
* **Development tools**: parts of the code and documentation were written with the help of AI
  coding assistants.
