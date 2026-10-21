This directory contains the code for the program in the support project.
This document is written in Markdown (`.md`) and is best viewed using a Markdown viewer.

# Project information

* Title : Building on open source tools for targeted and untargeted metabolomics and exposomics
* Redmine issue at NBIS: **#7579**  (internal project ID)
* Request by: **Stefano Papazian**
* Principal investigator: **Stefano Papazian** (<stefano.papazian@scilifelab.se>)
* Organisation: **Stockholm University**
* NBIS experts : Mun-Gwan Hong (<mungwan.hong@nbis.se>)
* Git repository : https://github.com/NBISweden/SMS-7579-23-exposome

--------------------------------------------------------------------------------

# Citing QuaNTA

* The app on [SciLifeLab Serve](https://serve.scilifelab.se): cite it by its DOI on Serve (shown on
  the app's page there), not by its address. Acknowledgement of the hosting, as suggested by Serve:
  "Technical infrastructure for hosting the app 'QuaNTA' was provided by SciLifeLab Serve
  (https://serve.scilifelab.se; RRID:SCR_027820), a platform developed and supported by SciLifeLab
  Data Centre."
* The methods QuaNTA uses are referenced in `app/reports/bibliography.bib`; the reports and the
  app's About popup cite them.

--------------------------------------------------------------------------------

# How to use the program

## Copy the code

At first, make a directory for the project unless it already exists.
Copy the code to the directory `<project>/code/`.
One easy way to accomplish this is to make a Git clone of the project repository using the following commands.

```sh
cd <project>                    # <project> should be replaced with the appropriate path
git clone <repository> code     # Find the <repository> URL provided above
```

The subdirectories within the `<project>/code/` directory are structured as shown below.

```text
├── code/
│   ├── envs/             # Software environment
|   ├── graphics/         # text format graphics, e.g. .svg
│   ├── projlib/          # Reusable code modules imported by other scripts
│   ├── R/                # Functions for `app.R` (shiny app)
│   ├── reports/          # Code that generates reports in `<project>/reports/`
│   │   └── chapters/       # Individual chapters of the reports
│   └── scripts/          # Scripts (AWK, R, Python, etc.)
```

## Software environment

To mitigate issues arising from software environment inconsistencies, the environment in which the scripts were developed has been saved together with the scripts.
We use a popular container system called [Docker](https://www.docker.com), which runs on multiple platforms such as Linux, MacOS and Windows.
Please ensure that Docker is installed and running.
For installation questions, please refer to the software's website.

The `Dockerfile` in the root of the repository builds the image that runs the Shiny app, e.g. on
[SciLifeLab Serve](https://serve.scilifelab.se).
It installs R and the packages in `renv.lock`, Quarto with TinyTeX (for the PDF report) and the
local `SumExp` package, copies the app (`app/`) to `/srv/shiny-server/`, and starts the app with
**shiny-server** on port **3838**, as the user `shiny` (UID 999, as SciLifeLab Serve expects).
The Quarto version is fixed in the `Dockerfile` (`QUARTO_VERSION`); change it there on purpose and
check the reports. `.dockerignore` keeps test data, mockups and caches out of the build.
Building the image needs internet access; the running app does not (fonts and LaTeX packages are
included in the image).

To build and run the image locally, run the commands below in the root of the repository and open
<http://localhost:3838> in a web browser.

```sh
docker build -t quanta .
docker run --rm -p 3838:3838 quanta
```

The parameters are as follows:

* `-t quanta` : Name the image `quanta`, which can be changed to any name.
* `--rm` : Remove the container when it stops.
* `-p 3838:3838` : Map port 3838 of the container, used by shiny-server, to port 3838 of the host.

All data of a user session (uploads, processed data, tables and reports) are stored in a private
temporary directory of that session inside the container and deleted when the session ends.

## Shiny app

When all the required R packages are installed on your system, you can run the Shiny app as follows.
At the project directory, run the following command to start the Shiny app.

```sh
R -e "shiny::runApp('code')"
```

This will indicate the URL to access the Shiny app.
Copy and paste the URL to a web browser to access the Shiny app.

## Example data

A synthetic example dataset to try the app with is in [`example_data/`](example_data/): the MS-DIAL file (`QuaNTA_example_ESIpos.zip`, uploadable as it is) and an optional parameters file. See [`example_data/README.md`](example_data/README.md) for its content.

## Running scripts without the Shiny app

As an alternative way, you can also run the individual scripts in the command line.
First, find the **`params.yml`** file in the `<project>/` directory and modify the parameters as needed.

The scripts are written in R and qmd, and can be run in the following way.

```sh
cd <project>
Rscript code/scripts/read-msdial.R    # Read MS-DIAL files
Rscript code/scripts/process.R        # Process the data
Rscript code/scripts/export_data.R    # Export the data into Excel files

# Generate reports
cd <project>/code/reports
quarto render report-internal.qmd --output <output_report>.html
quarto render report-external.qmd --output <output_report>.pdf
```

--------------------------------------------------------------------------------

# Individual files under `code/`

## In the root of `code/`

Here are some descriptions on individual files.

* `constants.yml` : Contants used in analyses and visualization
* `LICENSE` : License for the code here

## R style guide

R code in this project adhere to [the `tidyverse` **R style guide**](https://style.tidyverse.org/syntax.html) as closely as possible with some modifications.

R pacakges are **not** loaded to the search path using `library()` or `require()`.
Instead, R functions, except for those from the packages listed below, are primarily called with double colons (`::`) to specify the source package explicitly.

* in `R-core` (e.g. `base`, `stats`, `utils`, ...)
* Packages for S3 methods (e.g. `predict`, `autoplot`, ...) unable to specify them using `::`

Whenver a package or a module of code is to be loaded, use `box::use()` to explicitly state the source.
