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
R -e "shiny::runApp('app')"
```

This will indicate the URL to access the Shiny app.
Copy and paste the URL to a web browser to access the Shiny app.

## Tutorial

A step-by-step tutorial with interactive figures, explaining how to read and use the results:
**https://exposomics-scilifelab.github.io/QuaNTA/** (source in [`tutorial/`](tutorial/)).

## Example data

A synthetic example dataset to try the app with is in [`example_data/`](example_data/): the MS-DIAL file (`QuaNTA_example_ESIpos.zip`, uploadable as it is) and an optional parameters file. See [`example_data/README.md`](example_data/README.md) for its content.

## Running QuaNTA from the command line

The steps of the app are R scripts, which can also be run without the app, e.g. for batch
processing. R with the packages of the app is needed (as installed by the `Dockerfile`: the
packages in `renv.lock` and the local `SumExp` package), and Quarto for the reports (with a LaTeX
installation for the PDF report).

### With `run_quanta.R` (recommended)

[`run_quanta.R`](run_quanta.R), in the root of the repository, runs all steps from one file:

1. Open it and fill in the settings at the top: the data file, the output folder, the project
   information, the processing options (the same as in the app, with their allowed values in the
   comments), the steps to run and the normalization method of the exports and reports.
   Instead of the processing options in the script, `parameters_file` can point to a parameters
   file saved in the app ("Download settings (.yml)"): its settings are then used.
2. Run it from the root of the repository:

   ```sh
   Rscript run_quanta.R
   ```

It writes the parameters file `params.yml` into the output folder and runs reading, processing,
the export of the tables and the internal report (the PDF report on request). The output folder
then holds `intermediate/` (the read and processed data), `tables/` (the exported tables) and
`reports/`. With the settings as they are, it processes the example data into `quanta_output/`.

### The scripts one by one

The scripts read their settings from a parameters file, given by the environment variable
`QUANTA_PARAMS_FILE` (default: `params.yml` in the working directory), and find the code of the
app through `R_BOX_PATH`. Run them from the root of the repository:

```sh
export QUANTA_PARAMS_FILE=/path/to/params.yml   # Windows (PowerShell): $env:QUANTA_PARAMS_FILE = "..."
export R_BOX_PATH=$PWD/app
Rscript app/scripts/read-msdial.R               # Read the data file
Rscript app/scripts/process.R                   # Process the data
Rscript app/scripts/export_data.R --norm_method=auto   # Export the tables (auto: the best method)
```

The parameters file holds the paths (absolute) and the project information, and optionally the
processing options in a `processing:` section, as in a file saved in the app:

```yaml
project_title: My project
user: ""
university: ""
free_text: ""
concentration_unit: ng/mL
input_file: /path/to/data.txt          # A .txt file (unpack a .zip / .gz first)
intermediate_dir: /path/to/output/intermediate
table_dir: /path/to/output/tables
report_dir: /path/to/output/reports
processing:
  log_calibration: no
  weight: largestR2
  # ... (see run_quanta.R for all options and their allowed values)
```

Each processing option can also be given on the command line, e.g.
`Rscript app/scripts/process.R --weight=1`; the command line takes precedence over the file.
Options missing in both have their defaults (as in the app). As in the app, a value that is not
valid is replaced by the default, and a number outside its allowed range by the closest allowed
value; the scripts print a note for each.

Unlike the app, the scripts do not ask before processing: the app's checks of the data and the
settings (e.g. that an option needs a volumetric IS, or warnings about Comment values) are shown
only in the app. Read the printed notes and the log of each step.

The reports are rendered with `app/scripts/render_report.R`, which renders them in a private copy of
`app/reports/`; it takes a job file with the report, the normalization method and the output
folder. `run_quanta.R` writes this job file: use it (with only the report steps switched on) to
render reports of data processed before.
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
