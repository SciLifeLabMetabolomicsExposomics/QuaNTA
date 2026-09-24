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
We use a popular container system called [Docker](https://wwww.docker.com), which runs on multiple platforms such as Linux, MacOS and Windows.
The Docker image can be built using the `Dockerfile` provided in the `code/envs/` directory if you
want to customize the environment.
It encapsulates the software environment.
Please ensure that the necessary management software is pre-installed and running.
For installation questions, please refer to the software's website.

The commands below will restore the same environment and initiate the Shiny app using an image built from the `Dockerfile` and shared on Docker Hub.
Make sure you are in the `<project>` directory before running the command below.

```sh
docker pull mghong/exposome
docker run -it -v .:/proj -p 7579:7579 --name=expo-docker mghong/exposome /proj/code/expo_app.sh
```

The parameters are as follows:

* `-it` : Run the container in interactive mode.
* `-v .:/proj` : Mount the current directory to the `/proj` directory in the container.
* `-p 7579:7579` : Map port 7579 of the container to port 7579 of the host, which is used by the Shiny app.
* `--name=expo-docker` : Name the container `expo-docker`, which can be changed to any name.
* `mghong/exposome` : The name of the Docker image pulled from Docker Hub.
* `/proj/code/expo_app.sh` : The command to run inside the container, which starts the Shiny app.

## Shiny app

When all the required R packages are installed on your system, you can run the Shiny app as follows.
At the project directory, run the following command to start the Shiny app.

```sh
R -e "shiny::runApp('code')"
```

This will indicate the URL to access the Shiny app.
Copy and paste the URL to a web browser to access the Shiny app.

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
