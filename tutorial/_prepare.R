# ------------------------------------------------------------------------------------------- #
# Data of the tutorial: the example dataset read and processed by QuaNTA's own scripts, as the
# app does with its default settings. Run once (later pages reuse the results in `_data/`); delete
# `_data/` to process again, e.g. after a change of the processing.
#
# Used by the pages: `source("_prepare.R")` gives `tut`, a list of the paths and the processed data.
# ------------------------------------------------------------------------------------------- #

tut_root <- normalizePath(".", winslash = "/")                     # tutorial/
app_dir <- normalizePath(file.path(tut_root, "..", "app"), winslash = "/")
data_dir <- file.path(tut_root, "_data")
options(box.path = app_dir)

base <- "QuaNTA_example_ESIpos"
proc_file <- file.path(data_dir, "intermediate", paste0(base, "proc.rds"))

if (!file.exists(proc_file)) {
  dirs <- vapply(c(input = "input", intermediate = "intermediate", tables = "tables", report = "reports"),
                 function(d) { p <- file.path(data_dir, d); dir.create(p, recursive = TRUE, showWarnings = FALSE); p }, "")
  txt <- utils::unzip(file.path(tut_root, "..", "example_data", paste0(base, ".zip")), exdir = dirs[["input"]])
  txt <- txt[grepl("\\.txt$", txt, ignore.case = TRUE)][1]
  params <- file.path(data_dir, "params.yml")
  yaml::write_yaml(list(project_title = "QuaNTA tutorial", user = "", university = "", free_text = "",
                        concentration_unit = "ng/mL", input_file = normalizePath(txt, winslash = "/"),
                        intermediate_dir = dirs[["intermediate"]], table_dir = dirs[["tables"]],
                        report_dir = dirs[["report"]], dataset = basename(txt)), params)
  Sys.setenv(QUANTA_PARAMS_FILE = params, R_BOX_PATH = app_dir, APP_ROOT = app_dir)
  for (s in c("read-msdial.R", "process.R")) {
    status <- system2("Rscript", shQuote(file.path(app_dir, "scripts", s)), stdout = FALSE, stderr = FALSE)
    if (!identical(as.integer(status), 0L)) stop("Preparing the tutorial data failed in ", s)
  }
}

box::use(show = projlib/show)
raw <- readRDS(file.path(data_dir, "intermediate", paste0(base, ".rds")))   # As read from the file
colors <- lapply(yaml::read_yaml(file.path(app_dir, "constants.yml"))$COLORS, unlist)
tut <- list(
  data_dir = data_dir,
  to_report = readRDS(file.path(data_dir, "intermediate", paste0(base, "to_report.rds"))),
  proc = readRDS(proc_file),
  # The colours of the sample classes, as in the app
  class_colors = show$get_colors_of_classes(raw, colors$.ctrl_cat)
)
