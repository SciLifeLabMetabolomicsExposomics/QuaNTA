# ============================================================================================ #
# QuaNTA from the command line
#
# 1. Fill in the settings in the three sections below (or point `parameters_file` to a
#    parameters file saved in the app: "Download settings (.yml)").
# 2. Run from the project folder (the folder that contains `app/` and this file):
#        Rscript run_quanta.R
#
# The script writes the parameters file `params.yml` into the output folder and runs the steps
# of the app one after the other: reading the data file, processing, exporting the tables and
# generating the report(s). The results are in the output folder:
#   intermediate/  the read and processed data (.rds)
#   tables/        the exported tables (.xlsx)
#   reports/       the report(s)
#
# Requirements: R with the packages of the app (as in the Dockerfile), Quarto for the reports
# (and a LaTeX installation for the PDF report).
# Unlike the app, this script does not ask before processing: check the printed notes (e.g. a
# setting that was out of range and corrected) and the log of each step.
# ============================================================================================ #

## ---- 1. Data file and project information ---------------------------------------------------
input_file         <- "example_data/QuaNTA_example_ESIpos.zip"   # .txt, or .zip / .gz with one .txt
output_dir         <- "quanta_output"                              # Created if missing
project_title      <- ""
user               <- ""
university         <- ""
concentration_unit <- "ng/mL"
free_text          <- ""   # Free text for the external report

## ---- 2. Processing options ------------------------------------------------------------------
# A parameters file saved in the app: its settings (project information and processing options)
# replace those of this script. NULL: the settings below.
parameters_file <- NULL   # e.g. "quanta_parameters_20261026.yml"

processing <- list(
  # Reading
  per_batch           = TRUE,    # Blank subtraction and calibration per batch (Batch ID)
  # Quality control
  vis_prenorm         = FALSE,   # Pre-normalize all peak areas by the volumetric IS (vIS)
  rm_outlier          = TRUE,    # Remove outlier samples (> 20% of the IS beyond 3 SD, log scale)
  rm_outlier_scope    = "all",   # "all" injections, or "no_calib": calibration samples always kept
  blank_filter        = TRUE,    # Blank filtering (after normalization, before blank subtraction)
  blank_filter_factor = 5,       # Kept when the sample max or mean > factor x blank mean (>= 0)
  rsd_filter          = FALSE,   # Remove features with a high QC RSD%
  rsd_filter_scope    = "both",  # "both", "targeted" or "nontargeted"
  rsd_max_targeted    = 30,      # Maximum QC RSD% of targeted features (1-80)
  rsd_max_nontargeted = 30,      # Maximum QC RSD% of non-targeted features (1-80)
  # Calibration
  optimize_cal_points = FALSE,   # Leave out calibration points above the highest sample
  set_minimum_number  = FALSE,   # Require more calibration levels than 3 ...
  min_cal_points      = 3,       # ... namely this many (>= 3; used only with set_minimum_number)
  average_cal_reps    = FALSE,   # Average calibration replicates per batch (global curve)
  keep_all_calcurves  = FALSE,   # Keep the curves of compounds without a valid range (marked)
  r2_filter           = FALSE,   # Minimum R2 of the calibration curves ...
  min_r2              = 0.99,    # ... namely this (0-1)
  linear_only         = FALSE,   # Linear models only (no quadratic)
  linear_range_tol    = 15,      # ULOQ of linear curves: top levels deviating more than this % (1-50)
                                 # when back-calculated are above the linear range (log: SCALiR)
  enforce_linear_range = FALSE,  # Cut the curves at the ULOQ and refit them
  log_calibration     = FALSE,   # Fit in log scale (weighting 1 only)
  weight              = "largestR2",              # "largestR2", "1", "1_div_x" or "1_div_x2"
  llox_method         = "pt_signal_mean_plus_sd", # "pt_signal_mean" or "pt_signal_mean_plus_sd"
  use_rsd20           = TRUE,    # The LLOQ level must also have an RSD% <= 20%
  impute_llox         = TRUE     # Values below the LOD/LLOQ: LLOQ/4, LLOQ/2 (FALSE: n.d. / n.q.)
)

## ---- 3. Steps and outputs -------------------------------------------------------------------
steps <- c(read = TRUE, process = TRUE, export = TRUE, internal_report = TRUE, external_report = FALSE)
# Normalization of the exported tables and the reports: "auto" (the method with the lowest
# median QC RSD%), "loess_norm", "closest_norm", "mvis_norm" or "raw"
norm_method <- "auto"


# ============================================================================================ #
# Nothing to change below
# ============================================================================================ #
project_dir <- normalizePath(".", winslash = "/")
app_dir <- file.path(project_dir, "app")
if (!file.exists(file.path(app_dir, "projlib", "msdial.R"))) {
  stop("Run this script from the project folder (the folder that contains app/ and run_quanta.R).")
}
options(box.path = app_dir)
box::use(settings = projlib/settings)
abs_path <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)
# Quoted for the command line of the system (paths with spaces)
quote_arg <- function(x) shQuote(x, type = if (.Platform$OS.type == "windows") "cmd" else "sh")

# Settings of a parameters file saved in the app
user_info <- list(project_title = project_title, user = user, university = university,
                  concentration_unit = concentration_unit, free_text = free_text)
if (!is.null(parameters_file)) {
  if (!file.exists(parameters_file)) stop("Parameters file not found: ", parameters_file)
  pf <- settings$read_parameters_file(parameters_file)
  for (k in names(user_info)) if (!is.null(pf[[k]])) user_info[[k]] <- as.character(pf[[k]])
  if (is.list(pf$processing)) processing <- utils::modifyList(processing, pf$processing)
  cat("Settings from", parameters_file, "\n")
}

# Output folders
out <- abs_path(output_dir)
dirs <- c(input = "input", intermediate = "intermediate", tables = "tables", report = "reports")
dirs <- vapply(dirs, function(d) { p <- file.path(out, d); dir.create(p, recursive = TRUE, showWarnings = FALSE); p }, "")

# The data file: a .txt as it is; a .zip / .gz unpacked into input/
if (!file.exists(input_file)) stop("Data file not found: ", input_file)
ext <- tolower(tools::file_ext(input_file))
data_file <- if (ext == "zip") {
  inner <- utils::unzip(input_file, list = TRUE)$Name
  inner <- inner[!grepl("/$|(^|/)__MACOSX/|(^|/)\\.", inner)]
  if (length(inner) != 1 || !grepl("\\.txt$", inner, ignore.case = TRUE)) {
    stop("The zip file must contain exactly one .txt file.")
  }
  utils::unzip(input_file, files = inner, exdir = dirs[["input"]], junkpaths = TRUE)
  file.path(dirs[["input"]], basename(inner))
} else if (ext == "gz") {
  dest <- file.path(dirs[["input"]], sub("\\.gz$", "", basename(input_file), ignore.case = TRUE))
  con_in <- gzfile(input_file, "rb"); con_out <- file(dest, "wb")
  repeat { x <- readBin(con_in, "raw", 1e7); if (length(x) == 0) break; writeBin(x, con_out) }
  close(con_in); close(con_out)
  dest
} else if (ext == "txt") {
  abs_path(input_file)
} else {
  stop("The data file must be a .txt file, or a .zip / .gz file with one .txt file inside.")
}

# The parameters file of the scripts
params_file <- file.path(out, "params.yml")
yaml::write_yaml(c(user_info,
                   list(input_file = abs_path(data_file), intermediate_dir = dirs[["intermediate"]],
                        table_dir = dirs[["tables"]], report_dir = dirs[["report"]],
                        dataset = basename(data_file), processing = processing)),
                 params_file)
cat("Parameters file written:", params_file, "\n")
Sys.setenv(QUANTA_PARAMS_FILE = params_file, R_BOX_PATH = app_dir, APP_ROOT = app_dir)

# Run one script; stop when it fails
run <- function(what, script, args = character()) {
  cat("\n==== ", what, " ====\n", sep = "")
  status <- system2("Rscript", c(quote_arg(file.path(app_dir, "scripts", script)), args))
  if (!identical(as.integer(status), 0L)) stop(what, " failed (exit status ", status, "); see the messages above.")
}
if (steps[["read"]]) run("Reading the data file", "read-msdial.R")
if (steps[["process"]]) run("Processing", "process.R")

# The normalization method: "auto" is the best one found by processing
base <- tools::file_path_sans_ext(basename(data_file))
method <- norm_method
if (identical(method, "auto") && (steps[["internal_report"]] || steps[["external_report"]])) {
  tr <- readRDS(file.path(dirs[["intermediate"]], paste0(base, "to_report.rds")))
  method <- tr[["best normalization"]]
  cat("\nNormalization of the exports and reports:", method, "\n")
}
if (steps[["export"]]) run("Exporting the tables", "export_data.R", paste0("--norm_method=", norm_method))

# Reports (each rendered in a private copy of the report sources, as in the app)
render <- function(what, qmd, format, type) {
  time <- Sys.time()
  out_name <- paste0(base, "-", method, "-", type, "_", format(time, "%Y%m%d_%H%M%S"), ".", format)
  job <- list(reports_dir = file.path(app_dir, "reports"), work_dir = file.path(out, paste0("render_", type)),
              qmd = qmd, output_format = format, out_name = out_name, out_dir = dirs[["report"]],
              execute_params = list(norm_method = method, interactive_plot = FALSE,
                                    project_title = user_info$project_title,
                                    generated_at = format(time, "%Y-%m-%d %H:%M:%S %Z"), on_serve = FALSE))
  job_file <- file.path(out, paste0("job_", type, ".rds"))
  saveRDS(job, job_file)
  on.exit(unlink(job_file))
  run(what, "render_report.R", paste0("--job=", quote_arg(job_file)))
}
if (steps[["internal_report"]]) render("Internal report (HTML)", "report-internal.qmd", "html", "internal")
if (steps[["external_report"]]) render("External report (PDF)", "report-external.qmd", "pdf", "external")

cat("\nDone. The results are in", out, "\n")
