# ------------------------------------------------------------------------------------------- #
# Server-side setup: shared reactives that load results of the child processes.
#
# IMPORTANT: this file is sourced INSIDE `server()` (source(..., local = TRUE)), after
#   `ws`, `data_info`, `read_done` and `proc_done` have been created. It must NOT live in `R/`
#   (that folder is auto-sourced globally at app start-up).
#
# Design: the child processes write .rds files into the session workspace. Each file is read
# ONCE per run into a cached reactive; `read_done` / `proc_done` invalidate the cache when a new
# run has finished. All outputs share the cached objects instead of re-reading the files.
# ------------------------------------------------------------------------------------------- #

box::use(
  SumExp, # Light SummarizedExperiment, to use `[`
  util = projlib / msdial_utils, # Utility functions for MS-Dial data
  projlib / msdial, # Handle MS-Dial files
  projlib / show, # Functions to present data
  projlib / history, # What happened to each feature and sample
  ggplot2[...], # Too long to write and usually specific enough
  io = projlib / check_io_exist, # Check input/output files
)

# To keep constants consistent across reports
constants_yml <- yaml::read_yaml("constants.yml")
COLORS <- lapply(constants_yml$COLORS, unlist) # Lists to named vectors

# What `msdial$get_raw_data_file_name()` / `read_parsed_msdial_data()` need (absolute paths)
current_inputs <- shiny::reactive({
  list(input_file = data_info[["input_file"]], intermediate_dir = ws$intermediate)
})
rds_file <- function(suffix) msdial$get_raw_data_file_name(current_inputs(), suffix = suffix)

# Shown by every table and figure that has no data yet (instead of an empty space)
MSG_NOT_READ <- "No data yet: upload the data file and click \"Read\" on page 1 (Input data and processing)."
MSG_NOT_PROCESSED <- "Not processed yet: click \"Process data\" on page 1 (Input data and processing)."

# Parsed data (output of read-msdial.R)
raw_se_r <- shiny::reactive({
  read_done()
  results_reset()   # All results removed (e.g. a parameters file was loaded)
  input[["data_info-input_file_upload"]]   # A new upload deletes the data (also with the same name)
  has_input <- isTRUE(nzchar(data_info[["input_file"]]))   # `NULL` before an upload
  shiny::validate(shiny::need(has_input && file.exists(rds_file("")), MSG_NOT_READ))
  # Features, samples and metadata: the peak areas of all features are not kept in the app
  msdial$read_parsed_data_info(current_inputs())
})

# For consistency throughout the report
colors_of_classes <- shiny::reactive({
  show$get_colors_of_classes(raw_se_r(), COLORS$.ctrl_cat)
})

# Conversion tables between IDs (syntactically acceptable) and names (human-readable)
sample_id_name_tbl <- shiny::reactive({
  se <- raw_se_r()   # Not inside `col_df()`: S4 dispatch turns the validate() message into an error
  SumExp::col_df(se) |>
    tibble::rownames_to_column("sample_id") |>
    dplyr::select(sample_id, sample_name)
})
feature_id_name_tbl <- shiny::reactive({
  se <- raw_se_r()
  SumExp::row_df(se) |>
    tibble::rownames_to_column("feature_id") |>
    dplyr::select(feature_id, feature_name)
})

# Intermediate data of the quality-control steps (output of process.R), read once per run
to_report_r <- shiny::reactive({
  proc_done()
  results_reset()
  read_done()                              # Re-reading deletes the processed data (see app.R)
  input[["read_msdial-run_button"]]        # ... from the moment the read is started
  input[["data_info-input_file_upload"]]   # A new upload deletes it too
  shiny::validate(shiny::need(isTRUE(nzchar(data_info[["input_file"]])), MSG_NOT_READ))
  # The app's own copy (process.R): only what the app shows, and the data prepared for its figures
  # (much smaller than the full results, which the reports read). Data processed by older versions
  # have only the full results.
  path <- rds_file("app")
  if (!file.exists(path)) path <- rds_file("to_report")
  shiny::validate(shiny::need(file.exists(path), MSG_NOT_PROCESSED))
  to_report <- readRDS(path)
  shiny::validate(shiny::need(isTRUE(to_report[["Completed"]]), MSG_NOT_PROCESSED))
  to_report
})
# Data prepared by process.R for the figures and tables (`NULL` for data processed by older versions)
app_data_r <- shiny::reactive(to_report_r()[["app data"]])
# Slices of the cached list, no extra file reading
pre_norm_se_r <- shiny::reactive(to_report_r()[["before normalization"]])
internal_std_se_r <- shiny::reactive(to_report_r()[["internal std. before qc"]])

# Calibrated data (output of process.R, target mode only)
proc_r <- shiny::reactive({
  proc_done()
  results_reset()
  read_done()
  input[["read_msdial-run_button"]]
  input[["data_info-input_file_upload"]]
  shiny::validate(shiny::need(isTRUE(nzchar(data_info[["input_file"]])), MSG_NOT_READ))
  path <- rds_file("proc")
  shiny::validate(shiny::need(file.exists(path), MSG_NOT_PROCESSED))
  readRDS(path)
})
