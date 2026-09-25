# ------------------------------------------------------------------------------------------- #
# Shared setup chunk used across multiple Quarto markdown files for `report-ineternal.qmd`
# - Requires:
#   - `params$norm_method`
# ------------------------------------------------------------------------------------------- #

# Load packages and project local libraries
app_root <- Sys.getenv("APP_ROOT", "app")
setup_file <- file.path(app_root, "reports", "chapters", "_internal-shared_setup_chunk.R")
options(box.path = Sys.getenv("R_BOX_PATH", "app/")) # Path to project local libraries
box::use(
  SumExp, # Light SummarizedExperiment, to use `[`
  util = projlib / msdial_utils, # Utility functions for MS-Dial data
  projlib / msdial, # Handle MS-Dial files
  projlib / show, # Functions to present data
  ggplot2[...], # Too long to write and usually specific enough
  io = projlib / check_io_exist # Check input/output files
)

# The ID of the matrix to use in calibration
MAT_ID_BLANK_SUBT <- params$norm_method |>
  util$mat_id_of_blank_subtracted()
MAT_ID_IN_CALIB <- util$mat_id_in_calibration(MAT_ID_BLANK_SUBT)

# Get the input file name provided by the user
user_inputs <- msdial$get_user_input()
# To keep constants consistent across reports
constants_yml <- yaml::read_yaml(paste0(app_root, "/constants.yml"))
COLORS <- lapply(constants_yml$COLORS, unlist) # Lists to named vectors
raw_se <- msdial$read_parsed_msdial_data(user_inputs)
IS_TARGET_MODE <- !SumExp::metadata(raw_se)$is_non_target_mode
# For consistency throughout the report
COLORS_OF_CLASSES <- show$get_colors_of_classes(raw_se, COLORS$.ctrl_cat)

# Input/Output files
FILE <- list(
  i = rlang::list2(
    # Intermediate status of the data
    to_rep = msdial$get_raw_data_file_name(user_inputs, suffix = "to_report"),
  )
)
if (IS_TARGET_MODE) {
  # Processed data
  FILE$i$proc <- msdial$get_raw_data_file_name(user_inputs, suffix = "proc")
  io$check_io_exist(FILE)

  # Load the processed data using the specified normalization method
  lst_proc <- readRDS(FILE$i$proc)[[MAT_ID_BLANK_SUBT]]
} else {
  io$check_io_exist(FILE)
}

# Load the intermediate data during QC
to_report <- readRDS(FILE$i$to_rep)
stopifnot("Processing NOT completed." = to_report[["Completed"]])
pre_norm_se <- to_report[["before normalization"]]

# Conversion tables between IDs (syntactically acceptable) and names (human-readable)
# These are used for presenting tables and figures in reports
sample_id_name_tbl <- SumExp::col_df(raw_se) |>
  tibble::rownames_to_column("sample_id") |>
  dplyr::select(sample_id, sample_name)
feature_id_name_tbl <- SumExp::row_df(raw_se) |>
  tibble::rownames_to_column("feature_id") |>
  dplyr::select(feature_id, feature_name)
