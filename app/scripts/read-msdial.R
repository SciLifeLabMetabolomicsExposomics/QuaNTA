# ------------------------------------------------------------------------------------------- #
# Parse a MS-Dial file and save the data to an `rds` file
# NOTE: This script is designed to work with the utility functions in `projlib/msdial_utils.R`.
#   Please check the functions in the chapter "Variables used in `read-msdial.R`" of the file
#   whenever this script is modified.
# This script stops if there is ... in the input file.
# - no blank
# - no Cal0
# - problem in calibration sample ID e.g. "Cal3-2-1"
# - no internal standard
# - no missing `Batch ID`
# ------------------------------------------------------------------------------------------- #

# Handle command line options     ---------------
# The options are used to control the processing steps
option_list <- rlang::list2(
  optparse::make_option(
    c("--per_batch"), type = "logical", default = TRUE,
    help = "Process data per batch? [default: %default]"
  ),
)
opt_parser <- optparse::OptionParser(
  option_list = option_list,
  usage = "Usage: %prog [options]",
  description = "Read MS-Dial output files and save the results."
)
params <- optparse::parse_args(opt_parser)
cat(
  "\nRead MS-Dial parameters:\n",
  "  - Per batch processing:", params$per_batch, "\n"
)

# Load packages and project local libraries
options(box.path = "app/")           # Path to project local libraries
box::use(
  projlib/msdial,   # Handle MS-Dial files
  io = projlib/check_io_exist,       # Check input/output files
  util = projlib/msdial_utils,   
)

# Get the input file name provided by the user
user_inputs <- msdial$get_user_input("input_file", "intermediate_dir", "concentration_unit")
# Input/Output files
FILE <- list(
  i = user_inputs$input_file,
  # Output file name, the base name of the input file with .rds extension
  o = msdial$get_raw_data_file_name(user_inputs)
)
# Check if input files and output directories exist
io$check_io_exist(FILE)

print(io$check_io_exist(FILE))
print(FILE)
# "non-target mode": When there is missing information for calibration of targeted chemicals
is_non_target_mode <- FALSE

# Parse the Data into Elements of SumExp -------------------------------------------------

# The input file contains the data in a table format described as below. 
# 
# | Lines    |      1st     |       2nd        | 3rd column section |
# | :------- | :----------: | :--------------: | :----------------: |
# | 1st-4th  |    *empty*   |   Sample info    |      *empty*       |
# | From 5th | Feature info | Measurement data |    Extra stats     |

i_three_sections <- msdial$get_three_section_indices(FILE$i)
# Sample Info
sample_info <- msdial$fetch_sample_info(FILE$i, i_three_sections[["2nd"]])

# Feature Data

# util$conv_tbl <- tibble::tribble(
#   ~given_id,         ~id,            ~label,
#   "Alignment ID",    "alignment_id", "Alignment ID",
#   "Average Rt(min)", ".rt",          "Average Retention Time (min)",
#   "Comment",         ".std_type",    "Standard Type",      # e.g. "Quant", "IS", "vIS", or NA
#   ...
# )
to_valid_id <- function(given_id, tbl = util$conv_tbl) {
  # `match` to allow unknown IDs
  m <- match(given_id, tbl$given_id)
  ifelse(is.na(m), given_id, tbl$id[m])
}
features <- msdial$fetch_data_of_columns(FILE$i, i_three_sections[["1st"]])
# Identify features to be removed (Comment == "R")
i_to_keep <- which(is.na(features$Comment) | features$Comment != "R")
features <- features[i_to_keep, ]
# Duplicated "id"s for different type of data
conv_tbl <- util$conv_tbl[util$conv_tbl$given_id %in% colnames(features), ]
stopifnot(anyDuplicated(conv_tbl$id) == 0)
# Syntactically valid column names in R
features <- dplyr::rename_with(features, to_valid_id) |> 
  dplyr::select(        # Required, many other columns are not used
    alignment_id,
    feature_name,
    mz,
    .rt,
    .std_type,
  ) |> 
  as.data.frame()        # To have row names

# Syntactically valid ID
rownames(features) <- make.names(features$feature_name, unique = TRUE)
features <- features |> 
  dplyr::mutate(
    dplyr::across(c(mz, .rt), as.numeric),
    # Ignore other variants
    .std_type = dplyr::case_when(
      .std_type %in% c("Quant", "IS", "vIS") ~ .std_type,
      grepl("^IS\\\\", .std_type) ~ .std_type,
      grepl("^Quant\\\\", .std_type) ~ .std_type,
      .default = ""
    )
  )
# Apply labels for plots and tables
conv_tbl <- conv_tbl[match(colnames(features), conv_tbl$id), ]  # To make sure the order is right.
features <- labelled::set_variable_labels(features, .labels = conv_tbl$label)
stopifnot("`IS` features are required." = any(util$is_internal_std(features$.std_type)))
if (!any(util$is_targeted_feature(features$.std_type))) {
  warning("`Quant` features are missing.")
  is_non_target_mode <- TRUE
}

# Measured values of the features into a matrix
raw_df <- msdial$fetch_data_of_columns(FILE$i, i_three_sections[["2nd"]])
raw_df <- raw_df[i_to_keep, ]
stopifnot(identical(colnames(raw_df), labelled::remove_labels(sample_info$sample_name)))
colnames(raw_df) <- rownames(sample_info)     # Update with syntactically valid names
raw_mat <- lapply(raw_df, as.numeric) |> 
  as.data.frame(row.names = rownames(features)) |> 
  as.matrix()
labelled::label_attribute(raw_mat) <- "Raw"

cat("From the given file:", FILE$i, "\n",
    "Number of samples:", ncol(raw_mat), "\n",
    "Number of features:", nrow(raw_mat), "\n")

# Special control sample categories ------------------------------------------------------

sample_info <- sample_info |> 
  dplyr::mutate(
    # Control sample categories
    .ctrl_cat = dplyr::case_when(
      sample_type == "Standard" & 
        stringr::str_detect(sample_name, "Cal[[:digit:]]") ~ "CalCurve",
      sample_type == "QC"    ~ "QC",
      sample_type == "Blank" ~ "Blank",
      TRUE ~ ""         # # NA does not behave predictably with `==`
    ) |> 
      labelled::set_label_attribute("Control Sample Category")
  )

sample_info <- sample_info |> 
  dplyr::mutate(
    injection_order = as.integer(injection_order) |> 
      labelled::copy_labels_from(injection_order),
    batch_id = as.integer(batch_id) |> 
      labelled::copy_labels_from(batch_id),
  )

batch_ids <- unique(sample_info$batch_id)
stopifnot("`Batch ID` must not be NA." = all(!is.na(batch_ids)))
if (length(batch_ids) > 1) {
  cat("Multiple batches are detected: ", paste(batch_ids, collapse = ", "), "\n")
  # If not processing per batch, assign all samples to a single batch
  if (! params$per_batch) {
    cat("Processing per batch is disabled. Assigning all samples to a single batch, `all`.\n")
    sample_info <- sample_info |> 
      dplyr::mutate(
        org_batch_id = batch_id,
        batch_id = "all"
      )
    batch_ids <- "all"
  }
}
for (i_batch in batch_ids) {
  sinfo_b <- sample_info[sample_info$batch_id == i_batch, ]
  if (!any(sinfo_b$.ctrl_cat == "Blank")) {
    stop("`Blank` samples are required in batch ", i_batch)
  }
  for (catx in c("CalCurve", "QC")) {
    if (!any(sinfo_b$.ctrl_cat == catx)) {
      warning("`", catx, "` samples are missing in batch ", i_batch)
      is_non_target_mode <- TRUE
    }
  }
}

if (is_non_target_mode) {
  warning("Hence, the processing of the data will be stopped before calibration!")
} else {
  # Function to identify concentration values from the given IDs
  catch_concentration <- function(sid) {
    sid |> 
      stringr::str_extract("Cal([[:digit:]][[:digit:]-]*)", group = 1) |> 
      stringr::str_replace("-", ".") |>     # Replace "-" with "."
      as.numeric()
  }
  # Known concentration of calibration curves, which have been saved into the `sample_info`
  nm_c <- util$spiked_conc_pts_name
  sample_info[[nm_c]] <- ifelse(sample_info$.ctrl_cat == "CalCurve", 
                                catch_concentration(sample_info$sample_name), 
                                NA_real_) |> 
    labelled::set_label_attribute("Calibrant Concentration")
  cc_conc <- sample_info[[nm_c]][sample_info$.ctrl_cat == "CalCurve"]
  stopifnot(
    "Error in Calibration sample IDs" = all(!is.na(cc_conc)), 
    "Zero concentration is required." = any(cc_conc == 0)
  )
  has_single_cal_pt <- any(table(cc_conc) == 1)
}

# Into "SumExp" class
sumexp <- SumExp::SumExp(
  raw = raw_mat,
  col_df = sample_info,
  row_df = features,
  metadata = rlang::list2(
    file_name = basename(FILE$i),
    file_md5 = digest::digest(FILE$i, algo = "md5", file = TRUE),
    is_non_target_mode = is_non_target_mode,
    concentration_unit = user_inputs$concentration_unit,
    has_single_cal_pt = if (is_non_target_mode) FALSE else has_single_cal_pt,
  )
)

# SAVE -----

# Save the "SumExp" object
saveRDS(sumexp, file = FILE$o)
cat("`SumExp` object saved to:", FILE$o, "\n")
