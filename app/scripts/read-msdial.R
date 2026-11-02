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
# The `processing:` section of the parameters file (e.g. saved in the app) for the options not
# given on the command line (projlib/settings.R)
options(box.path = Sys.getenv("R_BOX_PATH", "app/"))
box::use(settings = projlib/settings)
params <- settings$options_with_parameters_file(params, "read_msdial")
cat(
  "\nRead parameters:\n",
  "  - Per batch processing:", params$per_batch, "\n"
)

# Load packages and project local libraries
options(box.path = Sys.getenv("R_BOX_PATH", "app/"))          # Path to project local libraries
box::use(
  projlib/msdial,   # Handle MS-Dial files
  io = projlib/check_io_exist,       # Check input/output files
  util = projlib/msdial_utils,   
  show = projlib/show,   # Counts of the value distribution (app 2.6)
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
# Required columns must be present exactly once with exact names (stops with a message otherwise)
msdial$check_feature_columns(FILE$i, i_three_sections[["1st"]])
# Columns similar to a required one but not used (e.g. "comment"): shown before processing
column_notes <- msdial$similar_feature_columns(FILE$i, i_three_sections[["1st"]])
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
comment_given <- trimws(as.character(features$.std_type))   # The Comment as given
features <- features |>
  dplyr::mutate(
    dplyr::across(c(mz, .rt), as.numeric),
    # Only the exact naming conventions; anything else is a non-targeted feature
    .std_type = dplyr::case_when(
      comment_given %in% c("Quant", "IS", "vIS") ~ comment_given,
      grepl("^IS\\\\", comment_given) ~ comment_given,
      grepl("^Quant\\\\", comment_given) ~ comment_given,
      .default = ""
    )
  )
# Comment values that do not follow the naming conventions (shown before processing): they are
# non-targeted features. `looks_like`: the convention it resembles (a whole word, in any case but R;
# "vIS" before "IS"), e.g. "IS - bad" looks like IS but is not used as one; `NA` if none
exact <- comment_given %in% c("Quant", "IS", "vIS") | grepl("^(IS|Quant)\\\\", comment_given)
odd <- !is.na(comment_given) & nzchar(comment_given) & !exact
comment_notes <- tibble::tibble(
  comment = comment_given[odd],
  # Quant / IS / vIS also in other cases (e.g. "quant", "Is"), as likely typos; R only as given
  looks_like = dplyr::case_when(
    grepl("\\bvIS\\b", comment, ignore.case = TRUE) ~ "vIS",
    grepl("\\bQuant\\b", comment, ignore.case = TRUE) ~ "Quant",
    grepl("\\bIS\\b", comment, ignore.case = TRUE) ~ "IS",
    grepl("\\bR\\b", comment) ~ "R",
    .default = NA_character_
  )
) |>
  dplyr::count(comment, looks_like)
if (nrow(comment_notes) > 0) {
  cat("Comment values that do not follow the naming conventions (IS, IS\\<compound>, Quant,",
      "Quant\\<IS>, vIS, R); these features are non-targeted:\n",
      paste0("  - \"", comment_notes$comment, "\" (", comment_notes$n, ")",
             ifelse(is.na(comment_notes$looks_like), "", paste0(", looks like ", comment_notes$looks_like)), "\n"),
      sep = "")
}
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
# Empty or non-numeric values (not in an export as such, e.g. a file edited by hand): read as 0 (not
# detected), and listed before processing (missing values would stop the processing)
na_cells <- is.na(raw_mat)
missing_values <- tibble::tibble(feature = features$feature_name[row(raw_mat)[na_cells]]) |>
  dplyr::count(feature)
if (any(na_cells)) {
  raw_mat[na_cells] <- 0
  cat(sum(na_cells), "empty or non-numeric value(s) were read as 0 (not detected), in",
      nrow(missing_values), "feature(s):", paste(utils::head(missing_values$feature, 10), collapse = ", "),
      if (nrow(missing_values) > 10) "...", "\n")
}

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

# Input errors the user must fix in the file ---------------------------------------------
# Collected and saved to `input_errors.txt` in the intermediate folder (the app shows them in a
# popup); the read data is then not saved, so the data cannot be processed
input_errors <- character(0)
# 1. The same injection number twice in a batch
inj_key <- paste(sample_info$batch_id, sample_info$injection_order)
dup_inj <- unique(inj_key[duplicated(inj_key) & !is.na(sample_info$injection_order)])
for (k in dup_inj) {
  i <- which(inj_key == k)
  input_errors <- c(input_errors, sprintf(
    "Injection order %s is used more than once in batch %s (samples: %s).",
    sample_info$injection_order[i[1]], sample_info$batch_id[i[1]],
    paste(sample_info$sample_name[i], collapse = ", ")))
}
# 2. The same name for two internal standards (also volumetric ones)
is_names <- features$feature_name[util$is_internal_std(features$.std_type) | features$.std_type == "vIS"]
for (nm in unique(is_names[duplicated(is_names)])) {
  input_errors <- c(input_errors, sprintf(
    "The internal standard name \"%s\" is used for %d features.", nm, sum(is_names == nm)))
}
# 3. A class used both for QC samples and for study samples
qc_cls <- unique(sample_info$Class[sample_info$.ctrl_cat == "QC"])
study_cls <- unique(sample_info$Class[sample_info$.ctrl_cat == ""])
for (cl in intersect(qc_cls, study_cls)) {
  input_errors <- c(input_errors, sprintf(
    "The class \"%s\" is used both for QC samples and for study samples.", cl))
}
# 4. A missing (or non-numeric) retention time or m/z: the internal standards are matched to the
# features by retention time
for (col in c(".rt", "mz")) {
  miss <- features$feature_name[is.na(features[[col]])]
  if (length(miss) > 0) {
    input_errors <- c(input_errors, sprintf(
      "The %s is empty or not a number for %d feature(s): %s.",
      if (col == ".rt") "retention time (Average Rt(min))" else "m/z", length(miss),
      paste0(paste(utils::head(miss, 10), collapse = ", "), if (length(miss) > 10) ", ..." else "")))
  }
}
errors_file <- file.path(user_inputs$intermediate_dir, "input_errors.txt")
if (length(input_errors) > 0) {
  writeLines(input_errors, errors_file)
  stop("The input file has ", length(input_errors), " error(s) that must be fixed:\n",
       paste("-", input_errors, collapse = "\n"), call. = FALSE)
}
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
  # Blank samples are not required per batch: a batch without them is blank-subtracted with the
  # average of all blank samples (its calibration is not affected)
  if (!any(sinfo_b$.ctrl_cat == "Blank")) {
    cat("No `Blank` samples in batch ", i_batch,
        ". The average of all blank samples will be subtracted.\n", sep = "")
  }
  # Neither QC nor calibration curve samples are required per batch:
  # - Without QC samples, the evaluations using QC samples are skipped.
  # - Without calibration curve samples, the batch is quantified by the global calibration curve.
  if (!any(sinfo_b$.ctrl_cat == "QC")) {
    cat("No `QC` samples in batch ", i_batch, ". The QC-based evaluations will not include it.\n", sep = "")
  }
  if (!any(sinfo_b$.ctrl_cat == "CalCurve")) {
    cat("No `CalCurve` samples in batch ", i_batch,
        ". It will be quantified using the global calibration curve.\n", sep = "")
  }
}
# Blank filtering and subtraction require blank samples in at least one batch
if (!any(sample_info$.ctrl_cat == "Blank")) {
  stop("`Blank` samples are required (in at least one batch).")
}
# Calibration requires calibration curve samples in at least one batch
if (!any(sample_info$.ctrl_cat == "CalCurve")) {
  warning("`CalCurve` samples are missing.")
  is_non_target_mode <- TRUE
}

if (is_non_target_mode) {
  # warning("Hence, the processing of the data will be stopped before calibration!")
  # (Not empty: an empty `{}` would make Rscript print "NULL" in the log)
  invisible(NULL)
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
    comment_notes = comment_notes,   # Comment values that do not follow the naming conventions
    column_notes = column_notes,     # Columns similar to a required column (not used)
    missing_values = missing_values,   # Empty or non-numeric values read as 0, per feature
    # Histogram counts of all raw values (app 2.6), counted once here instead of in the app
    value_bins = show$value_distribution_bins(raw_mat),
  )
)

# SAVE -----

# Save the "SumExp" object
saveRDS(sumexp, file = FILE$o)
cat("`SumExp` object saved to:", FILE$o, "\n")
