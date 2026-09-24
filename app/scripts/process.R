# ------------------------------------------------------------------------------------------- #
# Read a parsed MS-Dial file, process, and save the data to an `rds` file
# During processing for quality control, the intermediate data is saved to an `rds` file
# ------------------------------------------------------------------------------------------- #

#  -----  COMMAND LINE OPTIONS  ------------------------------------------------
# The options are used to control the processing steps
option_list <- rlang::list2(
  optparse::make_option(
    c("--optimize_cal_points"),
    type = "logical", default = FALSE,
    help = "Optimize cal curves to exclude points outside sample range [default: %default]"
  ),
  optparse::make_option(
    c("--min_cal_points"),
    type = "integer", default = 3,
    help = "The mininum required number to keep a calibration curve [default: %default]"
  ),
  optparse::make_option(
    c("--rm_outlier"),
    type = "logical", default = TRUE,
    help = "Quality control: remove outlier samples [default: %default]"
  ),
  optparse::make_option(
    c("--log_calibration"),
    type = "logical", default = FALSE,
    help = "Log scale for calibration curve fitting [default: %default]"
  ),
  optparse::make_option(
    c("--weight"),
    type = "character", default = "largestR2",
    help = paste(
      "Weighting method for calibration curve fitting.",
      "Possible values are: `largestR2`, `1`, `1_div_x`, `1_div_x2`.",
      "[default: %default]"
    )
  ),
  optparse::make_option(
    c("--llox_method"),
    type = "character", default = "pt_signal_mean",
    help = "Method to compute LOD/LLOQ"
  ),
  optparse::make_option(
    c("--use_rsd20"),
    type = "logical", default = TRUE,
    help = "Use RSD% 20% threshold for LLOQ [default: %default]"
  ),
  optparse::make_option(
    c("--blank_filter"),
    type = "logical", default = TRUE,
    help = "Quality control: blank filtering [default: %default]"
  ),
  optparse::make_option(
    c("--blank_filter_factor"),
    type = "double", default = 5,
    help = "Blank filtering factor [default: %default]"
  )
)
opt_parser <- optparse::OptionParser(
  option_list = option_list,
  usage = "Usage: %prog [options]",
  description = "Process MS-Dial parsed data and save the results."
)
params <- optparse::parse_args(opt_parser)
cat(
  "\nProcessing parameters:\n",
  "  - Optimize cal points: ", params$optimize_cal_points, "\n",
  "  - Minimum number of cal points: ", params$min_cal_points, "\n",
  "  - Outlier removal:", params$rm_outlier, "\n",
  "  - Log scale for calibration:", params$log_calibration, "\n",
  "  - Weight method:", params$weight, "\n",
  "  - LOD/LLOQ method: ", params$llox_method, "\n",
  "  - Use RSD% 20% for LLOQ: ", params$use_rsd20, "\n",
  "  - Blank filtering: ", params$blank_filter, "\n",
  "  - Blank filtering factor: ", params$blank_filter_factor, "\n"
)

#  -----  LIBRARIES AND FILES  -------------------------------------------------
# Load packages and project local libraries
options(box.path = "app/") # Path to project local libraries
box::use(
  SumExp, # Light SummarizedExperiment, `[`
  projlib / msdial, # Handle MS-Dial files
  util = projlib / msdial_utils, # Utility functions for MS-Dial data
  projlib / proc, # Processing functions
)
# Get the input file name provided by the user
user_inputs <- msdial$get_user_input("input_file", "intermediate_dir")

# Output files
FILE <- rlang::list2(
  # Processed data
  proc = msdial$get_raw_data_file_name(user_inputs, suffix = "proc"),
  # Intermediate status of the data for reporting
  to_rep = msdial$get_raw_data_file_name(user_inputs, suffix = "to_report"),
)
cat("\nProcessing steps are started.\n")
# Read the parsed data by `read-msdial.R`. If not available, warn the user
# This 'proc_sumexp' is the main data object that will undergo processing.
proc_sumexp <- msdial$read_parsed_msdial_data(user_inputs)

# no calibration points model
IS_NON_TARGET_MODE <- SumExp::metadata(proc_sumexp)$is_non_target_mode
stopifnot(!is.null(IS_NON_TARGET_MODE))

# For reports, store intermediate data during quality control steps.
# The list `to_report` contains:
# - `params`: Command-line options for the processing
# - `Completed`: A logical flag indicating whether the processing was completed
# - `is_non_target_mode`: A logical flag indicating if it is non-target mode
# - `blank_filter_factor`: The factor used for blank filtering (default: 5)
# - `before normalization`: `SumExp` object of all data before internal standard normalization
# - `internal std. before qc`: `SumExp` object of internal standard features before QC
# - `is failed internal std.`: A logical vector flagging failed internal standards.
#       length() == nrow(`internal std. before qc`)
# - `number of outlier internal std. per sample`: Count of outlying internal standards per sample
#       length() == ncol(`internal std. before qc`)
# - `is outlier sample`: A logical vector flagging outlier samples
#       length() == ncol(`internal std. before qc`)
# - `excluded categories in normalization`: Sample categories excluded from normalization
# - `LOESS fit`: A list of LOESS models for each sample. Output of `proc$get_loess_fit()`
# - `normalized`: `SumExp` object after normalization (before blank filtering)
# - `after blank filtering`: `SumExp` object after blank filtering
# - `matrix ids to blk subt`: IDs of matrices that underwent blank subtraction
# - `normalized - blank`: A list of `SumExp` objects after blank subtraction, split by batch
to_report <- rlang::list2(
  "params" = params,
  "Completed" = FALSE,
  "is_non_target_mode" = IS_NON_TARGET_MODE,
  "blank_filter" = params$blank_filter,
  "blank_filter_factor" = params$blank_filter_factor,
)


#  -----  NORMALIZATION USING INTERNAL STANDARDS  ------------------------------

to_report[["before normalization"]] <- proc_sumexp

# Extract internal standard features
internal_std_se <- local(
  envir = list(se = proc_sumexp),
  {
    se <- se[util$is_internal_std(se), ]
    se[order(util$retention_time(se)), ] # Sort by average retention time
  }
)
# For reports, store intermediate data during quality control steps.
to_report[["internal std. before qc"]] <- internal_std_se

##  FAILED INTERNAL STANDARDS  -----------------------------

num_zeros <- proc$count_zeros_per_feature(internal_std_se[["raw"]])
# Mark features with > 10% zeros
is_failed <- num_zeros > ncol(internal_std_se) * 0.1
to_report[["is failed internal std."]] <- is_failed
# Remove failed internal standard features
is_failed_in_proc_sumexp <- rownames(proc_sumexp) %in% rownames(internal_std_se)[is_failed]
proc_sumexp <- proc_sumexp[!is_failed_in_proc_sumexp, ]
internal_std_se <- internal_std_se[!is_failed, ]
# Impute remaining zeros with the mean of the same type
internal_std_se <- proc$impute_zeros_with_mean_of_same_type(internal_std_se, "raw")
cat("Internal standards check done (N = ", sum(is_failed), " failed have been removed).\n", sep = "")

##  OUTLIER SAMPLE REMOVAL  --------------------------------

if (params$rm_outlier) { # Controlled by the command line option
  # Number of outlying internal standard features per sample
  n_out <- proc$count_outliers_per_sample(internal_std_se, "raw", times = 3)
  n_internal_std <- nrow(internal_std_se)
  # Outlier samples: number of outlying features > 20% of the total number of features
  is_outlier <- n_out > (0.2 * n_internal_std)
  to_report[["number of outlier internal std. per sample"]] <- n_out
  to_report[["is outlier sample"]] <- is_outlier
  
  if (any(is_outlier)) {
    # Exclude the outlying samples
    stopifnot(identical(names(is_outlier), colnames(internal_std_se)))
    internal_std_se <- internal_std_se[, !is_outlier]
    stopifnot(identical(names(is_outlier), colnames(proc_sumexp)))
    proc_sumexp <- proc_sumexp[, !is_outlier]
    cat("Outlier samples (N = ", sum(is_outlier), ") are removed.\n", sep = "")
  } else {
    cat("No outlier samples are found.\n")
  }
}

##  NORMALIZE - CLOSEST INTERNAL STANDARD FEATURES  --------
proc_sumexp <- list(se = proc_sumexp, mat_id = "raw", istd_se = internal_std_se, proc = proc) |>
  list2env(parent = baseenv()) |> # To make sure all required objects are listed here
  local(envir = _, { # Control scope, like a function
    closest_istd <- proc$get_data_of_closest_istd( # @returns mat, feat_nm, rt
      se = se,
      istd_se = istd_se,
      mat_id = mat_id,
      verbose = TRUE
    )
    se[["closest_norm"]] <- (se[[mat_id]] / closest_istd$mat) |>
      labelled::set_variable_labels("Closest RT normalized")
    SumExp::row_df(se)[["closest_istd"]] <- closest_istd$feat_nm
    SumExp::row_df(se)[["closest_istd_rt"]] <- closest_istd$rt
    return(se)
  })
cat("Closest internal standard normalization is done.\n")

##  NORMALIZE - LOESS FIT OVER RT  -------------------------

overall_rt_range <- range(util$retention_time(proc_sumexp)) # Fit for RT of all features
excl_cat <- c("Blank", "CalCurve")
to_report[["excluded categories in normalization"]] <- excl_cat

loess_fit <- proc$get_loess_fit(
  istd_se = internal_std_se,
  excl_cat = excl_cat,
  overall_rt_range = overall_rt_range,
  span = 1,
  mat_id = "raw"
)
to_report[["LOESS fit"]] <- loess_fit
# Normalize the data by LOESS fit along RT
rt <- util$retention_time(proc_sumexp)
proc_sumexp[["loess_norm"]] <- sapply(
  colnames(proc_sumexp), function(sample_id) {
    norm_factor <- predict(loess_fit[[sample_id]], newdata = rt)
    exp(log(proc_sumexp[["raw"]][, sample_id]) - norm_factor) # Normalize in log scale
  }
) |>
  labelled::set_variable_labels("LOESS normalized")
cat("LOESS normalization is done.\n")

##  END OF INTERNAL STANDARD PROCESSING  -------------------
to_report[["normalized"]] <- proc_sumexp # Final internal standard normalized data
proc_sumexp <- proc_sumexp[!util$is_internal_std(proc_sumexp), ] # Remove internal standards


#  -----  BLANK FILTERING  ----------------------------------------------------

# Matrix IDs to perform blank subtraction. Raw + normalized matrices
to_blk_subt <- c("raw", "loess_norm", "closest_norm")
to_report[["matrix ids to blk subt"]] <- to_blk_subt

if (params$blank_filter) {
  for (mat_id in to_blk_subt) {
    cat("Performing blank filtering for matrix:", mat_id, "\n")
    proc_sumexp <- proc$filter_by_blank(
      proc_sumexp,
      mat_id = mat_id,
      factor = params$blank_filter_factor
    )
  }
  to_report[["after blank filtering"]] <- proc_sumexp
}


#  -----  BLANK SUBTRACTION  ---------------------------------------------------

mat_ids_after_blk_subt <- util$mat_id_of_blank_subtracted(to_blk_subt)

# Get batch IDs for per-batch processing
batch_ids <- as.character(SumExp::col_df(proc_sumexp)[["batch_id"]])
has_multiple_batches <- dplyr::n_distinct(batch_ids) > 1
if (has_multiple_batches) {
  cat(
    "Processing blank subtraction per batch.",
    "Found batch_ids:", paste(unique(batch_ids), collapse = ", "), "\n"
  )
} else {
  cat("Processing blank subtraction for a single batch.\n")
}
# Split the data by batch IDs
per_batch_proc_se_lst <- SumExp::split_columns(proc_sumexp, batch_ids)
# Per batch, then per normalization method
for (batch_id in unique(batch_ids)) {
  se <- per_batch_proc_se_lst[[batch_id]]
  per_batch_proc_se_lst[[batch_id]] <- proc$add_blank_subtracted_sumexp(
    sumexp = se,
    no_change = util$ctrl_smpl_cat(se) == "CalCurve",
    mat_ids = to_blk_subt,
    out_mat_ids = mat_ids_after_blk_subt
  )
}
to_report[["normalized - blank"]] <- per_batch_proc_se_lst

# Global blank subtraction for failed batches.
proc_sumexp <- proc$add_blank_subtracted_sumexp(
  sumexp = proc_sumexp,
  no_change = util$ctrl_smpl_cat(proc_sumexp) == "CalCurve",
  mat_ids = to_blk_subt,
  out_mat_ids = mat_ids_after_blk_subt
)
cat("Blank subtraction is done.\n")


#  -----  CALIBRATION USING CALCURVE  ------------------------------------------

#' Mark the expected processing has been completed
mark_completed_and_save_to_report <- function() {
  cat("Processing steps are completed.\n")
  cat("Saving intermediate data during the processing...\n")
  to_report[["Completed"]] <- TRUE
  saveRDS(to_report, file = FILE$to_rep)
  cat("The intermediate data saved to:", FILE$to_rep, "\n")
}
# When the data is produced without any targets (no calibration points), skip calibration
if (IS_NON_TARGET_MODE) {
  warning("NO CALIBRATION under non-target mode.")
  mark_completed_and_save_to_report()
  proc$stop_quietly()
}

if (params$log_calibration) cat("Log scale for calibration curve fitting is applied.\n")
if (has_multiple_batches) {
  cat("Processing calibration per batch.")
} else {
  cat("Processing calibration for a single batch.")
}

##  CALIBRATION FUNCTION  ----------------------------------

# Separated as a function to be used for both per-batch and global-mode calibration
quantify_using_calcurve <- function(qt_se, mat_id_to_calib, params) {
  # Whether to fit in log scale (--log_calibration):
  # - signal (normalized, blank-subtracted): `log(x + 1)` and `exp(x) - 1`
  # - concentration: log(x) / exp(x)
  in_log <- params$log_calibration
  
  # Restrict to targeted features
  qt_se <- qt_se[util$is_targeted_feature(qt_se), ]
  
  # Sort by chemical name for presentation consistency in the reports, exported files, etc.
  qt_se <- qt_se[order(SumExp::row_df(qt_se)$feature_name), ]
  # Preserve the signal matrix label; `log1p()` below may strip the label
  signal_mat_label <- labelled::get_label_attribute(qt_se[[mat_id_to_calib]])
  
  # If calibration curve is fitted in log scale, transform the signal
  if (in_log) qt_se[[mat_id_to_calib]] <- log1p(qt_se[[mat_id_to_calib]]) # log1p(x) = log(x + 1)
  
  # Determine the valid calibration range endpoints and LOD/LLOQ thresholds
  fun <- switch(params$llox_method,
                "pt_signal_mean" = proc$compute_llox_signal_using_mean_times,
                "pt_signal_mean_plus_sd" = proc$compute_llox_signal_using_mean_plus_sd_times
  )
  qt_se <- proc$find_calib_lim_pts_and_llox_from_llox_signal(
    qt_se, mat_id_to_calib, fun, params$use_rsd20,
    min_n = params$min_cal_points,
    enough_n = if (params$optimize_cal_points) 3 else Inf
  )
  
  # Exclude chemicals without a valid calibration range; return early if none qualify
  has_proper_range <- proc$has_proper_calibration_range(qt_se)
  SumExp::row_df(qt_se) <- cbind(SumExp::row_df(qt_se), has_proper_range)
  if (sum(has_proper_range) == 0) { # No chemicals with a valid range
    return(list(done = FALSE, data = qt_se))
  }
  # Copy signal into the calibration matrix, which will be modified during calibration
  # This preserves the original signal matrix for reference and for chemicals without a proper calibration range
  mat_id <- util$mat_id_in_calibration(mat_id_to_calib)
  qt_se[[mat_id]] <- qt_se[[mat_id_to_calib]]
  # Restore "mat_id_to_calib" before splitting
  if (in_log) qt_se[[mat_id_to_calib]] <- expm1(qt_se[[mat_id_to_calib]])
  
  # Split into two parts:
  # - `calcurve_se` : calibration curve samples
  # - `concn_se` : samples to be calibrated
  lst_q_se <- util$split_into_calcurve_and_other(qt_se, c("cc", "concn"))
  calcurve_se <- lst_q_se$cc
  concn_se <- lst_q_se$concn
  
  # Mask calibration points outside the valid concentration range
  calcurve_se <- proc$replace_outside_concentration_range_with_na(calcurve_se, mat_id)
  calcurve_se[[mat_id]] <- util$extract_with_na(calcurve_se[[mat_id]], i = has_proper_range)
  
  # Fit the calibration curve
  e <- list(
    cc_se = calcurve_se, mat_id = mat_id, in_log = in_log, w_method = params$weight,
    util = util, proc = proc
  ) |>
    list2env(parent = baseenv()) # To make sure all required objects are listed here
  calcurve_models <- local(envir = e, { # Isolated scope; intermediates do not leak out
    c_concs <- util$spiked_conc_pts(cc_se)
    if (in_log) {
      c_concs <- log(c_concs) # Signal in [[mat_id]] is already log-transformed; match the scale
      if (w_method != "1") {
        warning("Weighting methods other than `1` are not available under log scale. Using `1`.")
        w_method <- "1"
      }
    }
    out <- lapply(seq_len(nrow(cc_se)), function(ii) {
      proc$fit_and_test_calcurve_model(
        c_concs,
        signal = cc_se[[mat_id]][ii, ],
        weight_method = w_method,
        penalty_quadratic = 0.01
      )
    })
    names(out) <- rownames(cc_se)
    return(out)
  })
  SumExp::row_df(concn_se) <- SumExp::row_df(concn_se) |>
    dplyr::mutate(calcurve_model = calcurve_models)
  if (in_log) calcurve_se[[mat_id]] <- expm1(calcurve_se[[mat_id]]) # expm1(x) = exp(x) - 1
  
  concn_se[["conc0"]] <- proc$compute_concentration(concn_se, mat_id, log_scale = in_log) |>
    labelled::set_label_attribute("Concentration before filtering")
  # [[conc0]] is the unfiltered reference; [[conc]] will be NA-replaced below
  concn_se[["conc"]] <- concn_se[["conc0"]]
  concn_se <- util$save_src_mat_id_for_conc(concn_se, mat_id)
  # Set concentrations below LOD/LLOQ to NA; rows are kept intact to preserve data structure
  concn_se <- concn_se |>
    proc$replace_below_lod_lloq(conc_mat_id = "conc") |>
    proc$replace_conc_whose_signal_below_lloq(conc_mat_id = "conc") |>
    proc$replace_conc_whose_signal_above_lloq(conc_mat_id = "conc") |>
    proc$replace_conc_whose_signal_below_lod(conc_mat_id = "conc") 
  labelled::label_attribute(concn_se) <- signal_mat_label
  if (in_log) concn_se[[mat_id]] <- expm1(concn_se[[mat_id]]) # Restore to original scale
  
  # Flag chemicals where no study sample exceeded the LLOQ; these are excluded from exports
  non_qc_conc <- util$exclude_ctrl_smpl_cat(concn_se, "QC")[["conc"]]
  any_above_lloq <- non_qc_conc > SumExp::row_df(concn_se)[, "lloq"]
  has_ex <- rowSums(any_above_lloq) > 0
  has_ex[is.na(has_ex)] <- FALSE
  SumExp::row_df(concn_se)[["to_export"]] <- has_ex
  # Set concentrations of non-exported chemicals to NA
  unit <- SumExp::metadata(concn_se)$concentration_unit
  concn_se[["conc"]] <- util$extract_with_na(concn_se[["conc"]], i = has_ex, j = TRUE) |>
    labelled::set_label_attribute(paste0("Concentration [", unit, "]"))
  
  return(list(
    done = TRUE,
    calcurve = calcurve_se, # Calibration-curve samples with range metadata; no fitted models
    concn = concn_se # Quantified samples with fitted models, LOD/LLOQ thresholds, and export flags
  ))
}

##  CALIBRATION  -------------------------------------------

# Collect the output. "mat_id_to_calib" -> "batch_id" -> list of "calcurve" and "concn"
# - "mat_id_to_calib" indicates the ID of the matrix to use for calibration
# - "calcurve" is a SumExp object of the calibration curve
# - "concn" is a SumExp object having calibrated concentrations
lst_proc <- list()

# Per normalization method, then per batch
for (mat_id_to_calib in mat_ids_after_blk_subt) {
  lst_proc[[mat_id_to_calib]] <- list() # Initialize to collect the output per normalization method
  # Label for the normalized data.
  norm_lab <- labelled::get_label_attribute(proc_sumexp[[mat_id_to_calib]])
  cat(glue::glue("Processing {norm_lab} data."), "\n")
  
  # Global calibration for failed batches.
  cal_global <- quantify_using_calcurve(proc_sumexp, mat_id_to_calib, params)
  
  if (cal_global$done) {
    # In batch-mode, it can be "Global" or "Batch" or NA
    SumExp::row_df(cal_global$calcurve)[["src_calcurve"]] <- dplyr::if_else(
      SumExp::row_df(cal_global$calcurve)[, "has_proper_range"], "Global", NA_character_
    )
    # Progress message
    cat(glue::glue("  Global calibration using all samples is done."), "\n")
  } else {
    warning(
      "  NO Valid calibration points for any chemical in all samples.",
      call. = FALSE, immediate = TRUE
    )
  }
  
  uniq_batch_ids <- unique(as.character(SumExp::col_df(proc_sumexp)[["batch_id"]]))
  for (batch_id in uniq_batch_ids) {
    cat(glue::glue("  Processing batch `{batch_id}`."), "\n")
    
    # Batch-wise calibration
    cal_batch <- quantify_using_calcurve(per_batch_proc_se_lst[[batch_id]], mat_id_to_calib, params)
    if (cal_global$done) {
      # The data of the batch in globally calibrated data
      batch_in_global <- list(
        calcurve = cal_global$calcurve[, colnames(cal_batch$calcurve)],
        concn = cal_global$concn[, colnames(cal_batch$concn)]
      )
      stopifnot(exprs = {
        identical(colnames(cal_batch$concn), colnames(batch_in_global$concn))
        identical(colnames(cal_batch$calcurve), colnames(batch_in_global$calcurve))
      })
    }
    
    if (cal_batch$done) { # At least one chemical has proper calibration range
      has_proper_range <- SumExp::row_df(cal_batch$calcurve)[, "has_proper_range"]
      # Source of calibration curve
      src_calcurve <- dplyr::if_else(has_proper_range, "Batch", NA_character_)
      SumExp::row_df(cal_batch$calcurve)[["src_calcurve"]] <- src_calcurve
      # If any chemical has no proper range in the batch calibration, try to use global calibration
      if (cal_global$done && sum(!has_proper_range) > 0) {
        global_has_proper_range <- SumExp::row_df(batch_in_global$calcurve)[, "has_proper_range"]
        # Only global calibration has proper range, replace with global calibration
        in_only_global <- !has_proper_range & global_has_proper_range
        if (sum(in_only_global) > 0) {
          batch_in_only_global <- lapply(batch_in_global, \(x) x[in_only_global, ])
          cat(glue::glue(
            "    Replacing concentrations with global calibration for {sum(in_only_global)} ",
            "chemicals in batch `{batch_id}`.",
          ), "\n")
          # Replace the values in the batch calibration with the global calibration
          cal_batch$concn[["conc"]][in_only_global, ] <- batch_in_only_global$concn[["conc"]]
          cal_batch$concn[["conc0"]][in_only_global, ] <- batch_in_only_global$concn[["conc0"]]
          # colnames(SumExp::row_df(cal_batch$concn))
          # [1] "alignment_id"     "feature_name"     "mz"               ".rt"              ".std_type"
          # [6] "closest_istd"     "min_c_conc"       "max_c_conc"       "lod"              "lloq"
          # [11] "avg_signal_at_lloq"  "has_proper_range" "calcurve_model"   "to_export"
          SumExp::row_df(cal_batch$concn)[in_only_global, ] <- SumExp::row_df(batch_in_only_global$concn)
          # Replace information of the calibration curve with the one from global calibration
          # colnames(SumExp::row_df(cal_batch$calcurve))
          # [1] "alignment_id"     "feature_name"     "mz"               ".rt"              ".std_type"
          # [6] "closest_istd"     "min_c_conc"       "max_c_conc"       "lod"              "lloq"
          # [11] "avg_signal_at_lloq"  "has_proper_range" "src_calcurve"
          SumExp::row_df(cal_batch$calcurve)[in_only_global, ] <- SumExp::row_df(batch_in_only_global$calcurve)
        }
      }
      lst_proc[[mat_id_to_calib]][[batch_id]] <- cal_batch
      # Progress message
      cat(glue::glue("  Calibration of batch `{batch_id}` is done."), "\n")
    } else {
      warning(glue::glue(
        "  NO Valid calibration points for any chemical in batch `{batch_id}`.",
      ), call. = FALSE, immediate = TRUE)
      if (cal_global$done) {
        lst_proc[[mat_id_to_calib]][[batch_id]] <- list(
          done = TRUE,
          calcurve = batch_in_global$calcurve,
          concn = batch_in_global$concn
        )
        cat(glue::glue("  Globally calibrated data copied to the batch `{batch_id}`."), "\n")
      }
    }
  }
}
# Save the processed data
saveRDS(lst_proc, file = FILE$proc)
cat("The `SumExp` object after processing saved to:", FILE$proc, "\n")

# Mark the processing step as completed
mark_completed_and_save_to_report()
