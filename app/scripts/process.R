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
    c("--keep_all_calcurves"),
    type = "logical", default = FALSE,
    help = paste(
      "Keep the calibration curves of chemicals without a valid calibration range (e.g. too few",
      "levels above the LLOQ), with a range from the LLOQ (or lowest level) to the highest level",
      "[default: %default]"
    )
  ),
  optparse::make_option(
    c("--r2_filter"),
    type = "logical", default = FALSE,
    help = "Calibration curves with an R2 below `--min_r2` are not valid [default: %default]"
  ),
  optparse::make_option(
    c("--min_r2"),
    type = "double", default = 0.99,
    help = "Minimum R2 of a calibration curve, with `--r2_filter` [default: %default]"
  ),
  optparse::make_option(
    c("--linear_only"),
    type = "logical", default = FALSE,
    help = "Fit linear calibration models only, no quadratic ones [default: %default]"
  ),
  optparse::make_option(
    c("--vis_prenorm"),
    type = "logical", default = FALSE,
    help = paste(
      "Pre-normalize all peak areas (also of the IS) by the volumetric internal standard(s): area / vIS",
      "area of the sample * average vIS area of all injections; several vIS one after the other",
      "[default: %default]"
    )
  ),
  optparse::make_option(
    c("--average_cal_reps"),
    type = "logical", default = FALSE,
    help = paste(
      "Average the calibration point replicates within each batch; the global curve of all batches,",
      "fitted on these averages, is used for every batch. Needs `--use_rsd20=FALSE` and",
      "`--llox_method=pt_signal_mean` (set so) [default: %default]"
    )
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
    c("--impute_llox"),
    type = "logical", default = TRUE,
    help = paste(
      "Replace concentrations below the LOD by LLOQ/4 and below the LLOQ by LLOQ/2. With FALSE,",
      "they have no value and are reported as `n.d.` and `n.q.` [default: %default]"
    )
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
  ),
  optparse::make_option(
    c("--rsd_filter"),
    type = "logical", default = FALSE,
    help = "Quality control: remove features with a QC RSD% above a threshold [default: %default]"
  ),
  optparse::make_option(
    c("--rsd_filter_scope"),
    type = "character", default = "both",
    help = paste(
      "Features the QC RSD% filter applies to.",
      "Possible values are: `both`, `targeted`, `nontargeted`. [default: %default]"
    )
  ),
  optparse::make_option(
    c("--rsd_max_targeted"),
    type = "double", default = 30,
    help = "Maximum QC RSD% (0-80) of targeted features [default: %default]"
  ),
  optparse::make_option(
    c("--rsd_max_nontargeted"),
    type = "double", default = 30,
    help = "Maximum QC RSD% (0-80) of non-targeted features [default: %default]"
  )
)
opt_parser <- optparse::OptionParser(
  option_list = option_list,
  usage = "Usage: %prog [options]",
  description = "Process MS-Dial parsed data and save the results."
)
params <- optparse::parse_args(opt_parser)
# Averaged calibration points have no replicates within a batch: no RSD% at the LLOQ level and
# no SD of the zero calibrant (the app disables those options then)
if (isTRUE(params$average_cal_reps)) {
  if (isTRUE(params$use_rsd20) || params$llox_method != "pt_signal_mean") {
    cat("Averaged calibration points: the RSD% 20% threshold is not used and the LOD/LLOQ method is",
        "`pt_signal_mean` (no replicates to compute an SD or RSD%).\n")
  }
  params$use_rsd20 <- FALSE
  params$llox_method <- "pt_signal_mean"
}
cat(
  "\nProcessing parameters:\n",
  "  - vIS pre-normalization: ", isTRUE(params$vis_prenorm), "\n",
  "  - Average calibration replicates per batch (global curve): ", isTRUE(params$average_cal_reps), "\n",
  "  - Optimize cal points: ", params$optimize_cal_points, "\n",
  "  - Minimum number of cal points: ", params$min_cal_points, "\n",
  "  - Keep all calibration curves: ", params$keep_all_calcurves, "\n",
  "  - Linear models only: ", params$linear_only, "\n",
  "  - Minimum R2 of calibration curves: ",
  if (isTRUE(params$r2_filter)) params$min_r2 else "none", "\n",
  "  - Outlier removal:", params$rm_outlier, "\n",
  "  - Log scale for calibration:", params$log_calibration, "\n",
  "  - Weight method:", params$weight, "\n",
  "  - LOD/LLOQ method: ", params$llox_method, "\n",
  "  - Use RSD% 20% for LLOQ: ", params$use_rsd20, "\n",
  "  - Values below the LOD/LLOQ: ",
  if (!isFALSE(params$impute_llox)) "replaced (LLOQ/4, LLOQ/2)" else "n.d. / n.q. (no value)", "\n",
  "  - Blank filtering: ", params$blank_filter, "\n",
  "  - Blank filtering factor: ", params$blank_filter_factor, "\n",
  "  - QC RSD% filtering: ", params$rsd_filter,
  if (isTRUE(params$rsd_filter)) {
    paste0(" (max. ", paste(c(
      if (params$rsd_filter_scope %in% c("both", "targeted")) paste0("targeted ", params$rsd_max_targeted, "%"),
      if (params$rsd_filter_scope %in% c("both", "nontargeted")) paste0("non-targeted ", params$rsd_max_nontargeted, "%")
    ), collapse = ", "), ")")
  }, "\n"
)
stopifnot(
  "`--rsd_filter_scope` must be `both`, `targeted` or `nontargeted`." =
    params$rsd_filter_scope %in% c("both", "targeted", "nontargeted"),
  "The minimum R2 must be between 0 and 1." =
    !isTRUE(params$r2_filter) || (params$min_r2 >= 0 && params$min_r2 <= 1),
  "The maximum QC RSD% must be between 0 and 80." =
    all(c(params$rsd_max_targeted, params$rsd_max_nontargeted) >= 0 &
          c(params$rsd_max_targeted, params$rsd_max_nontargeted) <= 80)
)

#  -----  LIBRARIES AND FILES  -------------------------------------------------
# Load packages and project local libraries
options(box.path = Sys.getenv("R_BOX_PATH", "app/")) # Path to project local libraries
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
# - `multivariate IS-PCA`: Factor, loadings, etc. of `proc$multivariate_is_pca_normalization()`
#       (absent when it could not be computed)
# - `median RSD% per normalization`: Named vector of the median RSD% of replicates per matrix ID
# - `median RSD% basis`: Sample category of those replicates: "QC", "CalCurve" or NA (none)
# - `best normalization`: Matrix ID of the normalization with the lowest median RSD%
#       (`loess_norm` when it cannot be computed)
# - `normalized`: `SumExp` object after normalization (before blank filtering)
# - `after blank filtering`: `SumExp` object after blank filtering
# - `qc rsd filter`: Settings of the QC RSD% filter (`enabled`, `scope`, `max_targeted`,
#       `max_nontargeted`) and whether it was `skipped` (with `skip_reason`)
# - `removed features`: Features removed by the blank filter or the QC RSD% filter, per matrix:
#       `mat_id`, `feature_id`, `feature_name`, `targeted`, `reason`, `qc_rsd`
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

#  -----  PRE-NORMALIZATION BY THE VOLUMETRIC INTERNAL STANDARD(S)  -------------
# `--vis_prenorm`: every peak area (also of the IS) is divided by the vIS area of its sample and
# multiplied by the average vIS area of all injections; with several vIS, one after the other.
# The vIS themselves are not changed (by themselves nor by another vIS). A sample without a vIS
# value (0 or missing) is not corrected by that vIS.
if (isTRUE(params$vis_prenorm)) {
  vis_rows <- which(util$std_type(proc_sumexp) %in% "vIS")
  if (length(vis_rows) == 0) {
    cat("vIS pre-normalization: no volumetric internal standard in the data; skipped.\n")
  } else {
    raw0 <- proc_sumexp[["raw"]]
    out <- raw0
    not_vis <- !seq_len(nrow(raw0)) %in% vis_rows
    for (v in vis_rows) {
      vis <- raw0[v, ]
      ok <- is.finite(vis) & vis > 0
      factor <- ifelse(ok, mean(vis[ok]) / vis, 1)
      out[not_vis, ] <- sweep(out[not_vis, , drop = FALSE], 2, factor, `*`)
      cat("vIS pre-normalization by ", rownames(raw0)[v], " (average area ", signif(mean(vis[ok]), 4), ")",
          if (any(!ok)) paste0("; ", sum(!ok), " sample(s) without a vIS value not corrected"), ".\n", sep = "")
    }
    proc_sumexp[["raw"]] <- labelled::set_label_attribute(out, "Raw (vIS pre-normalized)")
    to_report[["vIS prenormalization"]] <- SumExp::row_df(proc_sumexp)$feature_name[vis_rows]
  }
}

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
if (nrow(internal_std_se) == 0) {
  stop("All ", length(is_failed), " internal standards failed (zero values in more than 10% of the ",
       "injections): the data cannot be normalized. Check the internal standards in the file.",
       call. = FALSE)
}
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

##  NORMALIZE - MULTIVARIATE (PCA) OF INTERNAL STANDARDS  --
# Optional: if it fails (e.g. too few internal standards), the other methods are still used
norm_mat_ids <- c("loess_norm", "closest_norm")
mvis_out <- tryCatch(
  proc$multivariate_is_pca_normalization(se = proc_sumexp, istd_se = internal_std_se, mat_id = "raw"),
  error = function(e) {
    warning("Multivariate (PCA) IS normalization skipped: ", conditionMessage(e),
            call. = FALSE, immediate. = TRUE)
    NULL
  }
)
if (!is.null(mvis_out)) {
  proc_sumexp[["mvis_norm"]] <- mvis_out$normalized |>
    labelled::set_label_attribute("Multivariate (PCA) normalized")
  to_report[["multivariate IS-PCA"]] <- mvis_out[c("factor", "loadings", "explained_variance", "is_used")]
  norm_mat_ids <- c(norm_mat_ids, "mvis_norm")
  cat("Multivariate (PCA) IS normalization is done (PC1 explains ",
      round(100 * mvis_out$explained_variance, 1), "% of the IS variance).\n", sep = "")
}

##  BEST NORMALIZATION METHOD  -----------------------------
# Lowest median RSD% of replicate QC samples (before blank filtering/subtraction). Without QC
# replicates, the replicates of the calibration curve levels are used instead.
rsd_basis <- "QC"
median_rsd <- proc$median_rsd_per_matrix(proc_sumexp, c("raw", norm_mat_ids), cat = "QC")
if (all(is.na(median_rsd[norm_mat_ids]))) {
  rsd_basis <- "CalCurve"
  median_rsd <- proc$median_rsd_per_matrix(proc_sumexp, c("raw", norm_mat_ids), cat = "CalCurve")
}
if (all(is.na(median_rsd[norm_mat_ids]))) rsd_basis <- NA_character_
best_norm <- proc$select_best_normalization(median_rsd, candidates = norm_mat_ids)
to_report[["median RSD% per normalization"]] <- median_rsd
to_report[["median RSD% basis"]] <- rsd_basis
to_report[["best normalization"]] <- best_norm
cat("Median RSD% of ", rsd_basis, " replicates per method: ",
    paste0(names(median_rsd), " = ", round(median_rsd, 2), collapse = ", "), "\n",
    "Best normalization method: ", best_norm, "\n", sep = "")

##  END OF INTERNAL STANDARD PROCESSING  -------------------
to_report[["normalized"]] <- proc_sumexp # Final internal standard normalized data
proc_sumexp <- proc_sumexp[!util$is_internal_std(proc_sumexp), ] # Remove internal standards


#  -----  BLANK FILTERING  ----------------------------------------------------

# Matrix IDs to perform blank subtraction. Raw + normalized matrices
to_blk_subt <- c("raw", norm_mat_ids)
to_report[["matrix ids to blk subt"]] <- to_blk_subt

# Removed features are kept as rows of NA (per matrix, i.e. normalization method). Which features
# were removed and why is collected for the app and the reports:
# `removed features`: tibble of `mat_id`, `feature_id`, `feature_name`, `targeted`, `reason`
# ("Blank filter" / "QC RSD%") and `qc_rsd` (QC RSD% of the matrix, NA if not computed)
is_all_na_row <- function(m) rowSums(!is.na(m)) == 0
removed_features <- list()
targeted <- util$is_targeted_feature(proc_sumexp)
# The volumetric IS is not filtered (blank filter, QC RSD% filter): it is added to every sample,
# also to the blanks, and is not a feature to measure
is_vis <- util$std_type(proc_sumexp) %in% "vIS"
removed_tbl <- function(mat_id, is_removed, reason, qc_rsd = NA_real_) {
  tibble::tibble(
    mat_id = mat_id,
    feature_id = rownames(proc_sumexp),
    feature_name = SumExp::row_df(proc_sumexp)$feature_name,
    targeted = targeted,
    reason = reason,
    qc_rsd = qc_rsd
  )[is_removed, ]
}

if (params$blank_filter) {
  for (mat_id in to_blk_subt) {
    cat("Performing blank filtering for matrix:", mat_id, "\n")
    na_before <- is_all_na_row(proc_sumexp[[mat_id]])
    vis_values <- proc_sumexp[[mat_id]][is_vis, , drop = FALSE]
    proc_sumexp <- proc$filter_by_blank(
      proc_sumexp,
      mat_id = mat_id,
      factor = params$blank_filter_factor
    )
    proc_sumexp[[mat_id]][is_vis, ] <- vis_values   # Kept (see `is_vis`)
    SumExp::row_df(proc_sumexp)$blank_filtered[is_vis] <- TRUE
    removed_features[[length(removed_features) + 1]] <-
      removed_tbl(mat_id, !na_before & is_all_na_row(proc_sumexp[[mat_id]]), "Blank filter")
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
# Batches without blank samples: blank subtraction with the average of all blank samples. Their
# calibration follows the usual rules (batch-wise if possible, otherwise the global curve).
batches_without_blank <- setdiff(
  unique(batch_ids),
  unique(batch_ids[util$ctrl_smpl_cat(proc_sumexp) == "Blank"])
)
to_report[["batches without blank"]] <- batches_without_blank
if (length(batches_without_blank) > 0) {
  cat("Batches without blank samples (the average of all blank samples is subtracted):",
      paste(batches_without_blank, collapse = ", "), "\n")
}
# Which blank average is subtracted from each sample (shown in the exported tables)
SumExp::col_df(proc_sumexp)[["blank_source"]] <- ifelse(
  batch_ids %in% batches_without_blank, "Average of all batches", "Batch"
)
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

# The blank samples themselves (normalized, not blank subtracted), quantified after calibration
# for the exported concentration tables
blank_samples_se <- proc_sumexp[, util$ctrl_smpl_cat(proc_sumexp) == "Blank"]
SumExp::col_df(blank_samples_se)[["blank_source"]] <- rep("Not subtracted", ncol(blank_samples_se))

# Global blank subtraction for failed batches.
proc_sumexp <- proc$add_blank_subtracted_sumexp(
  sumexp = proc_sumexp,
  no_change = util$ctrl_smpl_cat(proc_sumexp) == "CalCurve",
  mat_ids = to_blk_subt,
  out_mat_ids = mat_ids_after_blk_subt
)
# A batch without blanks is not blank-subtracted on its own: take its globally subtracted data
for (batch_id in batches_without_blank) {
  in_batch <- as.character(SumExp::col_df(proc_sumexp)[["batch_id"]]) == batch_id
  per_batch_proc_se_lst[[batch_id]] <- proc_sumexp[, in_batch]
}
cat("Blank subtraction is done.\n")


#  -----  QC RSD% FILTERING  ---------------------------------------------------
# Remove features whose RSD% among the QC replicates exceeds the threshold, per normalization
# method, after normalization and blank subtraction (the matrices used for calibration). RSD% is
# computed on the QC samples of all batches. Skipped with a warning without QC replicates.
rsd_filter <- list(
  enabled = isTRUE(params$rsd_filter),
  scope = params$rsd_filter_scope,
  max_targeted = if (params$rsd_filter_scope %in% c("both", "targeted")) params$rsd_max_targeted else NA_real_,
  max_nontargeted = if (params$rsd_filter_scope %in% c("both", "nontargeted")) params$rsd_max_nontargeted else NA_real_,
  skipped = FALSE,
  skip_reason = NA_character_
)
if (rsd_filter$enabled) {
  if (!proc$has_qc_replicates(proc_sumexp)) {
    rsd_filter$skipped <- TRUE
    rsd_filter$skip_reason <- paste(
      "Not enough QC samples: at least two QC samples of the same class are needed to compute",
      "RSD%. No features were removed by the QC RSD% filter."
    )
    warning("QC RSD% filtering skipped. ", rsd_filter$skip_reason, call. = FALSE, immediate. = TRUE)
  } else {
    # The maximum RSD% of each feature; NA where the filter does not apply
    limit <- ifelse(targeted, rsd_filter$max_targeted, rsd_filter$max_nontargeted)
    for (ii in seq_along(to_blk_subt)) {
      mat_id <- to_blk_subt[ii]                     # e.g. "loess_norm" (reported)
      blk_mat_id <- mat_ids_after_blk_subt[ii]      # e.g. "loess_norm_blk" (filtered)
      rsd <- proc$qc_rsd_per_feature(proc_sumexp, blk_mat_id)
      already_removed <- is_all_na_row(proc_sumexp[[blk_mat_id]])
      to_remove <- !already_removed & !is_vis & !is.na(limit) & !is.na(rsd) & rsd > limit
      # In the overall (global) data and in each batch
      proc_sumexp[[blk_mat_id]] <- util$extract_with_na(proc_sumexp[[blk_mat_id]], i = !to_remove)
      for (batch_id in names(per_batch_proc_se_lst)) {
        se <- per_batch_proc_se_lst[[batch_id]]
        stopifnot(identical(rownames(se), rownames(proc_sumexp)))
        se[[blk_mat_id]] <- util$extract_with_na(se[[blk_mat_id]], i = !to_remove)
        per_batch_proc_se_lst[[batch_id]] <- se
      }
      removed_features[[length(removed_features) + 1]] <-
        removed_tbl(mat_id, to_remove, "QC RSD%", qc_rsd = rsd)
      cat("QC RSD% filtering for ", mat_id, ": ", sum(to_remove & targeted), " targeted and ",
          sum(to_remove & !targeted), " non-targeted features removed (the volumetric IS is not filtered).\n", sep = "")
    }
  }
}
to_report[["qc rsd filter"]] <- rsd_filter
to_report[["removed features"]] <- dplyr::bind_rows(removed_features)
to_report[["normalized - blank"]] <- per_batch_proc_se_lst


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
  quit(save = "no", status = 0)   # Done: a normal exit, so that the app reports success
}

if (params$log_calibration) cat("Log scale for calibration curve fitting is applied.\n")
if (has_multiple_batches) {
  cat("Processing calibration per batch.")
} else {
  cat("Processing calibration for a single batch.")
}

##  CALIBRATION FUNCTION  ----------------------------------

#' Concentrations from the signal in `se[[mat_id]]` (in the scale of the fit) with the fitted
#' models (`calcurve_model`) and limits in `row_df(se)`, and the LOD/LLOQ rules
#'
#' @returns `se` with the matrices `conc0` (before the rules), `conc` and `conc_flag` ("n.d." /
#'   "n.q." where the rules replaced a value below the LOD / LLOQ, otherwise `NA`). Without
#'   `--impute_llox`, those values are removed from `conc` at the end (see `censor_llox()`)
quantify_conc <- function(se, mat_id, in_log) {
  se[["conc0"]] <- proc$compute_concentration(se, mat_id, log_scale = in_log) |>
    labelled::set_label_attribute("Concentration before filtering")
  # [[conc0]] is the unfiltered reference; [[conc]] gets the LOD/LLOQ replacements below
  se[["conc"]] <- se[["conc0"]]
  se <- util$save_src_mat_id_for_conc(se, mat_id)
  # Replace concentrations below LOD/LLOQ; rows are kept intact to preserve data structure
  se <- se |>
    proc$replace_below_lod_lloq(conc_mat_id = "conc") |>
    proc$replace_conc_whose_signal_below_lloq(conc_mat_id = "conc") |>
    proc$replace_conc_whose_signal_above_lloq(conc_mat_id = "conc") |>
    proc$replace_conc_whose_signal_below_lod(conc_mat_id = "conc")
  # Curves kept although not valid can lack an LLOQ (no replacements then): no negative values
  neg <- se[["conc"]] < 0 & SumExp::row_df(se)$kept_calcurve
  neg[is.na(neg)] <- FALSE
  se[["conc"]][neg] <- 0
  se[["conc_flag"]] <- proc$llox_flags(se[["conc0"]], se[["conc"]], SumExp::row_df(se)$lloq)
  se
}

#' Without `--impute_llox`: the values replaced below the LOD / LLOQ get no value (`NA` in `conc`;
#' `conc_flag` tells "n.d." / "n.q."). Done after the decisions that use the replaced values
#' (e.g. which chemicals are exported), so that these are the same with and without replacement.
#'
#' @param se `NULL` or a `SumExp` object with `conc` and `conc_flag`
censor_llox <- function(se) {
  if (is.null(se)) return(se)
  SumExp::metadata(se)$llox_imputed <- !isFALSE(params$impute_llox)
  if (!isFALSE(params$impute_llox) || !all(c("conc", "conc_flag") %in% names(se))) return(se)
  se[["conc"]][!is.na(se[["conc_flag"]])] <- NA
  se
}

#' Concentrations of the blank samples of a batch, from their normalized signal WITHOUT blank
#' subtraction, with the final calibration of the batch (its own curves, or the global ones)
#'
#' @param blank_se The blank samples of the batch (before blank subtraction)
#' @param concn_se The quantified samples of the batch (`row_df` with the models and limits)
#' @param mat_id_to_calib The ID of the blank-subtracted matrix used for calibration
#' @returns A `SumExp` object of the blanks with `conc0` and `conc` (same chemicals as `concn_se`)
quantify_blanks <- function(blank_se, concn_se, mat_id_to_calib) {
  in_log <- isTRUE(params$log_calibration)
  mat_id <- util$mat_id_in_calibration(mat_id_to_calib)
  src_mat_id <- to_blk_subt[match(mat_id_to_calib, mat_ids_after_blk_subt)]   # Not subtracted
  se <- blank_se[rownames(concn_se), ]
  SumExp::row_df(se) <- SumExp::row_df(concn_se)
  sig <- se[[src_mat_id]]
  se[[mat_id]] <- if (in_log) log1p(sig) else sig
  se <- quantify_conc(se, mat_id, in_log)
  if (in_log) se[[mat_id]] <- expm1(se[[mat_id]])   # Restore to original scale
  # Chemicals not exported have no concentrations, as for the samples
  se[["conc"]] <- util$extract_with_na(se[["conc"]], i = SumExp::row_df(se)$to_export, j = TRUE) |>
    labelled::set_label_attribute(labelled::get_label_attribute(concn_se[["conc"]]))
  se[["conc_flag"]] <- util$extract_with_na(se[["conc_flag"]], i = SumExp::row_df(se)$to_export)
  se
}

# Separated as a function to be used for both per-batch and global-mode calibration
quantify_using_calcurve <- function(qt_se, mat_id_to_calib, params) {
  # Whether to fit in log scale (--log_calibration):
  # - signal (normalized, blank-subtracted): `log(x + 1)` and `exp(x) - 1`
  # - concentration: log(x) / exp(x)
  in_log <- params$log_calibration
  
  # Restrict to targeted features
  qt_se <- qt_se[util$is_targeted_feature(qt_se), ]
  # Exclude chemicals removed by blank filtering (their rows are all NA). Blank filtering is done
  # before splitting into batches, so the same chemicals are excluded globally and per batch.
  qt_se <- qt_se[rowSums(!is.na(qt_se[[mat_id_to_calib]])) > 0, ]
  
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
  
  # Exclude chemicals without a valid calibration range; return early if none qualify.
  # `range_issue` tells why a range is not valid. With `--keep_all_calcurves`, those chemicals keep
  # a curve (`kept_calcurve`) over a forced range, and count as having a proper range below.
  has_proper_range <- proc$has_proper_calibration_range(qt_se)
  rd <- SumExp::row_df(qt_se)
  rd$range_issue[has_proper_range] <- NA_character_
  rd$range_issue[!has_proper_range & is.na(rd$range_issue)] <- "No valid calibration range"
  SumExp::row_df(qt_se) <- rd
  kept_calcurve <- !has_proper_range & isTRUE(params$keep_all_calcurves)
  if (any(kept_calcurve)) {
    qt_se <- proc$force_calibration_range(qt_se, kept_calcurve, params$min_cal_points)
  }
  has_proper_range <- has_proper_range | kept_calcurve
  SumExp::row_df(qt_se) <- cbind(SumExp::row_df(qt_se), has_proper_range, kept_calcurve)
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
    linear_only = isTRUE(params$linear_only), util = util, proc = proc
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
        penalty_quadratic = 0.01,
        linear_only = linear_only
      )
    })
    names(out) <- rownames(cc_se)
    return(out)
  })
  SumExp::row_df(concn_se) <- SumExp::row_df(concn_se) |>
    dplyr::mutate(calcurve_model = calcurve_models)
  if (in_log) calcurve_se[[mat_id]] <- expm1(calcurve_se[[mat_id]]) # expm1(x) = exp(x) - 1

  # Minimum R2 (--r2_filter, --min_r2): a curve fitted with a lower R2 is not valid, i.e. no
  # concentrations and not exported; with --keep_all_calcurves it is kept (and marked) instead
  r2_failed <- rep(FALSE, length(calcurve_models))
  if (isTRUE(params$r2_filter)) {
    r2 <- vapply(calcurve_models, \(m) {
      if (is.list(m)) unname(m$R2s[[m$best_model_name]]) else NA_real_
    }, numeric(1))
    low_r2 <- !is.na(r2) & r2 < params$min_r2 & has_proper_range & !kept_calcurve
    if (any(low_r2)) {
      issue <- sprintf("RÂ² %.3f below the minimum %.3f", r2[low_r2], params$min_r2)
      keep <- isTRUE(params$keep_all_calcurves)
      set_r2_issue <- function(se) {
        rd <- SumExp::row_df(se)
        rd$range_issue[low_r2] <- issue
        if (keep) rd$kept_calcurve[low_r2] <- TRUE else rd$has_proper_range[low_r2] <- FALSE
        SumExp::row_df(se) <- rd
        se
      }
      concn_se <- set_r2_issue(concn_se)
      calcurve_se <- set_r2_issue(calcurve_se)
      if (!keep) r2_failed <- low_r2
      cat(sum(low_r2), "calibration curve(s) with RÂ² below", params$min_r2,
          if (keep) "(kept)." else "(not valid).", "\n")
    }
  }

  quantify <- function(se) quantify_conc(se, mat_id, in_log)
  concn_se <- quantify(concn_se)
  labelled::label_attribute(concn_se) <- signal_mat_label
  if (in_log) concn_se[[mat_id]] <- expm1(concn_se[[mat_id]]) # Restore to original scale
  # The calibration samples too (back-calculated, all points), for the export with all samples
  cc_conc <- local({
    se <- calcurve_se
    SumExp::row_df(se) <- dplyr::mutate(SumExp::row_df(se), calcurve_model = calcurve_models)
    se[[mat_id]] <- if (in_log) log1p(se[[mat_id_to_calib]]) else se[[mat_id_to_calib]]
    quantify(se)
  })
  calcurve_se[["conc0"]] <- cc_conc[["conc0"]]

  # Flag chemicals where no study sample exceeded the LLOQ; these are excluded from exports.
  # Curves kept by `--keep_all_calcurves` without an LLOQ are exported when they have a model.
  non_qc_conc <- util$exclude_ctrl_smpl_cat(concn_se, "QC")[["conc"]]
  any_above_lloq <- non_qc_conc > SumExp::row_df(concn_se)[, "lloq"]
  has_ex <- rowSums(any_above_lloq) > 0
  has_ex[is.na(has_ex)] <- FALSE
  kept_no_lloq <- SumExp::row_df(concn_se)$kept_calcurve & is.na(SumExp::row_df(concn_se)$lloq)
  has_ex[kept_no_lloq] <- !is.na(calcurve_models[kept_no_lloq])
  has_ex[r2_failed] <- FALSE   # Below the minimum R2: no concentrations
  SumExp::row_df(concn_se)[["to_export"]] <- has_ex
  # Set concentrations of non-exported chemicals to NA
  unit <- SumExp::metadata(concn_se)$concentration_unit
  concn_se[["conc"]] <- util$extract_with_na(concn_se[["conc"]], i = has_ex, j = TRUE) |>
    labelled::set_label_attribute(paste0("Concentration [", unit, "]"))
  calcurve_se[["conc"]] <- util$extract_with_na(cc_conc[["conc"]], i = has_ex, j = TRUE) |>
    labelled::set_label_attribute(paste0("Concentration [", unit, "]"))
  # Values replaced below the LOD / LLOQ ("n.d." / "n.q."), of the exported chemicals
  concn_se[["conc_flag"]] <- util$extract_with_na(concn_se[["conc_flag"]], i = has_ex)
  calcurve_se[["conc_flag"]] <- util$extract_with_na(cc_conc[["conc_flag"]], i = has_ex)

  return(list(
    done = TRUE,
    calcurve = calcurve_se, # Calibration-curve samples with range metadata; no fitted models
    concn = concn_se # Quantified samples with fitted models, LOD/LLOQ thresholds, and export flags
  ))
}

#' [quantify_using_calcurve()] that reports an error as "not done" instead of stopping, e.g. when
#' a batch has only one sample per calibration level (no SD / RSD% can be computed). Batches that
#' are not done fall back to the global calibration below.
try_quantify_using_calcurve <- function(qt_se, mat_id_to_calib, params, what) {
  tryCatch(
    quantify_using_calcurve(qt_se, mat_id_to_calib, params),
    error = function(e) {
      warning(glue::glue("  Calibration failed for {what}: {conditionMessage(e)}"),
              call. = FALSE, immediate. = TRUE)
      list(done = FALSE, reason = conditionMessage(e))
    }
  )
}

#' Why calibration curve samples are not enough for calibration, or `NULL` if they are enough
#'
#' Replicates are needed for the SD of the zero point (`pt_signal_mean_plus_sd`) and for the
#' RSD% of the calibration points (`--use_rsd20`).
calcurve_replicate_issue <- function(qt_se, params) {
  c_conc <- util$spiked_conc_pts(qt_se)
  n_per_pt <- table(c_conc[!is.na(c_conc)])
  if (length(n_per_pt) == 0) return("no calibration curve samples")
  if (params$llox_method == "pt_signal_mean_plus_sd" && !isTRUE(n_per_pt[["0"]] >= 2)) {
    return("< 2 zero-conc cal samples (needed for SD of the LOD/LLOQ)")
  }
  if (params$use_rsd20 && any(n_per_pt[names(n_per_pt) != "0"] < 2)) {
    return("< 2 replicates per cal point (needed for RSD% <= 20%)")
  }
  NULL
}

# Calibration used per batch, collected for the reports. One row per matrix ID and batch
calibration_per_batch <- list()

#' A compact copy of the global calibration (all batches) for the reports
#'
#' Kept for every chemical, also those calibrated batch-wise, so that the reports can show the
#' global curve next to the batch curves. Plain matrices and the model coefficients only: the
#' fitted models themselves (closures holding their data) would make the saved files large.
#'
#' @returns A list of `limits` (a tibble per chemical, with the model name, R2 and coefficients
#'   `coef` of the fit of signal on concentration), the calibration points (`cal_signal`,
#'   `cal_used`, `cal_conc`) and the quantified samples (`conc0`, `signal`); matrices have
#'   chemicals in rows and sample IDs in columns.
compact_global_calibration <- function(cal, mat_id_to_calib) {
  mat_id <- util$mat_id_in_calibration(mat_id_to_calib)
  rd <- SumExp::row_df(cal$concn)
  model_info <- lapply(rd$calcurve_model, function(m) {
    if (!is.list(m) || is.null(m$best_model_name)) {
      return(list(model = NA_character_, r2 = NA_real_, coef = NULL))
    }
    list(model = m$best_model_name, r2 = unname(m$R2s[[m$best_model_name]]),
         coef = unname(stats::coef(m$inv_model)))
  })
  limits <- tibble::tibble(
    feature_id = rownames(cal$concn),
    feature_name = rd$feature_name,
    has_proper_range = rd$has_proper_range,
    range_issue = rd$range_issue,          # Why the range is not valid (NA: valid)
    kept_calcurve = rd$kept_calcurve,      # Kept despite `range_issue` (--keep_all_calcurves)
    no_quant_samples = rd$no_quant_samples,   # No sample or QC at or above the LLOQ: full range
    lod = rd$lod, lloq = rd$lloq, max_c_conc = rd$max_c_conc,
    model = vapply(model_info, `[[`, "", "model"),
    r2 = vapply(model_info, `[[`, 0, "r2"),
    coef = lapply(model_info, `[[`, "coef")   # Intercept, conc (, conc^2); log scale if fitted so
  )
  list(
    log_scale = isTRUE(params$log_calibration),
    limits = limits,
    cal_signal = cal$calcurve[[mat_id_to_calib]],
    cal_used = !is.na(cal$calcurve[[mat_id]]),   # Points within the calibration range
    cal_conc = stats::setNames(SumExp::col_df(cal$calcurve)$c_conc, colnames(cal$calcurve)),
    conc0 = cal$concn[["conc0"]],
    signal = cal$concn[[mat_id]]
  )
}
to_report[["global calibration"]] <- list()

##  CALIBRATION  -------------------------------------------

# Collect the output. "mat_id_to_calib" -> "batch_id" -> list of "calcurve" and "concn"
# - "mat_id_to_calib" indicates the ID of the matrix to use for calibration
# - "calcurve" is a SumExp object of the calibration curve
# - "concn" is a SumExp object having calibrated concentrations
lst_proc <- list()

# `--average_cal_reps`: the calibration replicates of each batch are averaged (one point per level
# and batch) and the global curve fitted on these averages is used for every batch
AVERAGE_CAL <- isTRUE(params$average_cal_reps)
AVERAGE_CAL_NOTE <- "calibration replicates averaged per batch (option): the global curve of all batches is used"
cal_input_se <- if (AVERAGE_CAL) proc$average_calcurve_replicates(proc_sumexp) else proc_sumexp
if (AVERAGE_CAL) {
  cat("Calibration replicates averaged per batch:", sum(util$ctrl_smpl_cat(proc_sumexp) == "CalCurve"),
      "calibration samples ->", sum(util$ctrl_smpl_cat(cal_input_se) == "CalCurve"), "averaged points.",
      "The global curve is used for every batch.\n")
}

# Per normalization method, then per batch
for (mat_id_to_calib in mat_ids_after_blk_subt) {
  lst_proc[[mat_id_to_calib]] <- list() # Initialize to collect the output per normalization method
  # Label for the normalized data.
  norm_lab <- labelled::get_label_attribute(proc_sumexp[[mat_id_to_calib]])
  
  # Global calibration for failed batches.
  cal_global <- try_quantify_using_calcurve(cal_input_se, mat_id_to_calib, params, "all samples")
  
  if (cal_global$done) {
    # Source of the calibration curve per chemical. In batch-mode, it can be "Global" or "Batch"
    # or NA (no proper calibration range). Also kept with the concentrations for the exports.
    src_global <- dplyr::if_else(
      SumExp::row_df(cal_global$calcurve)[, "has_proper_range"], "Global", NA_character_
    )
    SumExp::row_df(cal_global$calcurve)[["src_calcurve"]] <- src_global
    SumExp::row_df(cal_global$concn)[["src_calcurve"]] <- src_global
    to_report[["global calibration"]][[mat_id_to_calib]] <-
      compact_global_calibration(cal_global, mat_id_to_calib)
  } else {
    warning(
      "  NO Valid calibration points for any chemical in all samples.",
      call. = FALSE, immediate = TRUE
    )
  }
  
  uniq_batch_ids <- unique(as.character(SumExp::col_df(proc_sumexp)[["batch_id"]]))
  for (batch_id in uniq_batch_ids) {
    # cat(glue::glue("  Processing batch `{batch_id}`."), "\n")
    
    # Batch-wise calibration, if the batch has enough calibration curve replicates
    batch_se <- per_batch_proc_se_lst[[batch_id]]
    issue <- if (AVERAGE_CAL) AVERAGE_CAL_NOTE else calcurve_replicate_issue(batch_se, params)
    cal_batch <- if (is.null(issue)) {
      try_quantify_using_calcurve(batch_se, mat_id_to_calib, params, glue::glue("batch `{batch_id}`"))
    } else {
      list(done = FALSE, reason = issue)
    }
    if (cal_global$done) {
      # The data of the batch in globally calibrated data
      # (Samples of the batch, also when the batch calibration failed and has no output)
      batch_samples <- colnames(per_batch_proc_se_lst[[batch_id]])
      batch_in_global <- lapply(cal_global[c("calcurve", "concn")], \(se) {
        se[, colnames(se) %in% batch_samples]
      })
      # Averaged calibration points have their own names: those of the batch, by batch ID
      if (AVERAGE_CAL) {
        cc <- cal_global$calcurve
        batch_in_global$calcurve <- cc[, as.character(SumExp::col_df(cc)$batch_id) == batch_id]
      }
      # A batch without its own calibration curve samples shows all the global ones, which made
      # its calibration curves (also keeps plots and tables from getting empty data)
      if (ncol(batch_in_global$calcurve) == 0) batch_in_global$calcurve <- cal_global$calcurve
      if (cal_batch$done) {
        stopifnot(exprs = {
          identical(colnames(cal_batch$concn), colnames(batch_in_global$concn))
          identical(colnames(cal_batch$calcurve), colnames(batch_in_global$calcurve))
        })
      }
    }
    
    if (cal_batch$done) { # At least one chemical has proper calibration range
      has_proper_range <- SumExp::row_df(cal_batch$calcurve)[, "has_proper_range"]
      # Source of calibration curve
      src_calcurve <- dplyr::if_else(has_proper_range, "Batch", NA_character_)
      SumExp::row_df(cal_batch$calcurve)[["src_calcurve"]] <- src_calcurve
      SumExp::row_df(cal_batch$concn)[["src_calcurve"]] <- src_calcurve
      # If any chemical has no proper range in the batch calibration (or only a curve kept by
      # `--keep_all_calcurves`), try to use the global calibration
      kept_b <- SumExp::row_df(cal_batch$calcurve)[, "kept_calcurve"]
      if (cal_global$done && sum(!has_proper_range | kept_b) > 0) {
        rd_global <- SumExp::row_df(batch_in_global$calcurve)
        # Only global calibration has proper range, replace with global calibration
        in_only_global <- (!has_proper_range | kept_b) & rd_global$has_proper_range & !rd_global$kept_calcurve
        if (sum(in_only_global) > 0) {
          batch_in_only_global <- lapply(batch_in_global, \(x) x[in_only_global, ])
          # Why the batch's own curve is not used (kept with the global curve info below)
          batch_issue <- SumExp::row_df(cal_batch$concn)$range_issue[in_only_global]

          # Replace the values in the batch calibration with the global calibration
          for (m in c("conc", "conc0", "conc_flag")) {
            cal_batch$concn[[m]][in_only_global, ] <- batch_in_only_global$concn[[m]]
            cal_batch$calcurve[[m]][in_only_global, ] <- batch_in_only_global$calcurve[[m]]
          }

          #Replacing information with global curve info
          SumExp::row_df(cal_batch$concn)[in_only_global, ] <- SumExp::row_df(batch_in_only_global$concn)
          SumExp::row_df(cal_batch$calcurve)[in_only_global, ] <- SumExp::row_df(batch_in_only_global$calcurve)
          for (part in c("concn", "calcurve")) {
            SumExp::row_df(cal_batch[[part]])$range_issue[in_only_global] <- batch_issue
          }
        }
      }
      lst_proc[[mat_id_to_calib]][[batch_id]] <- cal_batch
      src <- SumExp::row_df(cal_batch$concn)[["src_calcurve"]]
      batch_cal <- if (any(src %in% "Global")) "Batch + Global" else "Batch"
      note <- if (batch_cal == "Batch") {
        NA_character_
      } else {
        glue::glue("{sum(src %in% 'Global')} chemical(s) without a proper range in the batch use the global calibration")
      }
      # Progress message
      cat(glue::glue("  Calibration of batch `{batch_id}` is done."), "\n")
    } else {
      if (cal_global$done) {
        lst_proc[[mat_id_to_calib]][[batch_id]] <- list(
          done = TRUE,
          calcurve = batch_in_global$calcurve,
          concn = batch_in_global$concn
        )
        # cat(glue::glue("  Globally calibrated data copied to the batch `{batch_id}`."), "\n")
      } else {
        warning(glue::glue(
          "  NO Valid calibration points for any chemical in batch `{batch_id}`.",
        ), call. = FALSE, immediate = TRUE)
      }
      batch_cal <- if (cal_global$done) "Global" else NA_character_
      note <- if (is.null(cal_batch$reason)) {
        "no chemical with a proper calibration range in the batch"
      } else {
        cal_batch$reason
      }
    }

    # The blank samples of the batch, quantified with its final calibration (without blank
    # subtraction)
    in_batch <- as.character(SumExp::col_df(blank_samples_se)[["batch_id"]]) == batch_id
    if (!is.null(lst_proc[[mat_id_to_calib]][[batch_id]]) && any(in_batch)) {
      lst_proc[[mat_id_to_calib]][[batch_id]]$blank <- quantify_blanks(
        blank_samples_se[, in_batch], lst_proc[[mat_id_to_calib]][[batch_id]]$concn, mat_id_to_calib
      )
    }

    # Tag the samples of the batch with the calibration used: "Batch", "Batch + Global"
    # (some chemicals from the global calibration) or "Global" (batch calibration not possible)
    if (!is.null(lst_proc[[mat_id_to_calib]][[batch_id]])) {
      parts <- intersect(c("calcurve", "concn", "blank"), names(lst_proc[[mat_id_to_calib]][[batch_id]]))
      for (part in parts) {
        n_smpl <- ncol(lst_proc[[mat_id_to_calib]][[batch_id]][[part]])   # Can be 0 for "calcurve"
        SumExp::col_df(lst_proc[[mat_id_to_calib]][[batch_id]][[part]])[["calibration"]] <-
          rep(batch_cal, n_smpl)
        SumExp::metadata(lst_proc[[mat_id_to_calib]][[batch_id]][[part]])$calibration_note <- note
      }
    }
    calibration_per_batch[[length(calibration_per_batch) + 1L]] <- tibble::tibble(
      mat_id = mat_id_to_calib, batch_id = batch_id, calibration = batch_cal, note = note
    )
  }
}
to_report[["calibration per batch"]] <- dplyr::bind_rows(calibration_per_batch)
cat("\nCalibration used per batch:\n")
print(as.data.frame(to_report[["calibration per batch"]]), right = FALSE)
# Values below the LOD / LLOQ: replaced, or without a value ("n.d." / "n.q.") without --impute_llox
for (mat_id_to_calib in names(lst_proc)) {
  for (batch_id in names(lst_proc[[mat_id_to_calib]])) {
    for (part in intersect(c("calcurve", "concn", "blank"), names(lst_proc[[mat_id_to_calib]][[batch_id]]))) {
      lst_proc[[mat_id_to_calib]][[batch_id]][[part]] <- censor_llox(lst_proc[[mat_id_to_calib]][[batch_id]][[part]])
    }
  }
}
if (isFALSE(params$impute_llox)) {
  cat("Concentrations below the LOD / LLOQ have no value (reported as n.d. / n.q.).\n")
}
# Save the processed data
saveRDS(lst_proc, file = FILE$proc)
cat("The `SumExp` object after processing saved to:", FILE$proc, "\n")

# Mark the processing step as completed
mark_completed_and_save_to_report()
