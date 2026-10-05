# ------------------------------------------------------------------------------------------- #
# Functions for processing of the data
# ------------------------------------------------------------------------------------------- #
box::use(util = ./msdial_utils)

#  -----  QC STEPS  ------------------------------------------------------------

##  QC STEPS  ----------------------------------------------

#' Add intermediate data at each step of quality control
#'
#' @param name A character string for the name of the intermediate data
#' @param data The data to be stored with the `name`
#' @param file A file name to store the intermediate data
#' @md
#' @export
append_to_qc_steps <- function(..., file) {
  if (!file.exists(file)) stop("Run `initialize_qc_steps` first.")
  to_report <- readRDS(file)
  dots <- rlang::list2(...)
  nms <- names(dots)
  stopifnot(
    "Name of the intermediate data must be provided" = !any(sapply(nms, is.null)),
    "Duplicated names" = anyDuplicated(c(nms, names(to_report))) == 0
  )
  for (nm in nms) {
    to_report[[nm]] <- dots[[nm]]
  }
  saveRDS(to_report, file)
}
#' @rdname append_to_qc_steps
#' @export
initialize_qc_steps <- function(file) {
  saveRDS(list(), file)
}

#' Stop without showing error messages
#'
#' @export
stop_quietly <- function() {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}


#  -----  NORMALIZATION WITH I.S.  ---------------------------------------------
# Normalize the data using internal standards

##  VOLUMETRIC NORMALIZATION  ------------------------------

# #' Normalize the data by the volumetric internal standard
# #'
# #' @param se A [`SumExp::SumExp`] object
# #' @param is_vIS A logical vector indicating the volumetric internal standard. Only one
# #'   volumetric internal standard is allowed. The length of the vector should be the same as
# #'   the number of rows of `se`.
# #' @param mat_id The name of a matrix in `se` to be normalized
# #' @returns A [`SumExp::SumExp`] object with the volumetric normalization. The normalized
# #'   matrix is added to the `se` with the name `vol_norm`.
# #' @md
# #' @export
# normalize_volumetric <- function(se, is_vIS, mat_id) { # nolint: object_name_linter.
#   stopifnot("Only one volumetric internal standard is allowed." = sum(is_vIS) == 1)
#   stopifnot(nrow(se) == length(is_vIS))
#   vIS_se <- se[is_vIS, ] # nolint: object_name_linter.
#   se <- se[!is_vIS, ]

#   v <- as.vector(vIS_se[[mat_id]])
#   mat <- t(replicate(nrow(se), v))  # Column-wise normalization
#   # <<---- Volumetric normalization ---->> #
#   se[["vol_norm"]] <- se[[mat_id]] / mat * mean(v, na.rm = TRUE)
#   labelled::label_attribute(se[["vol_norm"]]) <- "Volumetric normalized"
#   se
# }

##  DATA CLEANING  -----------------------------------------

#' Count zeros per feature
#'
#' @param mat A numeric matrix
#' @returns A numeric vector of the number of zeros per feature in rows
#' @export
count_zeros_per_feature <- function(mat) {
  rowSums(mat == 0) |>
    labelled::set_label_attribute("Number of zeros")
}

#' Impute zeros with the mean of the same sample type
#'
#' @param se A [`SumExp::SumExp`] object
#' @param mat_id The name of a matrix in `se` to be imputed
#' @returns A [`SumExp::SumExp`] object with the imputed matrix
#' @md
#' @export
impute_zeros_with_mean_of_same_type <- function(se, mat_id) {
  by_type <- SumExp::split_columns(se, SumExp::col_df(se)[["sample_type"]])
  avg_by_type <- purrr::map(by_type, function(ea) {
    # Get the mean of the same sample type
    mat <- ea[[mat_id]]
    mat[mat == 0] <- NA
    rowMeans(mat, na.rm = TRUE)
  })
  # Impute zeros with the mean of the same type
  for (typ in names(by_type)) {
    i_typ <- SumExp::col_df(se)[["sample_type"]] == typ
    for (ii in seq_len(nrow(se))) {
      # Replace the zeros with the mean of the same type
      old_values <- se[[mat_id]][ii, i_typ]
      se[[mat_id]][ii, i_typ][old_values == 0] <- avg_by_type[[typ]][ii]
    }
  }
  se
}

#' Identify outliers
#'
#' @param times A numeric value for the threshold. mean +/- times * sd
#' @param x A numeric vector
#'
#' @returns A logical vector indicating whether each element is an outlier
#' @export
identify_outliers <- function(x, times = 3) {
  m <- mean(x)
  sd <- stats::sd(x)
  return((x > m + times * sd) | (x < m - times * sd))
}

#' Count outlying internal standard features per sample
#'
#' @param se A [`SumExp::SumExp`] object
#' @param mat_id The name of a matrix in `se`
#' @param times A numeric value for the threshold. mean +/- times * sd
#' @returns A numeric vector of the number of outlying internal standard features per sample
#' @md
#' @export
count_outliers_per_sample <- function(se, mat_id, times = 3) {
  x <- se[[mat_id]]
  x <- log1p(x)              # Log-transform the data
  outlying <- apply(x, 1, identify_outliers, times = times)
  return(rowSums(outlying))     # Transposed by `apply` above
}

##  NORMALIZATION METHODS  ---------------------------------

#' Get the values of the closest internal standard features
#'
#' @param se A [`SumExp::SumExp`] object
#' @param istd_se A [`SumExp::SumExp`] object of internal standard features
#' @param mat_id The name of a matrix in `se`
#' @param verbose A logical value indicating whether to show messages
#' @returns A list of two elements:
#' * `mat`: a numeric matrix of the values of the closest internal standard features
#' * `feat_nm`: the names of the closest internal standards
#' * `rt`: the retention times of the closest internal standards
#' @export
get_data_of_closest_istd <- function(se, istd_se, mat_id, verbose = TRUE) {
  out <- get_data_of_closest_istd_in_class(se, istd_se, mat_id)
  # When any compound target classes are specified
  istd_std_type <- util$std_type(istd_se)
  is_sub_istd <- grepl("^IS\\\\", istd_std_type)
  if (any(is_sub_istd)) {
    # Identify the compound target classes
    classes <- unique(istd_std_type[is_sub_istd])
    if (verbose) cat("Compound target classes: ", paste(classes, collapse = ", "), "\n")
    # Get the closest internal standard feature for each subgroup
    for (g in classes) {
      g_istd_se <- istd_se[istd_std_type == g, ]
      matched_quant_id <- sub("^IS\\\\", "Quant\\\\", g)
      # To find which target features are in which compound target class
      is_sub_quant <- util$std_type(se) == matched_quant_id
      g_se <- se[is_sub_quant, , drop = FALSE]
      if (nrow(g_se) == 0) {
        warning(paste0("No quantification features for the compound class: ", g))
        next
      }
      g_out <- get_data_of_closest_istd_in_class(g_se, g_istd_se, mat_id)
      # Replace the values of the closest internal standard features in the original matrix
      # with the values of the closest internal standard features in the compound class
      out$mat[is_sub_quant, ] <- g_out$mat
      out$feat_nm[is_sub_quant] <- g_out$feat_nm
      out$rt[is_sub_quant] <- g_out$rt
    }
  }
  return(out)
}
#' @rdname get_data_of_closest_istd
#' [get_data_of_closest_istd_in_class()] is a helper function to get the values of the
#' closest internal standard features within each subgroup.
get_data_of_closest_istd_in_class <- function(se, istd_se, mat_id) {
  rt_x <- util$retention_time(se)
  rt_istd <- util$retention_time(istd_se)
  # Find the closest internal standard feature
  i_closest <- sapply(rt_x, \(.x) which.min(abs(rt_istd - .x)))
  # dim(mat) == dim(se) Not dim(istd_se)
  mat <- istd_se[[mat_id]][i_closest, , drop = FALSE]
  rownames(mat) <- rownames(se)
  list(
    mat = mat,
    feat_nm = SumExp::row_df(istd_se)$feature_name[i_closest],
    rt = util$retention_time(istd_se)[i_closest]
  )
}

#' Get the LOESS fit model
#'
#' @param istd_se A [`SumExp::SumExp`] object of internal standard features
#' @param excl_cat A character vector of the categories to exclude
#' @param overall_rt_range A numeric vector of the overall retention time range, to which the
#'   model is expanded
#' @param span A numeric value for the span of the LOESS fit
#' @param mat_id The name of a matrix in `istd_se`
#'
#' @returns A list of the LOESS fit models
#' @md
#' @export
get_loess_fit <- function(istd_se, excl_cat, overall_rt_range, span, mat_id) {
  # Log-transform the data
  istd_log <- log(istd_se[[mat_id]])
  rt_istd <- util$retention_time(istd_se)

  # Normalize the data using the internal standards
  # Mean of each internal standard feature for overall measurement samples
  # , excluding the calibration curve and blank samples
  mat <- log(util$exclude_ctrl_smpl_cat(istd_se, excl_cat)[[mat_id]])
  stopifnot("Failed interncal control(s)! value = 0" = all(is.finite(mat)))
  istd_m <- rowMeans(mat)
  # Subtract the mean from the data
  fr_m_log <- istd_log - istd_m

  # Expand to smallest RT
  istd_smallest_rt <- fr_m_log[which.min(rt_istd), ]
  # Expand to largest RT
  istd_largest_rt <- fr_m_log[which.max(rt_istd), ]
  # Add the expanded range
  fr_m_log <- rbind(
    istd_smallest_rt,
    istd_largest_rt,
    fr_m_log
  )
  # LOESS fit of the internal standard features along RT
  loess_fit <- purrr::map(seq_len(ncol(fr_m_log)), function(ii) {
    # Fit a loess curve
    y <- fr_m_log[, ii]
    y[is.finite(y) == FALSE] <- NA
    stopifnot(identical(
      names(y), c("istd_smallest_rt", "istd_largest_rt", rownames(istd_se))
    ))
    suppressWarnings(stats::loess(y ~ c(overall_rt_range, rt_istd), span = span))
  })
  names(loess_fit) <- colnames(fr_m_log)
  loess_fit
}

#' Multivariate (PCA) internal standard normalization
#'
#' Per-sample normalization factor derived from the first principal component of UVN-scaled
#' (unit-variance scaled, NOT mean-centered) internal standard peak areas. The factor is rescaled
#' to a mean of 1 so the normalized values stay in the same order of magnitude as the raw data.
#' Follows Surowiec et al. (2017), Metabolomics 13:114.
#'
#' @param se A [`SumExp::SumExp`] object with the features to be normalized
#' @param istd_se A [`SumExp::SumExp`] object of internal standard features only, with the same
#'   samples in the same order as `se`
#' @param mat_id The name of a matrix in `se` and `istd_se`
#'
#' @returns A list:
#' * `normalized`: a numeric matrix with the same dimensions as `se[[mat_id]]`
#' * `factor`: the per-sample normalization factor (PC1 score / its mean), named by sample ID
#' * `loadings`: the loadings of the internal standards on PC1
#' * `explained_variance`: the fraction of the (uncentered) internal standard variance on PC1
#' * `is_used`: IDs of the internal standards used (those with a positive, finite variance)
#' @md
#' @export
multivariate_is_pca_normalization <- function(se, istd_se, mat_id) {
  stopifnot(identical(colnames(se), colnames(istd_se)))
  is_x <- t(istd_se[[mat_id]]) # Samples in rows, internal standards in columns
  if (any(!is.finite(is_x))) stop("Internal standard data contain NA, NaN or infinite values.")
  if (any(is_x <= 0)) stop("Internal standard peak areas must be > 0.")

  # UVN scaling WITHOUT mean centering
  is_sd <- apply(is_x, 2, stats::sd)
  keep <- is.finite(is_sd) & is_sd > 0
  if (sum(keep) < 2) stop("At least two internal standards with non-zero variance are required.")
  is_scaled <- sweep(is_x[, keep, drop = FALSE], 2, is_sd[keep], FUN = "/")

  # PCA without centering via SVD (`prcomp()` centers by default)
  svd_fit <- svd(is_scaled, nu = 1, nv = 1)
  t1 <- svd_fit$u[, 1] * svd_fit$d[1]
  p1 <- svd_fit$v[, 1]
  if (sum(p1) < 0) { # The sign of a PC is arbitrary; orient it along the peak areas
    t1 <- -t1
    p1 <- -p1
  }
  if (any(t1 <= 0)) stop("Non-positive PC1 scores; the normalization factor is undefined.")
  norm_factor <- stats::setNames(t1 / mean(t1), colnames(se))

  list(
    normalized = sweep(se[[mat_id]], 2, norm_factor, FUN = "/"),
    factor = norm_factor,
    loadings = stats::setNames(p1, colnames(is_x)[keep]),
    explained_variance = svd_fit$d[1]^2 / sum(svd_fit$d^2),
    is_used = colnames(is_x)[keep]
  )
}

##  COMPARISON OF NORMALIZATION METHODS  -------------------

#' Median RSD% of replicate samples per matrix
#'
#' RSD% is computed per feature within each class (replicate group, e.g. a QC class or a
#' calibration level) of the sample category `cat`, then the median over all features and classes
#' is taken. Targeted features are used; if there are none (non-target mode), all features except
#' internal standards are used.
#'
#' @param se A [`SumExp::SumExp`] object, including the matrices in `mat_ids`
#' @param mat_ids A character vector of matrix IDs to compare
#' @param cat The sample category of the replicates, e.g. `"QC"` or `"CalCurve"`
#' @returns A named numeric vector of the median RSD% per matrix ID. `NA` when it cannot be
#'   computed, e.g. without replicates.
#' @md
#' @export
median_rsd_per_matrix <- function(se, mat_ids, cat = "QC") {
  qc_se <- util$extract_ctrl_smpl_cat(se, cat)
  is_tgt <- util$is_targeted_feature(qc_se)
  qc_se <- if (any(is_tgt)) qc_se[is_tgt, ] else qc_se[!util$is_internal_std(qc_se), ]
  qc_class <- SumExp::col_df(qc_se)$Class
  rsd_perc <- function(x) 100 * stats::sd(x) / mean(x)
  vapply(stats::setNames(nm = mat_ids), function(mat_id) {
    rsd <- unlist(lapply(unique(qc_class), function(cl) {
      apply(qc_se[[mat_id]][, qc_class == cl, drop = FALSE], 1, rsd_perc)
    }))
    rsd <- rsd[is.finite(rsd)]
    if (length(rsd) == 0) NA_real_ else stats::median(rsd)
  }, numeric(1))
}

#' Whether there are QC replicates to compute RSD% (a QC class with at least two samples)
#'
#' @param se A [`SumExp::SumExp`] object
#' @returns `TRUE` or `FALSE`
#' @export
has_qc_replicates <- function(se) {
  qc_class <- SumExp::col_df(util$extract_ctrl_smpl_cat(se, "QC"))$Class
  any(table(qc_class) >= 2)
}

#' QC RSD% of each feature
#'
#' RSD% is computed within each QC class with at least two samples; with several QC classes the
#' median over the classes is taken.
#'
#' @param se A [`SumExp::SumExp`] object
#' @param mat_id The name of a matrix in `se`
#' @returns A numeric vector of RSD% per feature (rows of `se`); `NA` when it cannot be computed,
#'   e.g. all values are zero or missing
#' @export
qc_rsd_per_feature <- function(se, mat_id) {
  qc_se <- util$extract_ctrl_smpl_cat(se, "QC")
  qc_class <- SumExp::col_df(qc_se)$Class
  classes <- names(which(table(qc_class) >= 2))
  rsd_perc <- function(x) {
    m <- mean(x, na.rm = TRUE)
    if (!is.finite(m) || m == 0) NA_real_ else 100 * stats::sd(x, na.rm = TRUE) / m
  }
  if (length(classes) == 0) return(rep(NA_real_, nrow(se)))
  per_class <- vapply(classes, function(cl) {
    apply(qc_se[[mat_id]][, qc_class == cl, drop = FALSE], 1, rsd_perc)
  }, numeric(nrow(se)))
  per_class <- matrix(per_class, nrow = nrow(se))
  out <- apply(per_class, 1, \(v) if (all(is.na(v))) NA_real_ else stats::median(v, na.rm = TRUE))
  stats::setNames(out, rownames(se))
}

#' Select the normalization method with the lowest median RSD%
#'
#' @param median_rsd A named numeric vector from [median_rsd_per_matrix()]
#' @param candidates Matrix IDs eligible to be selected
#' @param fallback Matrix ID returned when no candidate has a valid RSD%
#' @returns A single matrix ID
#' @md
#' @export
select_best_normalization <- function(median_rsd, candidates, fallback = "loess_norm") {
  v <- median_rsd[intersect(candidates, names(median_rsd))]
  v <- v[!is.na(v)]
  if (length(v) == 0) fallback else names(v)[which.min(v)]
}

#  -----  BLANK FILTERING  -----------------------------------------------------

#' Filter features based on blank samples
#'
#' Criteria: Keep feature if max(sample) > [`factor`] * avg(blank) OR avg(sample) > [`factor`] * avg(blank)
#'
#' @param se A [`SumExp::SumExp`] object
#' @param mat_id The name of a matrix in `se`
#' @param factor A numeric value for the filtering factor.
#' @returns A [`SumExp::SumExp`] object with the features filtered
#' @export
filter_by_blank <- function(sumexp, mat_id, factor = 5) {
  g <- ifelse(util$ctrl_smpl_cat(sumexp) == "Blank", "Blank", "Other")
  se_lst <- SumExp::split_columns(sumexp, g)

  if (ncol(se_lst[["Blank"]]) == 0) {
    warning("No blank samples found. Skipping blank filtering.")
    return(sumexp)
  }
  blank_mat <- se_lst[["Blank"]][[mat_id]]
  sample_mat <- se_lst[["Other"]][[mat_id]]

  blank_avg <- rowMeans(blank_mat, na.rm = TRUE)
  sample_max <- apply(sample_mat, 1, max, na.rm = TRUE)
  sample_avg <- rowMeans(sample_mat, na.rm = TRUE)

  to_keep <- (sample_max > factor * blank_avg) | (sample_avg > factor * blank_avg)
  to_keep[is.na(to_keep)] <- FALSE

  cat("Blank filtering: ", sum(!to_keep), " features removed (", sum(to_keep), " remain).\n", sep = "")
  sumexp[[mat_id]] <- util$extract_with_na(sumexp[[mat_id]], i = to_keep)

  SumExp::row_df(sumexp)[["blank_filtered"]] <- to_keep

  sumexp
}

##  BLANK SUBTRACTION  -------------------------------------

#' Subtract the average values of the blank samples from the samples
#'
#' @param sumexp A [`SumExp::SumExp`] object of the samples
#' @param no_change A condition to select the samples that should not be changed.
#' @param mat_ids The names of matrices in `sumexp`, from which the blank values are subtracted
#' @param out_mat_ids The names of the output matrices in the returned object.
#'   These should be the same size as `mat_ids` in the same order.
#'
#' Each batch is subtracted by the average of its own blank samples. A batch without blank samples
#' is subtracted by the average of all blank samples (of all batches).
#'
#' @returns A [`SumExp::SumExp`] object with the blank values subtracted.
#'   The columns of the blanks are removed from the `sumexp`.
#' @md
#' @export
add_blank_subtracted_sumexp <- function(sumexp,
                                        no_change,
                                        mat_ids,
                                        out_mat_ids) {
  stopifnot(length(mat_ids) == length(out_mat_ids))
  g <- ifelse(util$ctrl_smpl_cat(sumexp) == "Blank", "Blank", "Other")
  se_lst <- SumExp::split_columns(sumexp, g)
  blank_se <- se_lst[["Blank"]]     # NULL when there are no blank samples (e.g. a batch without)
  has_blank <- !is.null(blank_se) && ncol(blank_se) > 0
  sumexp <- se_lst[["Other"]]
  no_change <- no_change[g == "Other"]    # Blank has been split already
  for (ii in seq(mat_ids)) {      # Paired `mat_ids` and `out_mat_ids`
    mat_id <- mat_ids[ii]
    blank_bat_ids <- if (has_blank) SumExp::col_df(blank_se)[["batch_id"]] else character(0)
    se_bat_ids <- SumExp::col_df(sumexp)[["batch_id"]]
    x_mat <- sumexp[[mat_id]]
    x_lab <- labelled::get_label_attribute(x_mat)
    mat_subt <- x_mat   # To store the blank-subtracted data
    for (i_batch in unique(blank_bat_ids)) {
      # Mean per feature of the blank samples in the batch
      blank_mean <- rowMeans(blank_se[[mat_id]][, blank_bat_ids == i_batch, drop = FALSE])
      # <<---- Subtract by blank average ---->> #
      is_batch_i <- se_bat_ids == i_batch
      to_be_chg <- is_batch_i & !no_change
      mat_subt[, to_be_chg] <- x_mat[, to_be_chg] - blank_mean
    }
    # Batches without blank samples: subtract the average of all blank samples
    no_blank_batches <- setdiff(unique(se_bat_ids), unique(blank_bat_ids))
    if (length(no_blank_batches) > 0 && has_blank) {
      all_blank_mean <- rowMeans(blank_se[[mat_id]])
      to_be_chg <- se_bat_ids %in% no_blank_batches & !no_change
      mat_subt[, to_be_chg] <- x_mat[, to_be_chg] - all_blank_mean
    }
    mat_subt[mat_subt < 0] <- 0
    sumexp[[out_mat_ids[ii]]] <- mat_subt |>
      labelled::set_label_attribute(paste(x_lab, "(blank adjusted)"))
  }
  sumexp
}


#  -----  CALIBRATION  ---------------------------------------------------------

##  CALIBRATION WORKING RANGE - UTILS  ---------------------

#' Get the values satisfying the condition
#'
#' @param cond A logical vector to select the values.
#' @param values A numeric or character vector of values.
#'   When it is a character vector, the values are converted to numeric.
#'
#' @returns The `values` that satisfies the condition `cond`.
#'   If no value satisfies the condition, return `NA_real_`.
#' @md
satisfying_values <- function(cond, values = names(cond)) {
  stopifnot(length(cond) == length(values))
  values <- as.numeric(values)
  if (all(!cond | is.na(cond))) {
    return(NA_real_)
  }
  values[which(cond)]
}

#' Split in columns and sort by the spiked concentration
#'
#' Split a matrix column-wise at the spiked concentration points and sort by the values
#'
#' @param cc_se A [`SumExp::SumExp`] object of the calibration curve samples
#' @param mat_id A matrix ID in the `cc_se`
#'
#' @returns A list of matrices of `mat_id` split column-wise. The names of the list are the
#'   spiked concentration points.
#'   Each element of the list is a matrix with the same number of rows as `mat_id` and the number
#'   of columns equal to the number of samples with the same spiked concentration.
#'   The list is sorted by the concentration values.
#' @md
split_in_columns_and_sort_by_spiked_conc <- function(cc_se, mat_id) {
  # Split the columns by the values
  conc <- util$spiked_conc_pts(cc_se)
  stopifnot("Spiked concentration values are not numeric" = is.numeric(conc))
  sorted <- stats::setNames(nm = sort(unique(conc)))

  lapply(sorted, \(.x) cc_se[[mat_id]][, conc == .x, drop = FALSE])
}

#' Apply a function to the values in `mat_id` of spiked concentration points
#'
#' @inheritParams split_in_columns_and_sort_by_spiked_conc
#' @inheritParams base::apply
#' @param FUN A function to apply to the values in `mat_id` of spiked concentration points.
#'   Ideally, the function should return a single value.
#' @returns A matrix (or vector when only one point) of the results of applying `FUN` to the
#'   values in `mat_id` per spiked concentration point
#' @md
apply_per_spiked_conc <- function(cc_se, mat_id, MARGIN, FUN, ..., simplify = TRUE) { # nolint
  cc_lst <- split_in_columns_and_sort_by_spiked_conc(cc_se, mat_id)

  # Checking that calibration standards are increasing with higher conc and if
  # not replacing lower signals with 0

  # Creating df with ncol = number of concentration points and nrow = number of compounds
  cal_curve_df <- data.frame(matrix(ncol = length(cc_lst), nrow = nrow(cc_se)))
  rownames(cal_curve_df) <- rownames(cc_se)
  colnames(cal_curve_df) <- names(cc_lst)

  # Looping through all concentrations and averaging all injections of same conc
  # for all compounds simultaneously
  for (i in seq_len(length(cc_lst))) {
    cal_curve_df[, i] <- rowMeans(cc_lst[[i]])
  }

  # Checking starting index of increase for all compounds
  for (comp in seq_len(nrow(cal_curve_df))) {
    start_ind <- find_increasing_start(cal_curve_df[comp, ])
    # NA: missing signals, e.g. chemicals removed by blank filtering (rows set to NA). Left as
    # they are; they end up without a proper calibration range
    if (is.na(start_ind)) next

    if (start_ind > 1) { # Change to 2 here if 0 always kept

      for (lst_ind in seq_len((start_ind - 1))) { # Remove first if 0 always kept seq_len((start_ind-1))[-1]
        cc_lst[[lst_ind]][comp, ] <- rep(0, ncol(cc_lst[[lst_ind]]))
      }
    }
  }

  sapply(cc_lst, apply, MARGIN, FUN, ..., simplify = TRUE)
}

#' Get the values of the given concentration points
#'
#' @param mat A matrix of values, in which the rows are chemicals and the columns are
#'   calibration points. The column names should be the concentration values at each point.
#' @param given_conc A vector or one value of the concentration points. If it is a vector, the
#'   length should be the same as the number of rows in `mat`.
#' @returns A vector of the values of the given concentration points
values_of_given_conc_in_mat <- function(mat, given_conc) {
  stopifnot(nrow(mat) == length(given_conc) || length(given_conc) == 1)
  stopifnot(any(!is.na(given_conc)))   # At least one value is not NA
  # Concentration values
  conc <- as.numeric(colnames(mat))
  stopifnot(all(given_conc[!is.na(given_conc)] %in% conc), anyDuplicated(conc) == 0)
  # Extract the value of the concentration point
  if (length(given_conc) == 1) {
    mat[, conc == given_conc]
  } else {
    sapply(seq_len(nrow(mat)), \(ii) {
      if (is.na(given_conc[ii])) {
        return(NA_real_)
      }
      mat[ii, conc == given_conc[ii]]
    })
  }
}

##  LOD/LLOQ  ----------------------------------------------

#' Compute the LOD/LLOQ
#'
#' @name compute_llox
#'
#' @param m_neg A numeric vector of the mean signals of the negative control samples
#' @param sd_neg A numeric vector of the standard deviation of the signals of the negative
#'   control samples
#' @param times Multiplication factor to the standard deviation of the signal to get the LOD or
#'   LLOQ. Common values are 3 and 10 for LOD and LLOQ, respectively.
#'
#' @returns A numeric vector of the LOD/LLOQ signal values for each chemical
NULL

#' Compute the LOD/LLOQ signal using mean + SD * times
#'
#' @inheritParams compute_llox
#' @inherit compute_llox returns
#' @export
compute_llox_signal_using_mean_plus_sd_times <- function(m_neg, sd_neg, times) {
  stopifnot(exprs = {
    length(m_neg) == length(sd_neg)
    length(times) == 1
  })
  if (any(is.na(sd_neg))) {
    stop("NA values in sd_neg. Cannot compute LOD/LLOQ using mean + SD * times.")
  }
  # Compute LOD/LLOQ using mean + SD * times
  llox_signal <- m_neg + times * sd_neg
  # Set the chemical IDs as names
  names(llox_signal) <- names(m_neg)
  llox_signal
}

#' Compute the LOD/LLOQ signal using mean * times
#'
#' @inheritParams compute_llox
#' @param sd_neg Not used. It is included to keep the same function signature as other
#'   `compute_llox*` functions.
#' @inherit compute_llox returns
#' @export
compute_llox_signal_using_mean_times <- function(m_neg, sd_neg, times) {
  stopifnot(exprs = {
    length(m_neg) == length(sd_neg)
    length(times) == 1
  })
  # Compute LOD/LLOQ using mean * times
  llox_signal <- m_neg * times
  # Set the chemical IDs as names
  names(llox_signal) <- names(m_neg)
  llox_signal
}

#' Get the minimum concentration points that satisfy the condition
#'
#' @param mat_cond A logical matrix, where each row is a chemical and each column is a concentration
#'   point. The spiked concentration points should be sorted in ascending order.
#'
#' @returns A numeric vector of the same length as the number of rows in `mat_cond`.
#' @md
#' @export
identify_min_pt_satisfying <- function(mat_cond) {
  apply(mat_cond, 1, \(.x) min(satisfying_values(.x)))
}

#' Compute the LLO(Q/D) using the slope and standard deviation
#'
#' `s * factor / slope`
#' , where `s` the standard deviation of the lower-limit concentration point,
#' `slope` is the slope of the calibration curve at the point, and `factor` is a multiplication
#' factor.
#'
#' @param v A numeric vector of the peak areas of the calibration samples
#' @param min_conc The lower limit concentration of calibration curve
#' @param calcurve_model The calibration curve model
#' @param conc A numeric vector of the concentrations of the calibration samples. It should be
#'   the same length as the `v`.
#' @param times Multiplication factor to the mean of the peak area of the `min_conc`
#'   concentration. Common values are 3 and 10 for LOD and LLOQ, respectively.
#' @returns A numeric value of the LLO(Q/D)
#' @md
#' @export
compute_llox_using_slope_and_sd <- function(v, conc, min_conc, calcurve_model, times) {
  stopifnot(length(conc) == length(v))       # Identical concentrations
  # Standard deviation of the lowest concentration point
  v <- v[conc == min_conc]
  s <- stats::sd(v, na.rm = TRUE)
  # Slope of calibration curve
  e <- environment(calcurve_model$best_model)
  slope <- if (grepl("linear", calcurve_model$best_model_name)) {
    get("b1", envir = e)
  } else {
    a <- get("a", envir = e)
    b <- get("b", envir = e)
    2 * a * min_conc + b
  }
  # Calculate the LLOx
  (s * times) / slope
}

##  WITH DERIVED MIN/MAX CONCENTRATION  --------------------

#' Identify the minimum concentration point for calibration curve
#'
#' @param mat_m A matrix of the mean signals per spiked concentration point. The columns are
#'   the spiked concentration points, which should be sorted in ascending order.
#' @param lod_signal A numeric vector of the LOD signals
#' @returns A numeric vector of the minimum concentrations
#' @md
min_conc_for_curve_using_lox_signal <- function(mat_m, lod_signal) {
  stopifnot(nrow(mat_m) == length(lod_signal))
  # The minimum concentration having higher signal than the LOD
  out <- apply(mat_m > lod_signal, 1, \(.x) min(satisfying_values(.x)))
  nm <- names(out)
  out <- as.numeric(out)         # Lost names by this
  names(out) <- nm
  out <- labelled::set_label_attribute(out, "Minimum Concentration")
  out
}

#' Identify the maximum concentration point of the calibration samples
#'
#' @param mat_m A matrix of the mean signals per spiked concentration point. The columns are
#'   the spiked concentration points, which should be sorted in ascending order.
#' @param mat_q A matrix of the peak areas of the samples of interest
#'
#' @returns A numeric vector of the upper limit concentration points for the calibration curve.
#'   The names of the returned vector are the row names.
#'   Some returned values mean:
#'   * `0`  : All values of `mat_q` are zero for the row
#' @md
max_conc_for_curve <- function(mat_m, mat_q) {
  stopifnot(exprs = {
    nrow(mat_m) == nrow(mat_q)
    identical(rownames(mat_m), rownames(mat_q))
  })
  # Find maximum peak area of the samples of non-calibration samples
  max_in_q <- apply(mat_q, 1, max, na.rm = TRUE)
  # Find the concentration point that is just above the maximum peak area
  out <- identify_min_pt_satisfying(mat_m > max_in_q)
  out[max_in_q == 0] <- 0
  # Set to maximum concentration when all values are less than the maximum peak area
  max_pt <- max(as.numeric(colnames(mat_m)))
  out[is.na(out)] <- max_pt
  out <- labelled::set_label_attribute(out, "Maximum Concentration")
  stopifnot(identical(names(out), rownames(mat_q)))
  out
}

#' Make sure to have enough calibration curve samples
#'
#' @param max_conc A vector of the maximum concentration
#' @param min_conc A vector of the minimum concentration
#' @param conc_pts A numeric vector of spiked concentrations
#' @param min_n Required minimum number of concentrations for the calibration curve. If the
#'   number of concentrations is less than this, `NA` is returned.
#' @param enough_n Enough number of concentrations for the calibration curve
#'
#' @returns A numeric vector of the maximum concentrations that provide enough number of spiked
#'   concentrations. The names are the same as the input vector `max_conc`. If the number of
#'   concentrations points is less than `min_n`, the returned value is set to `NA_real_`
#' @md
make_sure_to_have_enough_calcurve_pts <- function(max_conc, min_conc, conc_pts, min_n, enough_n) {
  stopifnot(length(max_conc) == length(min_conc))
  uniq_conc <- sort(unique(conc_pts))
  n_uniq_conc <- length(uniq_conc)
  # Indices of the minimum/maximum concentration points
  i_min <- match(min_conc, uniq_conc)
  i_max <- match(max_conc, c(0, uniq_conc)) - 1 # If `max_conc` == 0, take the largest range
  # Counting from minimum
  i_enough_from_min <- i_min + enough_n - 1
  # Find the indices of the maximums that provide enough number of spiked concentration points
  i_max <- ifelse(i_enough_from_min <= i_max, i_max, i_enough_from_min)
  # Limit when `i_enough_from_min` > number of concentration points
  i_max <- ifelse(i_max > n_uniq_conc, n_uniq_conc, i_max)
  # If the number of valid concentration points is less than `min_n`, return NA
  i_max <- ifelse(i_max >= i_min + min_n - 1, i_max, NA_real_)
  out <- uniq_conc[i_max]
  names(out) <- names(max_conc)
  out <- labelled::copy_labels(max_conc, out)
  stopifnot(identical(names(out), names(max_conc)))
  out
}

#' Find the calibration curve limits and the LOD/LLOQ
#'
#' @param sumexp A [`SumExp::SumExp`] object including the calibration curve samples
#' @param mat_id The name of a matrix in `sumexp`
#' @param compute_llox_signal_fun A function to compute the LOD and LLOQ signals.
#'   It is expected to take three arguments: `m_neg`, `sd_neg`, and `times`.
#'   The function should return a numeric vector of the LOD or LLOQ signals for each chemical.
#' @param use_rsd20 A logical value indicating whether to use the RSD <= 20% condition for the
#'  LLOQ. If `FALSE`, only the signal > LLOQ signal condition is used.
#' @param min_n Required minimum number of concentrations for the calibration curve. 
#' @param enough_n Enough number of concentrations for the calibration curve
#'
#' @returns A [`SumExp::SumExp`] object with the calibration curve limits and the LOD/LLOQ. The
#'   added columns to the [`SumExp::row_df()`] of `sumexp` are: `min_c_conc`, `max_c_conc`, `lod`,
#'   `lloq`, `avg_signal_at_lloq`, `lod_signal`, `lloq_signal`, which are the lower and upper limits
#'   of the calibration curve, the LOD and LLOQ, the average signal at the LLOQ point, and the LOD
#'   and LLOQ in signal perspective, respectively.
#'   Some values in the columns are set to have the following meanings:
#'   * `min_c_conc` = NA : No valid concentration even at the top of the calibration curve
#'   * `max_c_conc` = NA : Not enough calibration curve samples to make a calibration curve
#'   * `max_c_conc` = 0  : All values are zero for the row
#'
#' @md
#' @seealso [max_conc_for_curve()]
#' @export
find_calib_lim_pts_and_llox_from_llox_signal <- function(sumexp,
                                                         mat_id,
                                                         compute_llox_signal_fun,
                                                         use_rsd20,
                                                         min_n,
                                                         enough_n) {
  neg_ctrl_or_not <- local({
    c_pt <- util$spiked_conc_pts(sumexp)
    is_neg_ctrl <- !is.na(c_pt) & c_pt == 0
    g <- ifelse(is_neg_ctrl, "Yes", "No")
    SumExp::split_columns(sumexp, g)
  })
  neg_ctrl_se <- neg_ctrl_or_not[["Yes"]]
  not_neg_ctrl_se <- neg_ctrl_or_not[["No"]]
  # Compute the mean and standard deviation of the negative control samples
  m_neg <- rowMeans(neg_ctrl_se[[mat_id]], na.rm = TRUE)
  sd_neg <- apply(neg_ctrl_se[[mat_id]], 1, stats::sd, na.rm = TRUE)
  stopifnot(exprs = {
    identical(names(m_neg), names(sd_neg))
    identical(names(m_neg), rownames(sumexp))
  })

  # Split the samples into calibration curve and other samples
  se_lst <- util$split_into_calcurve_and_other(not_neg_ctrl_se, out_names = c("cc", "quant"))
  # Get mean of signal per spiked concentration point
  # - Columns: spiked concentration points (sorted)
  # - Rows: chemicals
  mat_m <- apply_per_spiked_conc(se_lst$cc, mat_id, 1, mean, na.rm = TRUE)
  stopifnot(is.matrix(mat_m))

  # Limit of detection
  lod_signal <- compute_llox_signal_fun(m_neg, sd_neg, times = 3)
  stopifnot(identical(rownames(sumexp), names(lod_signal)))
  lod <- apply(mat_m > lod_signal, 1, \(.x) min(satisfying_values(.x)))

  # Lower limit of quantification
  lloq_signal <- compute_llox_signal_fun(m_neg, sd_neg, times = 10)
  stopifnot(identical(rownames(sumexp), names(lloq_signal)))
  mat_cond <- mat_m > lloq_signal

  # Throwing error telling user that no cal points > lloq
  if (!any(mat_cond)) {
    stop("No cal points above lloq_signal")
  }

  if (use_rsd20) {
    if (any(table(util$spiked_conc_pts(se_lst$cc)) < 2)) {
      stop("Some spiked concentration points have less than two samples. Cannot compute LLOQ")
    }
    # Get standard deviation of signal per spiked concentration point
    mat_sd <- apply_per_spiked_conc(se_lst$cc, mat_id, 1, stats::sd, na.rm = TRUE)
    stopifnot(is.matrix(mat_sd))

    mat_rsd <- mat_sd / mat_m
    mat_rsd[mat_m == 0] <- Inf  # Avoid division by zero
    mat_cond <- mat_cond & (mat_rsd <= 0.2)

    # Throwing error telling user that no cal points survived cut-offs
    if (!any(mat_cond)) {
      stop("No cal point RSD < 20% and above lloq_signal")
    }
  }
  lloq <- apply(mat_cond, 1, \(.x) min(satisfying_values(.x)))
  # Average signal of the LLOQ point
  avg_signal_at_lloq <- values_of_given_conc_in_mat(mat_m, lloq)
  # Calibration curve concentration lower limit.
  min_c_conc <- lloq
  min_c_conc <- labelled::set_label_attribute(min_c_conc, "Minimum Concentration")
  # Calibration curve concentration upper limit
  mat_q <- util$exclude_ctrl_smpl_cat(se_lst$quant, excl_cat = "QC")[[mat_id]]
  conc_pts <- as.numeric(colnames(mat_m))
  max_c_conc <- max_conc_for_curve(mat_m, mat_q) |>
    make_sure_to_have_enough_calcurve_pts(min_c_conc, conc_pts, min_n, enough_n)
  stopifnot(identical(names(min_c_conc), names(max_c_conc)))
  # data.frame for SumExp::row_df, instead of tibble
  limits_df <- data.frame(min_c_conc, max_c_conc, lod, lloq, avg_signal_at_lloq,
                          lod_signal, lloq_signal)
  stopifnot(identical(rownames(limits_df), rownames(sumexp)))
  SumExp::row_df(sumexp) <- cbind(SumExp::row_df(sumexp), limits_df)
  sumexp
}

#' Check if the calibration curve has a proper calibration range
#'
#' @param sumexp A [`SumExp::SumExp`] object of the calibration curve samples.
#'   The object should have the calibration curve limits added by
#'   [add_calibration_curve_limits()]
#'   The minimum and maximum points are required.
#'
#' @returns A logical vector
#' @md
#' @export
has_proper_calibration_range <- function(sumexp) {
  min_c_conc <- SumExp::row_df(sumexp)[["min_c_conc"]]
  max_c_conc <- SumExp::row_df(sumexp)[["max_c_conc"]]
  # No valid concentration point even at the top of the calibration curve
  no_valid_conc <- is.na(min_c_conc)
  # All concentrations are zero
  all_zero <- max_c_conc == 0
  # No valid maximum concentration
  no_max_conc <- max_c_conc == -9
  # Not enough calibration curve samples to make a calibration curve
  not_enough <- is.na(max_c_conc)
  !(no_valid_conc | all_zero | no_max_conc | not_enough)
}

#' Replace values outside the concentration range with NA
#'
#' @param x [`SumExp::SumExp`] object or a matrix of the calibration curve samples
#' @param ... Additional arguments. Not used.
#' @param mat_id The name of a matrix in `x` when `x` is a [`SumExp::SumExp`] object
#' @param conc A vector of concentrations of the calibration curve samples (`matrix`)
#' @param min_conc,max_conc A vector of minimum and maximum valid concentrations (`matrix`)
#'
#' @returns An object of the same class as `x` with the values outside the concentration range
#'   replaced with NA
#' @md
#' @export
replace_outside_concentration_range_with_na <- function(x, ...) {
  UseMethod("replace_outside_concentration_range_with_na")
}
#' @rdname replace_outside_concentration_range_with_na
#' @method replace_outside_concentration_range_with_na matrix
#' @export
replace_outside_concentration_range_with_na.matrix <-
  function(x, conc, min_conc, max_conc) {
    stopifnot(exprs = {
      length(conc) == ncol(x)
      length(min_conc) == nrow(x)
      length(max_conc) == nrow(x)
    })
    # Concentration values in a matrix, rows are features and columns are concentrations
    conc_mat <- matrix(rep(conc, each = nrow(x)), nrow = nrow(x))
    to_na <- conc_mat < min_conc | conc_mat > max_conc
    x[to_na] <- NA_real_
    x
  }
#' @rdname replace_outside_concentration_range_with_na
#' @method replace_outside_concentration_range_with_na SumExp
#' @export
replace_outside_concentration_range_with_na.SumExp <- function(x, mat_id) {
  x[[mat_id]] <- replace_outside_concentration_range_with_na.matrix(
    x = x[[mat_id]],
    conc = util$spiked_conc_pts(x),
    min_conc = SumExp::row_df(x)[["min_c_conc"]],
    max_conc = SumExp::row_df(x)[["max_c_conc"]]
  )
  x
}

##  CALIBRATION CURVE FITTING  -----------------------------

#' Fit and test calibration curve models
#'
#' @param conc A vector of concentrations
#' @param signal A vector of signal values
#' @param weight_method The method to use for weighting the models. The options are:
#'   - `largestR2`: The model with the largest R2 value is used as the weight
#'   - `1`: Constant weight of 1
#'   - `1/x`: Inverse of the concentration
#'   - `1/x2`: Inverse of the concentration squared
#' @param penalty_quadratic The penalty for the quadratic models
#'
#' @returns A list with the best model, the name of it, the R2 values of all models, and the
#'   number of unique concentrations. The best model is chosen as the model with the highest R2
#'   after applying the penalty to quadratic models.
#' @examples
#' conc <- rep(c(0.1, 0.2, 0.5, 1, 2), 3)
#' signal <- conc * 5 + rnorm(length(conc))
#' fit_and_test_calcurve_model(conc, signal)
#' @name fit_calcurve_model
#' @md
NULL
#' @rdname fit_calcurve_model
#' @export
fit_and_test_calcurve_model <- function(conc,
                                        signal,
                                        weight_method = "largestR2",
                                        penalty_quadratic = 0) {
  # If no signal return NA
  if (all(is.na(signal) | signal == 0)) {
    return(NA)
  }

  # Weight alternatives
  weights_alt <- rlang::list2(
    "1" = rep(1, length(conc)),
    "1_div_x" = 1 / conc,
    "1_div_x2" = 1 / (conc^2),
  )

  # Limit the weight alternatives when one has been chosen
  if (weight_method != "largestR2") {
    weights_alt <- weights_alt[weight_method]
  }
  # Linear and quadratic models
  lmodels <- lapply(weights_alt, \(w) linear_calcurve_model(conc, signal, weights = w))
  names(lmodels) <- paste("linear", names(lmodels), sep = "-")

  qmodels <- lapply(weights_alt, \(w) quadratic_calcurve_model(conc, signal, weights = w))
  names(qmodels) <- paste("quadratic", names(qmodels), sep = "-")

  models <- c(lmodels, qmodels)
  r2s <- sapply(models, \(x) suppressWarnings(summary(x$inv_mod)$r.squared))
  # Panelty to quadratic models
  r2s_adj <- r2s
  is_quadratic <- grepl("quadratic", names(models))
  r2s_adj[is_quadratic] <- r2s[is_quadratic] - penalty_quadratic
  i_best <- which.max(r2s_adj)
  rlang::list2(
    "best_model" = models[[i_best]]$model,        # Best model by R2
    "best_model_name" = names(models)[i_best],
    "R2s" = r2s,
    "R2s_adj" = r2s_adj,
    "n_conc" = length(unique(conc[!is.na(signal)])),      # Number of unique concentrations
    "inv_model" = models[[i_best]]$inv_mod
  )
}
#' @param weights weights for linear model fit
#' @rdname fit_calcurve_model
quadratic_calcurve_model <- function(conc, signal, weights) {
  lmfit <- stats::lm(signal ~ conc + I(conc^2), weights = weights, model = FALSE)
  beta <- stats::coef(lmfit)
  a <- beta[["I(conc^2)"]]
  b <- beta[["conc"]]
  cc <- beta[["(Intercept)"]]
  model <- function(x) {
    det <- b^2 - 4 * a * (cc - x)
    det <- ifelse(det < 0, 0, det) # det < 0
    conc <- (-b + sqrt(det)) / (2 * a)
    conc
  }
  list(model = model, inv_mod = lmfit)
}
#' @rdname fit_calcurve_model
linear_calcurve_model <- function(conc, signal, weights) {
  lmfit <- stats::lm(signal ~ conc, weights = weights, model = FALSE)
  beta <- stats::coef(lmfit)
  b1 <- beta[["conc"]]
  b0 <- beta[["(Intercept)"]]
  model <- function(x) {
    (x - b0) / b1
  }
  list(model = model, inv_mod = lmfit)
}

#' Compute the concentration of features
#'
#' @param sumexp A [`SumExp::SumExp`] object of the samples
#' @param mat_id The name of a matrix in `sumexp`
#' @param log_scale Whether to use the logarithmic scale for the signal and concentration.
#'
#' @returns A matrix of the concentration of features
#' @md
#' @export
compute_concentration <- function(sumexp, mat_id, log_scale = FALSE) {
  mat <- sumexp[[mat_id]]
  models <- SumExp::row_df(sumexp)$calcurve_model
  no_model <- is.na(models)
  # Calculate the concentration of each feature
  conc <- sapply(rownames(mat), \(i_feature) {
    if (no_model[i_feature]) {
      return(rep(NA_real_, ncol(mat)))
    }
    v <- mat[i_feature, ]
    # Concentration by the best model
    models[[i_feature]]$best_model(v)
  }) |>
    t()          # Features to rows
  if (log_scale) {
    conc <- exp(conc)    # Back transform
  }
  colnames(conc) <- colnames(mat)
  conc <- labelled::set_label_attribute(conc, "Concentration")
  conc
}

##  POST CALIBRATION  --------------------------------------

#' Replace the values below the LLOQ and LOD
#'
#' @param sumexp A [`SumExp::SumExp`] object
#' @param conc_mat_id The name of a matrix in `sumexp` with the concentration values
#'
#' @returns A [`SumExp::SumExp`] object with the concentration values below the LLOQ replaced with
#'   half of the LLOQ and the concentration values below the LOD replaced with 1/4 of the LLOQ. The
#'   concentration values are replaced in the `conc_mat_id` matrix.
#'
#' @md
#' @export
replace_below_lod_lloq <- function(sumexp, conc_mat_id) {
  stopifnot(conc_mat_id %in% names(sumexp))
  c_mat <- sumexp[[conc_mat_id]]
  lod <- SumExp::row_df(sumexp)[, "lod"]
  lloq <- SumExp::row_df(sumexp)[, "lloq"]

  out <- c_mat
  # Replace the values below LLOQ with half of the LLOQ
  out <- ifelse(c_mat < lloq, lloq / 2, out)
  # Replace the values below LOD with 1/4 of the LLOQ
  out <- ifelse(c_mat < lod, lloq / 4, out)
  out <- labelled::copy_labels(c_mat, out)
  sumexp[[conc_mat_id]] <- out
  sumexp
}

#' Replace concentration values below LLOQ
#'
#' Replace concentration values of which the signal is below the average signal of the LLOQ with a
#' half of the LLOQ concentration value.
#'
#' @param sumexp A [`SumExp::SumExp`] object
#' @param conc_mat_id The name of a matrix in `sumexp` with the concentration values
#'
#' @returns A [`SumExp::SumExp`] object with the concentration values below LLOQ after the
#'   replacement.
#' @md
#' @export
replace_conc_whose_signal_below_lloq <- function(sumexp, conc_mat_id) {
  c_mat <- sumexp[[conc_mat_id]]  # Concentration matrix
  signal_mat <- sumexp[[util$src_mat_id_for_conc(sumexp)]]  # Signal matrix

  lod <- SumExp::row_df(sumexp)[["lod"]]
  lloq <- SumExp::row_df(sumexp)[["lloq"]]
  avg_signal_at_lloq <- SumExp::row_df(sumexp)[["avg_signal_at_lloq"]]

  # Replace the values below LLOQ with half of the LLOQ
  out <- ifelse(signal_mat < avg_signal_at_lloq & c_mat >= lod, lloq / 2, c_mat)
  out <- labelled::copy_labels(c_mat, out)
  sumexp[[conc_mat_id]] <- out
  sumexp
}

#' Replace concentration values whose signal is above the average signal of the LLOQ
#'
#' Replace concentration values of which the signal is above the average signal of the LLOQ with
#' the LLOQ concentration value.
#'
#' @param sumexp A [`SumExp::SumExp`] object
#' @param conc_mat_id The name of a matrix in `sumexp` with the concentration values
#'
#' @returns A [`SumExp::SumExp`] object with the concentration values above LLOQ after the
#'   replacement.
#' @md
#' @export
replace_conc_whose_signal_above_lloq <- function(sumexp, conc_mat_id) {
  c_mat <- sumexp[[conc_mat_id]]  # Concentration matrix
  signal_mat <- sumexp[[util$src_mat_id_for_conc(sumexp)]]  # Signal matrix
  
  lloq <- SumExp::row_df(sumexp)[["lloq"]]
  avg_signal_at_lloq <- SumExp::row_df(sumexp)[["avg_signal_at_lloq"]]

  # Replace the values above LLOQ with LLOQ
  out <- ifelse(signal_mat >= avg_signal_at_lloq & c_mat < lloq, lloq, c_mat)
  out <- labelled::copy_labels(c_mat, out)
  sumexp[[conc_mat_id]] <- out
  sumexp
}

#' Set concentration when the signal is below the LOD signal
#'
#' When the peak area (signal) is below the LOD signal, the calibrated concentration is set to 1/4 
#' of the LLOQ.
#'
#' @param sumexp A [`SumExp::SumExp`] object
#' @param conc_mat_id The name of a matrix in `sumexp` with the concentration values
#'
#' @returns A [`SumExp::SumExp`] object with concentration set to 1/4 of the LLOQ when the signal is
#'   below the LOD signal.
#' @md
#' @export
replace_conc_whose_signal_below_lod <- function(sumexp, conc_mat_id) {
  c_mat <- sumexp[[conc_mat_id]]  # Concentration matrix
  signal_mat <- sumexp[[util$src_mat_id_for_conc(sumexp)]]  # Signal matrix
  
  lloq <- SumExp::row_df(sumexp)[["lloq"]]
  lod_signal <- SumExp::row_df(sumexp)[["lod_signal"]]

  out <- ifelse(signal_mat < lod_signal, lloq / 4, c_mat)
  out <- labelled::copy_labels(c_mat, out)
  sumexp[[conc_mat_id]] <- out
  sumexp
}

#' Find out in which calibration point there is continuous increase in following points
#'
#' @param signal A vector with signals from instrument sorted in increasing theoretical conc
#'
#' @returns Index of the cal point at which point there is always increasing signal. `NA` when
#'   any signal is missing.
#' @md
#' @export
find_increasing_start <- function(signal) {
  signal <- as.double(unlist(signal)) # Also accepts a one-row data frame
  if (anyNA(signal)) return(NA_integer_)
  n <- length(signal)
  for (i in seq_len(n)) {
    if (all(diff(as.double(signal[i:n])) > 0)) {
      return(i)
    }
  }
  return(NA) # No such index found
}
