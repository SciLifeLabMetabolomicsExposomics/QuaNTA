# ------------------------------------------------------------------------------------------- #
# Functions for processing of the data
# ------------------------------------------------------------------------------------------- #
box::use(util = ./msdial_utils)

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
#' The mean of the non-zero values of the feature in the samples of the same type (`sample_type`).
#' When the feature has no non-zero value in that type, the mean of its non-zero values in all
#' samples is used instead.
#'
#' @param se A [`SumExp::SumExp`] object
#' @param mat_id The name of a matrix in `se` to be imputed
#' @returns A list of two elements:
#' * `se`: the [`SumExp::SumExp`] object with the imputed matrix
#' * `imputed`: a tibble of the imputed values (`feature_id`, `sample_id`, `sample_type`, `value`,
#'   and `fallback`: `TRUE` when the mean of all samples was used)
#' @md
#' @export
impute_zeros_with_mean_of_same_type <- function(se, mat_id) {
  mat <- se[[mat_id]]
  type <- SumExp::col_df(se)[["sample_type"]]
  non_zero <- mat
  non_zero[!is.finite(non_zero) | non_zero == 0] <- NA
  avg_all <- rowMeans(non_zero, na.rm = TRUE)
  imputed <- list()
  for (typ in unique(type)) {
    i_typ <- type == typ
    # The mean of the same type; NaN when the feature has no non-zero value in that type
    avg_typ <- rowMeans(non_zero[, i_typ, drop = FALSE], na.rm = TRUE)
    fallback <- is.nan(avg_typ)
    avg_typ[fallback] <- avg_all[fallback]
    for (ii in seq_len(nrow(mat))) {
      is_zero <- i_typ & mat[ii, ] == 0
      if (!any(is_zero)) next
      mat[ii, is_zero] <- avg_typ[ii]
      imputed[[length(imputed) + 1]] <- tibble::tibble(
        feature_id = rownames(mat)[ii], sample_id = colnames(mat)[is_zero], sample_type = typ,
        value = unname(avg_typ[ii]), fallback = fallback[[ii]]
      )
    }
  }
  se[[mat_id]] <- mat
  imputed <- if (length(imputed) == 0) {
    tibble::tibble(feature_id = character(), sample_id = character(), sample_type = character(),
                   value = numeric(), fallback = logical())
  } else {
    dplyr::bind_rows(imputed)
  }
  list(se = se, imputed = imputed)
}

#' Identify outliers
#'
#' Missing values (`NA`) are left out of the mean and SD, and are not outliers themselves.
#'
#' @param times A numeric value for the threshold. mean +/- times * sd
#' @param x A numeric vector
#'
#' @returns A logical vector indicating whether each element is an outlier
#' @export
identify_outliers <- function(x, times = 3) {
  m <- mean(x, na.rm = TRUE)
  sd <- stats::sd(x, na.rm = TRUE)
  out <- (x > m + times * sd) | (x < m - times * sd)
  out[is.na(out)] <- FALSE
  out
}

#' Count outlying internal standard features per sample
#'
#' In log scale (peak areas deviate by factors). The mean and SD of each internal standard are of
#' its non-zero values; a zero (the IS was not found in the sample) counts as outlying.
#'
#' @param se A [`SumExp::SumExp`] object
#' @param mat_id The name of a matrix in `se`
#' @param times A numeric value for the threshold. mean +/- times * sd
#' @returns A numeric vector of the number of outlying internal standard features per sample
#' @md
#' @export
count_outliers_per_sample <- function(se, mat_id, times = 3) {
  x <- se[[mat_id]]
  if (nrow(x) == 0) return(stats::setNames(rep(0, ncol(x)), colnames(x)))   # No IS left
  is_missing <- !is.finite(x) | x <= 0
  x[is_missing] <- NA
  x <- log(x)
  outlying <- apply(x, 1, identify_outliers, times = times) | t(is_missing)
  # A samples x IS matrix (transposed by `apply` above); `apply` gives a vector with one IS only
  outlying <- matrix(outlying, nrow = ncol(x), dimnames = list(colnames(x), rownames(x)))
  return(rowSums(outlying))
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
  # Find the closest internal standard feature (`NA` without a retention time or without any IS:
  # `which.min()` then gives nothing, and `sapply()` would return a list)
  i_closest <- vapply(rt_x, \(.x) {
    i <- which.min(abs(rt_istd - .x))
    if (length(i) == 0) NA_integer_ else i
  }, integer(1))
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
#' median over the classes is taken. Per batch (`by_batch`): within each QC class of each batch,
#' the median over the classes of a batch, then the median over the batches.
#'
#' @param se A [`SumExp::SumExp`] object
#' @param mat_id The name of a matrix in `se`
#' @param by_batch Per batch (`batch_id`) instead of all batches pooled
#' @returns A numeric vector of RSD% per feature (rows of `se`); `NA` when it cannot be computed,
#'   e.g. all values are zero or missing
#' @export
qc_rsd_per_feature <- function(se, mat_id, by_batch = FALSE) {
  qc_se <- util$extract_ctrl_smpl_cat(se, "QC")
  batch <- as.character(SumExp::col_df(qc_se)$batch_id)
  median_na <- function(m) apply(m, 1, \(v) if (all(is.na(v))) NA_real_ else stats::median(v, na.rm = TRUE))
  if (by_batch && length(unique(batch)) > 1) {
    # Per batch: without the zero values (as the RSD% shown per batch)
    per_batch <- vapply(unique(batch), function(b) {
      unname(.qc_rsd_median_over_classes(qc_se[, batch == b], mat_id, exclude_zero = TRUE))
    }, numeric(nrow(se)))
    out <- median_na(matrix(per_batch, nrow = nrow(se)))
  } else {
    out <- .qc_rsd_median_over_classes(qc_se, mat_id)
  }
  stats::setNames(out, rownames(se))
}

#' QC RSD% of each feature for all batches pooled ("Global") and, per batch, within each batch
#'
#' As [qc_rsd_per_feature()]: the median over the QC classes of the RSD% within each class.
#' Processed per batch, the zero values are left out (also in "Global").
#'
#' @inheritParams qc_rsd_per_feature
#' @returns A matrix: features in rows (`rownames(se)`), "Global" and the batches in columns
#' @export
qc_rsd_feature_by_batch <- function(se, mat_id, by_batch = FALSE) {
  qc_se <- util$extract_ctrl_smpl_cat(se, "QC")
  batch <- as.character(SumExp::col_df(qc_se)$batch_id)
  by_batch <- by_batch && length(unique(batch)) > 1
  cols <- list(Global = .qc_rsd_median_over_classes(qc_se, mat_id, exclude_zero = by_batch))
  if (by_batch) {
    for (b in unique(batch)) cols[[b]] <- .qc_rsd_median_over_classes(qc_se[, batch == b], mat_id, exclude_zero = TRUE)
  }
  out <- do.call(cbind, lapply(cols, unname))
  dimnames(out) <- list(rownames(se), names(cols))
  out
}

#' The number of features above the QC RSD% limit, per batch and type (app and report 3.12)
#'
#' @param rsd The matrix of [qc_rsd_feature_by_batch()]
#' @param targeted Whether each feature (row) is a targeted compound
#' @param counted The features counted (not removed before, e.g. by the blank filter; not the vIS)
#' @param limit The limit of each feature (%)
#' @param used Whether the limit of each feature was used by the QC RSD% filter
#' @returns A tibble of `batch`, `type` ("Targeted", "Non-targeted"), `n_above`, `n_total` (with an
#'   RSD%), `limit` and `used`
#' @export
qc_rsd_counts <- function(rsd, targeted, counted, limit, used) {
  purrr::map_dfr(colnames(rsd), function(b) {
    r <- rsd[, b]
    purrr::map_dfr(c(TRUE, FALSE), function(tg) {
      k <- counted & targeted == tg
      tibble::tibble(batch = b, type = if (tg) "Targeted" else "Non-targeted",
                     n_above = sum(k & !is.na(r) & r > limit, na.rm = TRUE),
                     n_total = sum(k & !is.na(r)),
                     limit = if (any(k)) limit[k][1] else NA_real_, used = any(used[k]))
    })
  })
}

#' The median over the QC classes (with at least two samples) of the RSD% within each class
.qc_rsd_median_over_classes <- function(qc_se, mat_id, exclude_zero = FALSE) {
  qc_class <- SumExp::col_df(qc_se)$Class
  classes <- names(which(table(qc_class) >= 2))
  rsd_perc <- function(x) {
    if (exclude_zero) {
      x <- x[!is.na(x) & x > 0]
      if (length(x) < 2) return(NA_real_)
    }
    m <- mean(x, na.rm = TRUE)
    if (!is.finite(m) || m == 0) NA_real_ else 100 * stats::sd(x, na.rm = TRUE) / m
  }
  if (length(classes) == 0) return(rep(NA_real_, nrow(qc_se)))
  per_class <- vapply(classes, function(cl) {
    apply(qc_se[[mat_id]][, qc_class == cl, drop = FALSE], 1, rsd_perc)
  }, numeric(nrow(qc_se)))
  per_class <- matrix(per_class, nrow = nrow(qc_se))
  apply(per_class, 1, \(v) if (all(is.na(v))) NA_real_ else stats::median(v, na.rm = TRUE))
}

#' RSD% of each feature within each QC group, without the zero values, for all batches pooled
#' ("Global") and, per batch, within each batch
#'
#' Zeros (not detected, or at most the blank after blank subtraction) are left out and counted. A
#' feature without values (removed, e.g. by the blank filter) has no RSD% and no count of zeros.
#'
#' @param m A numeric matrix (features in rows, the QC injections in columns)
#' @param qc_class The QC group of each column
#' @param batch The batch of each column
#' @param by_batch Also per batch
#' @returns A tibble of `batch` ("Global" or the batch ID), `QC`, `feature_id` (the row names of
#'   `m`), `rsd` (`NA` with fewer than two non-zero values), `n` (QC injections) and `n_zero`
#' @export
qc_rsd_by_group <- function(m, qc_class, batch, by_batch = FALSE) {
  batch <- as.character(batch)
  sets <- c(list(Global = rep(TRUE, ncol(m))),
            if (by_batch && length(unique(batch)) > 1) lapply(stats::setNames(nm = unique(batch)), \(b) batch == b))
  rows <- lapply(names(sets), function(b) {
    lapply(unique(qc_class[sets[[b]]]), function(cl) {
      cols <- sets[[b]] & qc_class == cl
      if (sum(cols) < 2) return(NULL)
      x <- m[, cols, drop = FALSE]
      removed <- rowSums(!is.na(x)) == 0
      nz <- x
      nz[!is.na(nz) & nz <= 0] <- NA
      n_nz <- rowSums(!is.na(nz))
      mu <- rowMeans(nz, na.rm = TRUE)
      sd <- sqrt(rowSums((nz - mu)^2, na.rm = TRUE) / (n_nz - 1))
      rsd <- ifelse(n_nz >= 2 & is.finite(mu) & mu > 0, 100 * sd / mu, NA_real_)
      tibble::tibble(batch = b, QC = cl, feature_id = rownames(m), rsd = rsd, n = sum(cols),
                     n_zero = ifelse(removed, NA_integer_, as.integer(rowSums(!is.na(x) & x <= 0))))
    })
  })
  out <- dplyr::bind_rows(unlist(rows, recursive = FALSE))
  if (nrow(out) == 0) {
    out <- tibble::tibble(batch = character(), QC = character(), feature_id = character(),
                          rsd = numeric(), n = integer(), n_zero = integer())
  }
  out
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

#' The ratio of the blank filter: the maximum or the mean of each feature in the study samples
#' over its mean in the blanks
#'
#' Only the study samples count: not the QCs, calibration standards or blanks (their injection
#' type, `.ctrl_cat`, is empty).
#'
#' @param sumexp A [`SumExp::SumExp`] object
#' @param mat_id The name of a matrix in `sumexp`
#' @param stat "max" or "mean": the statistic of the study samples
#' @returns A named numeric vector (one per feature); `NA` (or `Inf`, `NaN`) when it cannot be
#'   computed, e.g. no study samples, no blanks or no values
#' @export
blank_ratio <- function(sumexp, mat_id, stat = c("max", "mean")) {
  stat <- match.arg(stat)
  cat_ <- util$ctrl_smpl_cat(sumexp)
  m <- sumexp[[mat_id]]
  is_blank <- cat_ %in% "Blank"
  is_study <- is.na(cat_) | cat_ == ""
  blank_avg <- rowMeans(m[, is_blank, drop = FALSE], na.rm = TRUE)
  s <- m[, is_study, drop = FALSE]
  sample_stat <- if (stat == "max") {
    suppressWarnings(apply(s, 1, max, na.rm = TRUE))   # -Inf without values
  } else {
    rowMeans(s, na.rm = TRUE)
  }
  stats::setNames(sample_stat / blank_avg, rownames(m))
}

#' Filter features based on blank samples
#'
#' Criteria: Keep a feature if [`stat`] (maximum or mean) of the study samples > [`factor`] *
#' mean of the blanks (QCs and calibration standards do not count; see [blank_ratio()])
#'
#' @param se A [`SumExp::SumExp`] object
#' @param mat_id The name of a matrix in `se`
#' @param factor A numeric value for the filtering factor.
#' @param stat "max" or "mean": the statistic of the study samples compared with the blanks
#' @returns A [`SumExp::SumExp`] object with the features filtered
#' @export
filter_by_blank <- function(sumexp, mat_id, factor = 5, stat = c("max", "mean")) {
  stat <- match.arg(stat)
  cat_ <- util$ctrl_smpl_cat(sumexp)
  if (!any(cat_ %in% "Blank")) {
    warning("No blank samples found. Skipping blank filtering.")
    return(sumexp)
  }
  if (!any(is.na(cat_) | cat_ == "")) {
    warning("No study samples found. Skipping blank filtering.")
    return(sumexp)
  }

  # A blank mean of 0 gives Inf (kept) or, without a signal in the samples either, NaN (removed)
  to_keep <- blank_ratio(sumexp, mat_id, stat) > factor
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
#' @param subtract `FALSE`: nothing is subtracted (processing option `--blank_subtract`); the
#'   output matrices are the input ones, without the blanks
#' @returns A [`SumExp::SumExp`] object with the blank values subtracted.
#'   The columns of the blanks are removed from the `sumexp`.
#' @md
#' @export
add_blank_subtracted_sumexp <- function(sumexp,
                                        no_change,
                                        mat_ids,
                                        out_mat_ids,
                                        subtract = TRUE) {
  stopifnot(length(mat_ids) == length(out_mat_ids))
  g <- ifelse(util$ctrl_smpl_cat(sumexp) == "Blank", "Blank", "Other")
  se_lst <- SumExp::split_columns(sumexp, g)
  blank_se <- se_lst[["Blank"]]     # NULL when there are no blank samples (e.g. a batch without)
  has_blank <- !is.null(blank_se) && ncol(blank_se) > 0
  sumexp <- se_lst[["Other"]]
  no_change <- no_change[g == "Other"]    # Blank has been split already
  if (!subtract) has_blank <- FALSE        # Nothing subtracted
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
      labelled::set_label_attribute(paste(x_lab, if (subtract) "(blank adjusted)" else "(no blank subtraction)"))
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

##  WITH DERIVED MIN/MAX CONCENTRATION  --------------------

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
#' @param enough_n Enough number of concentrations for the calibration curve; a range with fewer
#'   points is extended upwards to `max(enough_n, min_n)` points
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
  # Counting from minimum: at least `min_n` points (a range cut at the highest sample is extended
  # to the required minimum, not only to `enough_n`, which may be smaller)
  i_enough_from_min <- i_min + max(enough_n, min_n) - 1
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
  # Why a chemical has no valid calibration range (NA: valid), in the order of the checks above
  n_from_lloq <- vapply(lloq, \(l) if (is.na(l)) 0L else sum(conc_pts >= l), integer(1))
  range_issue <- dplyr::case_when(
    rowSums(mat_m > lloq_signal, na.rm = TRUE) == 0 ~
      "No calibration level above the LLOQ signal",
    is.na(lloq) ~ "No calibration level above the LLOQ signal with RSD% <= 20%",
    max_c_conc %in% 0 ~ "No signal in the samples",
    is.na(max_c_conc) ~ sprintf("Only %d calibration level(s) from the LLOQ upwards (minimum %d)",
                                n_from_lloq, min_n),
    TRUE ~ NA_character_
  )
  # No study sample or QC at or above the LLOQ (signal below the mean signal of the LLOQ level):
  # the full calibration range, from the LLOQ to the highest level, instead of one cut at the
  # highest sample (`enough_n`), to show where the chemical could have been quantified
  mat_sq <- util$exclude_ctrl_smpl_cat(se_lst$quant, excl_cat = "Blank")[[mat_id]]
  max_in_sq <- suppressWarnings(apply(mat_sq, 1, max, na.rm = TRUE))
  no_quant_samples <- !is.na(lloq) & !(max_in_sq >= avg_signal_at_lloq)
  no_quant_samples[is.na(no_quant_samples)] <- FALSE
  full <- no_quant_samples & !is.na(max_c_conc)
  max_c_conc[full] <- max(conc_pts)
  # data.frame for SumExp::row_df, instead of tibble
  limits_df <- data.frame(min_c_conc, max_c_conc, lod, lloq, avg_signal_at_lloq,
                          lod_signal, lloq_signal, range_issue, no_quant_samples)
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

#' Calibration range of curves kept despite an invalid range (`--keep_all_calcurves`)
#'
#' The range runs from the LLOQ (without an LLOQ: the lowest calibration level above zero) up to
#' the highest calibration level, and is extended downwards to have at least `min_n` levels.
#'
#' @param sumexp A [`SumExp::SumExp`] object with the calibration curve limits added by
#'   [find_calib_lim_pts_and_llox_from_llox_signal()]
#' @param rows A logical vector: the chemicals (rows) whose range is set
#' @param min_n Required minimum number of concentrations for the calibration curve
#'
#' @returns `sumexp` with `min_c_conc` and `max_c_conc` set for `rows`
#' @md
#' @export
force_calibration_range <- function(sumexp, rows, min_n) {
  conc_pts <- util$spiked_conc_pts(sumexp)
  conc_pts <- sort(unique(conc_pts[!is.na(conc_pts) & conc_pts > 0]))
  n <- length(conc_pts)
  rd <- SumExp::row_df(sumexp)
  i_min <- match(rd$lloq, conc_pts)
  i_min[is.na(i_min)] <- 1L
  i_min <- pmin(i_min, max(1L, n - min_n + 1L))
  rd$min_c_conc[rows] <- conc_pts[i_min[rows]]
  rd$max_c_conc[rows] <- conc_pts[n]
  SumExp::row_df(sumexp) <- rd
  sumexp
}

#' Average the calibration curve replicates within each batch
#'
#' The calibration curve samples of each batch and concentration level are replaced by one sample
#' with their average (of every matrix; `NA` values left out). The other samples are unchanged.
#' The batches are those of the data file: without processing per batch (all samples in one batch,
#' `all`), its batch ID (`org_batch_id`), so that every batch still gives one point per level.
#' The averaged samples are named "CalAvg_B<batch>_<concentration>" (sample ID and name) and keep
#' the sample information of the first replicate; `n_averaged` is the number of replicates.
#'
#' @param se A [`SumExp::SumExp`] object with all samples
#' @returns A [`SumExp::SumExp`] object with the averaged calibration curve samples last
#' @export
average_calcurve_replicates <- function(se) {
  cd <- SumExp::col_df(se)
  is_cc <- util$ctrl_smpl_cat(se) == "CalCurve"
  if (!any(is_cc)) return(se)
  conc <- util$spiked_conc_pts(se)
  batch <- as.character(cd$batch_id)
  if ("org_batch_id" %in% names(cd)) batch <- dplyr::coalesce(as.character(cd$org_batch_id), batch)
  key <- ifelse(is_cc, paste(batch, conc, sep = "|"), NA_character_)
  keys <- unique(key[is_cc])
  first <- match(keys, key)
  ids <- paste0("CalAvg_B", batch[first], "_",
                format(conc[first], scientific = FALSE, drop0trailing = TRUE, trim = TRUE))
  average <- function(m) {
    avg <- vapply(keys, function(k) {
      r <- rowMeans(m[, key %in% k, drop = FALSE], na.rm = TRUE)
      r[is.nan(r)] <- NA_real_
      r
    }, numeric(nrow(m)))
    avg <- matrix(avg, nrow = nrow(m), dimnames = list(rownames(m), ids))
    labelled::copy_labels(m, cbind(m[, !is_cc, drop = FALSE], avg))
  }
  new_cd <- cd[c(which(!is_cc), first), , drop = FALSE]
  rownames(new_cd) <- c(rownames(cd)[!is_cc], ids)
  new_cd$sample_name[sum(!is_cc) + seq_along(ids)] <- ids
  new_cd$n_averaged <- c(rep(1L, sum(!is_cc)), as.integer(table(factor(key[is_cc], levels = keys))))
  mats <- lapply(stats::setNames(nm = names(se)), function(nm) average(se[[nm]]))
  do.call(SumExp::SumExp, c(mats, list(col_df = new_cd, row_df = SumExp::row_df(se),
                                       metadata = SumExp::metadata(se))))
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

#' Upper limit of quantification (ULOQ): the top of the linear range of a calibration curve
#'
#' The ULOQ is located the same way whether the curve is cut (`--enforce_linear_range`) or not,
#' and regardless of the minimum number of calibration levels: it is always the actual top of the
#' linear range. A cut curve keeps at least the minimum number of levels, so it can end above the
#' ULOQ (`top`); only `cut` decides whether the returned model and signal are the cut ones.
#'
#' - Linear models ([trim_to_linear_range()]): back-calculation of the calibration levels.
#' - Log calibration, any model ([linear_range_of_log_curve()]): the SCALiR algorithm.
#' - Quadratic models (without log calibration): no ULOQ.
#'
#' Values between the ULOQ and the top of the curve are above the linear range; values above the
#' top of the curve are extrapolated.
#' @name uloq
NULL

# Levels below which the linear range is not looked for: a line through two levels fits them exactly
.uloq_min_levels <- 3

#' The note of the linear range of a curve: the levels above the ULOQ, and where the curve ends
#'
#' @param label "Linear range" or "Linear range (SCALiR)"
#' @param uloq,top The ULOQ and the top of the (cut) curve
#' @param above The levels above the ULOQ (highest first)
#' @param dev Their back-calculated deviations (`NULL`: not shown)
#' @param cut,min_n As in [trim_to_linear_range()]
#' @param extra Further notes
#' @noRd
.linear_range_note <- function(label, uloq, top, above, dev = NULL, cut, min_n, extra = NULL) {
  pct <- function(x) sprintf("%.1f%%", 100 * x)
  num <- function(x) as.character(signif(x, 4))
  kept <- above[above <= top]   # In the cut curve although above the ULOQ (minimum of levels)
  note <- c(
    if (length(above) > 0) {
      sprintf("%s up to %s: level(s) %s above it%s; %s", label, num(uloq), paste(num(above), collapse = ", "),
              if (length(dev) > 0) paste0(" (back-calculated deviation ", paste(pct(dev), collapse = ", "), ")") else "",
              if (!cut) "not cut" else if (length(kept) == 0) "dropped" else
                sprintf("level(s) %s kept in the curve (minimum of %d levels), the others dropped",
                        paste(num(kept), collapse = ", "), min_n))
    },
    extra
  )
  if (length(note) == 0) NA_character_ else paste(note, collapse = "; ")
}

#' Trim a linear calibration curve to its linear range
#'
#' The mean signal of each calibration level of the curve is back-calculated with the linear
#' model. While the top level deviates from its nominal concentration by more than `max_dev` and
#' more than 3 levels are left, the top level is left out and the linear model refitted (with the
#' same weighting method, linear models only). The highest level left is the ULOQ. The curve keeps
#' at least `min_n` levels: when cut, it ends at the ULOQ or at its `min_n`-th level (`top`).
#'
#' @param conc Spiked concentration of each calibration sample
#' @param signal Signal of the calibration samples of one chemical (`NA`: not in the curve)
#' @param model The fitted linear model of [fit_and_test_calcurve_model()]
#' @param max_dev The maximum relative deviation of the top level (e.g. 0.15 for 15%)
#' @param min_n The minimum number of calibration levels
#' @param weight_method As in [fit_and_test_calcurve_model()]
#' @param cut Whether the curve is cut (`--enforce_linear_range`); only changes the note
#' @returns A list of `uloq`, `top` (the top of the cut curve: the ULOQ, or higher to keep `min_n`
#'   levels), `model` (refitted on the levels up to `top`), `signal` (with the levels above `top`
#'   `NA`), `dropped` (the levels above `top`) and `note` (`NA` when the curve is linear over its
#'   whole range)
#' @seealso [uloq]
#' @export
trim_to_linear_range <- function(conc, signal, model, max_dev, min_n, weight_method, cut = TRUE) {
  above <- numeric()
  dev <- numeric()
  curve <- NULL   # The cut curve: where `min_n` levels are left (NULL: at the ULOQ)
  repeat {
    levels <- sort(unique(conc[!is.na(signal)]))
    top <- levels[length(levels)]
    if (is.null(curve) && length(levels) <= min_n) curve <- list(top = top, model = model, signal = signal)
    top_mean <- mean(signal[!is.na(signal) & conc == top])
    top_dev <- abs(model$best_model(top_mean) - top) / top
    if (!is.finite(top_dev) || top_dev <= max_dev || length(levels) <= .uloq_min_levels) break
    above <- c(above, top)
    dev <- c(dev, top_dev)
    signal[conc == top] <- NA
    model <- fit_and_test_calcurve_model(conc, signal, weight_method = weight_method, linear_only = TRUE)
  }
  if (is.null(curve) || curve$top <= top) curve <- list(top = top, model = model, signal = signal)
  still <- if (is.finite(top_dev) && top_dev > max_dev) {
    sprintf("%s level %s still deviates %.1f%% (%d levels left)",
            if (length(above) > 0) "its top" else "Linear range: the top", signif(top, 4), 100 * top_dev, length(levels))
  }
  list(uloq = top, top = curve$top, model = curve$model, signal = curve$signal, dropped = above[above > curve$top],
       note = .linear_range_note("Linear range", top, curve$top, above, dev, cut, min_n, still))
}

#' The top of the linear range by the SCALiR algorithm
#'
#' Ponce et al. 2024, Anal. Chem. 96, 6566: ln(signal) = ln(conc) + b (slope 1); while the mean
#' squared residual is above `max_resid` and more than `min_n` points are left, the point farthest
#' from the line is removed. The top is the highest concentration left.
#'
#' @param conc Spiked concentration of each calibration sample
#' @param signal Signal of the calibration samples of one chemical, in the original scale (`NA`:
#'   not in the curve)
#' @returns The top of the linear range, `NA` with fewer than `min_n` positive points
#' @export
scalir_top <- function(conc, signal, max_resid = 0.01, min_n = 3) {
  ok <- !is.na(signal) & conc > 0 & signal > 0
  if (sum(ok) < min_n) return(NA_real_)
  cx <- conc[ok]
  x <- log(cx)
  y <- log(signal[ok])
  repeat {
    res <- y - (x + mean(y - x))   # Residuals of the line with slope 1
    if (mean(res^2) <= max_resid || length(x) <= min_n) break
    j <- which.max(abs(res))
    x <- x[-j]; y <- y[-j]; cx <- cx[-j]
  }
  max(cx)
}

#' The linear range of a calibration curve fitted in log scale (`--log_calibration`)
#'
#' The ULOQ is the top of the linear range by SCALiR ([scalir_top()]) among the points of the
#' curve. Without the points above it, the points SCALiR removed below it count again, so SCALiR
#' is run again on the points up to the ULOQ, until the ULOQ does not change. The curve keeps at
#' least `min_n` levels: when cut, it ends at the ULOQ or at its `min_n`-th level (`top`), and the
#' model is refitted on the points up to there.
#'
#' @param conc Spiked concentration of each calibration sample
#' @param signal Signal of the calibration samples of one chemical, in the original scale
#' @param signal_fit The signal the curve is fitted on (original scale; `NA`: not in the curve)
#' @param model The fitted model of [fit_and_test_calcurve_model()]
#' @param min_n The minimum number of calibration levels
#' @param refit A function of `signal_fit` that refits the model
#' @param cut Whether the curve is cut (`--enforce_linear_range`); only changes the note
#' @returns A list as [trim_to_linear_range()]
#' @seealso [uloq]
#' @export
linear_range_of_log_curve <- function(conc, signal, signal_fit, model, min_n, refit, cut = TRUE) {
  levels <- sort(unique(conc[!is.na(signal_fit)]))
  sig <- ifelse(is.na(signal_fit), NA, signal)
  uloq <- NA_real_
  repeat {
    u <- scalir_top(conc, sig)
    if (is.na(u)) break   # Fewer than 3 points left: the ULOQ found so far
    uloq <- u
    if (!any(!is.na(sig) & conc > u)) break
    sig[conc > u] <- NA
  }
  if (is.na(uloq)) {
    return(list(uloq = NA_real_, top = max(levels), model = model, signal = signal_fit, dropped = numeric(),
                note = "Linear range (SCALiR): fewer than 3 points with a signal"))
  }
  # The curve: up to the ULOQ, but at least `min_n` levels
  top <- max(uloq, levels[min(min_n, length(levels))])
  dropped <- sort(levels[levels > top], decreasing = TRUE)
  if (length(dropped) > 0) {
    signal_fit[conc > top] <- NA
    model <- refit(signal_fit)
  }
  list(uloq = uloq, top = top, model = model, signal = signal_fit, dropped = dropped,
       note = .linear_range_note("Linear range (SCALiR)", uloq, top, sort(levels[levels > uloq], decreasing = TRUE),
                                 cut = cut, min_n = min_n))
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
#' @param linear_only Whether to fit linear models only (no quadratic ones)
#' @param log_log Whether to fit a straight line in log-log scale (`--log_calibration`; see
#'   [loglog_calcurve_model()]): then the weight is 1 and there are no quadratic models.
#'   `conc` and `signal` are always given in the original scale.
#'
#' @returns A list with the best model, the name of it, the R2 values of all models, and the
#'   number of unique concentrations. The best model is chosen as the model with the highest R2
#'   after applying the penalty to quadratic models; the penalty is used only for this choice
#'   (`R2s` are the real R2 values, `R2s_adj` those with the penalty). `NA` without a signal.
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
                                        penalty_quadratic = 0,
                                        linear_only = FALSE,
                                        log_log = FALSE) {
  # If no signal return NA
  if (all(is.na(signal) | signal == 0)) {
    return(NA)
  }
  if (log_log) {
    m <- loglog_calcurve_model(conc, signal)
    if (is.null(m)) return(NA)
    r2 <- c("linear-1" = suppressWarnings(summary(m$inv_mod)$r.squared))
    return(rlang::list2(
      "best_model" = m$model, "best_model_name" = "linear-1", "R2s" = r2, "R2s_adj" = r2,
      "n_conc" = length(unique(conc[!is.na(signal)])), "inv_model" = m$inv_mod
    ))
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

  # Quadratic models only when not restricted to linear ones (`--linear_only`)
  qmodels <- if (linear_only) list() else lapply(weights_alt, \(w) quadratic_calcurve_model(conc, signal, weights = w))
  if (length(qmodels) > 0) names(qmodels) <- paste("quadratic", names(qmodels), sep = "-")

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
#' A straight line in log-log scale (`--log_calibration`): ln(signal) = b0 + b1 ln(conc), weight 1,
#' fitted on the points with a positive signal and concentration (a log of 0 does not exist). The
#' same scale as SCALiR, which finds the ULOQ of these curves ([scalir_top()]).
#'
#' @returns `NULL` with fewer than 2 such points; otherwise `model` (the concentration of a signal,
#'   both in the original scale; 0 for a signal of 0 or less, which is below the LOD) and `inv_mod`
#'   (the fit, `lm` of the log signal on `conc` = the log concentration)
#' @rdname fit_calcurve_model
loglog_calcurve_model <- function(conc, signal) {
  ok <- !is.na(signal) & !is.na(conc) & signal > 0 & conc > 0
  if (sum(ok) < 2) return(NULL)
  d <- data.frame(conc = log(conc[ok]), signal = log(signal[ok]))
  lmfit <- stats::lm(signal ~ conc, data = d, model = FALSE)
  beta <- stats::coef(lmfit)
  b1 <- beta[["conc"]]
  b0 <- beta[["(Intercept)"]]
  model <- function(x) {
    out <- ifelse(is.na(x), NA_real_, 0)
    pos <- !is.na(x) & x > 0
    out[pos] <- exp((log(x[pos]) - b0) / b1)
    out
  }
  list(model = model, inv_mod = lmfit)
}

#' Compute the concentration of features
#'
#' @param sumexp A [`SumExp::SumExp`] object of the samples
#' @param mat_id The name of a matrix in `sumexp` (the signal, in the original scale; also for
#'   curves fitted in log-log scale, whose models take the original scale)
#'
#' @returns A matrix of the concentration of features
#' @md
#' @export
compute_concentration <- function(sumexp, mat_id) {
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
  colnames(conc) <- colnames(mat)
  conc <- labelled::set_label_attribute(conc, "Concentration")
  conc
}

##  POST CALIBRATION  --------------------------------------

#' Replace the values of a concentration matrix where a condition holds
#'
#' Where the condition or the replacement is `NA` (e.g. no LLOQ for a calibration curve kept by
#' `--keep_all_calcurves`), the value is kept.
#'
#' @param c_mat A concentration matrix (chemicals in rows)
#' @param cond A logical matrix of the same size
#' @param value The replacement, one per chemical (row)
#' @returns `c_mat` with the replaced values and its labels
.replace_where <- function(c_mat, cond, value) {
  value <- matrix(value, nrow = nrow(c_mat), ncol = ncol(c_mat))
  cond <- cond & !is.na(value)
  cond[is.na(cond)] <- FALSE
  out <- c_mat
  out[cond] <- value[cond]
  labelled::copy_labels(c_mat, out)
}

#' Apply the LOD / LLOQ rules to the calibrated concentrations
#'
#' Each value is decided once, from its calculated concentration and its signal (no value is
#' replaced again after a first replacement):
#'
#' | Signal                                          | Concentration | Result      |
#' |-------------------------------------------------|---------------|-------------|
#' | below the LOD signal                            | any           | LLOQ / 4    |
#' | from the LOD signal to below the average signal | any           | LLOQ / 2    |
#' |   of the LLOQ level                             |               |             |
#' | at or above the average signal of the LLOQ level | below the LLOQ | the LLOQ   |
#' | at or above the average signal of the LLOQ level | at or above the LLOQ | unchanged (calibrated) |
#'
#' Chemicals without an LLOQ or the signals of the limits (e.g. curves kept although not valid)
#' are not changed by the rules that need them.
#'
#' @param sumexp A [`SumExp::SumExp`] object with the concentrations, the signal they were
#'   calculated from (see `util$src_mat_id_for_conc()`) and the `lloq`, `lod_signal` and
#'   `avg_signal_at_lloq` of each chemical (`row_df`; signals in the original scale)
#' @param conc_mat_id The name of the matrix with the calculated concentrations; it is replaced by
#'   the result
#' @returns `sumexp` with the concentrations after the rules
#' @md
#' @export
apply_llox_rules <- function(sumexp, conc_mat_id) {
  stopifnot(conc_mat_id %in% names(sumexp))
  conc <- sumexp[[conc_mat_id]]
  signal <- sumexp[[util$src_mat_id_for_conc(sumexp)]]
  rd <- SumExp::row_df(sumexp)
  lloq <- rd[["lloq"]]
  # Thresholds of each chemical (row) as matrices (signals in the original scale)
  as_mat <- function(v) matrix(v, nrow = nrow(conc), ncol = ncol(conc))
  lod_sig <- as_mat(rd[["lod_signal"]])
  lloq_sig <- as_mat(rd[["avg_signal_at_lloq"]])
  # The groups, all from the values before any replacement. A signal of 0 is never detected (also
  # when the LOD signal is 0, i.e. no signal in Cal0; see `util$is_detected()`)
  not_detected <- !util$is_detected(signal, lod_sig)
  detected <- !not_detected & signal < lloq_sig
  below_lloq <- signal >= lloq_sig & conc < as_mat(lloq)
  out <- conc
  out <- .replace_where(out, not_detected, lloq / 4)
  out <- .replace_where(out, detected, lloq / 2)
  out <- .replace_where(out, below_lloq, lloq)
  sumexp[[conc_mat_id]] <- labelled::copy_labels(conc, out)
  sumexp
}

#' Which concentrations the LOD/LLOQ rules replaced below the LOD or the LLOQ
#'
#' @param conc0 The concentrations before the rules
#' @param conc The concentrations after the rules (same dimensions)
#' @param lloq The LLOQ of each chemical (row)
#' @returns A character matrix: "n.d." (not detected: replaced by LLOQ/4), "n.q." (not quantified:
#'   replaced by LLOQ/2), otherwise `NA` (also for values set to the LLOQ, whose signal is at or
#'   above the LLOQ level)
#' @export
llox_flags <- function(conc0, conc, lloq) {
  lloq <- matrix(lloq, nrow = nrow(conc), ncol = ncol(conc))
  replaced <- !is.na(conc) & !is.na(lloq) & (is.na(conc0) | conc != conc0)
  flag <- matrix(NA_character_, nrow = nrow(conc), ncol = ncol(conc), dimnames = dimnames(conc))
  flag[replaced & conc == lloq / 4] <- "n.d."
  flag[replaced & conc == lloq / 2] <- "n.q."
  flag
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
