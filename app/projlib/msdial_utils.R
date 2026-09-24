# ------------------------------------------------------------------------------------------- #
# * Functions to access the elements of the MS-Dial data
# * Utility functions
# ------------------------------------------------------------------------------------------- #

#  -----  VARIABLES USED IN `read-msdial.R`  -----------------------------------

##  FEATURES  ----------------------------------------------

#' Feature variables
#' @export
conv_tbl <- tibble::tribble(
  ~given_id,         ~id,            ~label,
  "Alignment ID",    "alignment_id", "Alignment ID",
  "Metabolite name", "feature_name", "Feature Name",
  "Average Mz",      "mz",           "Average M/Z",
  "Quant mass",      "mz",           "Quant Mass",
  "Average Rt(min)", ".rt",          "Average Retention Time (min)",
  "Comment",         ".std_type",    "Standard Type", # e.g. "Quant", "IS", "vIS", or NA
)

#' Get the retention time of the calibration curve samples
#'
#' @param sumexp A [`SumExp::SumExp`] object
#' @returns A numeric vector of the retention times
#' @export
retention_time <- function(sumexp) {
  stopifnot(inherits(sumexp, "SumExp"))
  SumExp::row_df(sumexp)[[".rt"]]
}

#' Get the standard type of the features
#'
#' @param sumexp A [`SumExp::SumExp`] object
#' @returns A character vector of the standard types
#' @export
std_type <- function(sumexp) {
  stopifnot(inherits(sumexp, "SumExp"))
  SumExp::row_df(sumexp)[[".std_type"]]
}

#' Get which features are targeted features
#'
#' @param x A [`SumExp::SumExp`] object or a character vector of standard types
#' @param ... Not used
#'
#' @returns A logical vector indicating which features are targeted features
#' @export
is_targeted_feature <- function(x, ...) {
  UseMethod("is_targeted_feature")
}
#' @rdname is_targeted_feature
#' @export
is_targeted_feature.character <- function(x) {
  stopifnot(inherits(x, "character"))
  x == "Quant" | grepl("^Quant\\\\", x)
}
#' @rdname is_targeted_feature
#' @export
is_targeted_feature.SumExp <- function(x) {
  std_type(x) |>
    is_targeted_feature.character()
}

#' Get which features are internal standards
#'
#' @param x A [`SumExp::SumExp`] object or a character vector of standard types
#' @param ... Not used
#' @returns A logical vector indicating which features are internal standards
#' @export
is_internal_std <- function(x, ...) {
  UseMethod("is_internal_std")
}
#' @rdname is_internal_std
is_internal_std.character <- function(x) {
  stopifnot(inherits(x, "character"))
  x == "IS" | grepl("^IS\\\\", x)
}
#' @rdname is_internal_std
is_internal_std.SumExp <- function(x) {
  std_type(x) |>
    is_internal_std.character()
}

##  SAMPLES  -----------------------------------------------

#' Special control sample categories
#'
#' For example, "CalCurve", "QC", and "Blank" are special control sample categories.
#' @param sumexp A [`SumExp::SumExp`] object
#' @returns
#' [ctrl_smpl_cat()]: A character vector of the control sample categories.
#'
#' @export
ctrl_smpl_cat <- function(sumexp) {
  stopifnot(inherits(sumexp, "SumExp"))
  SumExp::col_df(sumexp)[[".ctrl_cat"]]
}
#' @rdname ctrl_smpl_cat
#' @description
#' [exclude_ctrl_smpl_cat()]: Exclude one or more special control sample categories
#'
#' @param excl_cat A character vector of the control sample categories to be excluded
#' @returns
#' [exclude_ctrl_smpl_cat()]: A [`SumExp::SumExp`] object with the specified control sample
#' categories excluded
#' @export
exclude_ctrl_smpl_cat <- function(sumexp, excl_cat) {
  stopifnot(is.character(excl_cat))
  excl_cat <- unique(excl_cat)
  if (length(excl_cat) == 0) {
    return(sumexp)
  }
  s_cat <- ctrl_smpl_cat(sumexp)
  sumexp[, (!s_cat %in% excl_cat) | is.na(s_cat)]
}
#' @rdname ctrl_smpl_cat
#' @description
#' [extract_ctrl_smpl_cat()]: Extract one or more special control sample categories
#' @returns
#' [extract_ctrl_smpl_cat()]: A [`SumExp::SumExp`] object with the specified control sample
#' categories extracted
#' @export
extract_ctrl_smpl_cat <- function(sumexp, cat) {
  stopifnot(is.character(cat))
  cat <- unique(cat)
  if (length(cat) == 0) {
    return(sumexp)
  }
  s_cat <- ctrl_smpl_cat(sumexp)
  sumexp[, !is.na(s_cat) & (s_cat %in% cat)]
}

#' Get which samples are calibration curve samples
#'
#' @param sumexp A [`SumExp::SumExp`] object including calibration curve samples
#' @returns A logical vector indicating which samples are calibration curve samples
#' @export
is_calcurve_sample <- function(sumexp) {
  stopifnot(inherits(sumexp, "SumExp"))
  ctrl_smpl_cat(sumexp) == "CalCurve"
}

#' Split the columns of a [`SumExp::SumExp`] object into the calibration curve and the other
#'
#' @param sumexp A [`SumExp::SumExp`] object including the calibration curve samples
#' @param out_names A character vector of length 2. The names of the output list.
#'   The first element is the name of the calibration curve samples.
#' @returns A list of two [`SumExp::SumExp`] objects with the names `CalCurve` and `Other`
#' @export
split_into_calcurve_and_other <- function(sumexp, out_names = c("CalCurve", "Other")) {
  stopifnot(length(out_names) == 2)
  g <- ifelse(is_calcurve_sample(sumexp), out_names[1], out_names[2])
  SumExp::split_columns(sumexp, g)
}

#' The name of the column containing the spiked concentration points
#' @export
spiked_conc_pts_name <- "c_conc"

#' Get the spiked concentration points
#'
#' @param cc_se A [`SumExp::SumExp`] object including the calibration curve samples
#' @returns A numeric vector of the concentrations of the calibration curve samples
#' @export
spiked_conc_pts <- function(cc_se) {
  stopifnot(inherits(cc_se, "SumExp"))
  SumExp::col_df(cc_se)[[spiked_conc_pts_name]]
}


#  -----  VARIABLES CREATED DURING PROCESSING  ---------------------------------

##  CALIBRATION --------------------------------------------

#' Get or save the ID of the source matrix used to get concentrations
#'
#' @param sumexp A [`SumExp::SumExp`] object
#' @param value The name of a matrix
#' @returns
#' [src_mat_id_for_conc()]: The matrix ID of the source matrix used to get concentrations
#' [save_src_mat_id_for_conc()]: The updated [`SumExp::SumExp`] object
#' @export
src_mat_id_for_conc <- function(sumexp) {
  stopifnot(inherits(sumexp, "SumExp"))
  SumExp::metadata(sumexp)$src_mat_id_for_conc
}
#' @rdname src_mat_id_for_conc
#' @export
save_src_mat_id_for_conc <- function(sumexp, value) {  # `src_mat_id_for_conc<-` doesn't work in `box`
  stopifnot(inherits(sumexp, "SumExp"), is.character(value), length(value) == 1)
  SumExp::metadata(sumexp)$src_mat_id_for_conc <- value
  sumexp
}

# #' Get or set the calibration curve model for a given matrix
# #'
# #' @param sumexp A [`SumExp::SumExp`] object
# #' @param mat_id The name of a matrix
# #' @param value The calibration curve model to be stored
# #' @returns
# #' [calcurve_model()]: The calibration curve model for the given matrix
# #' [calcurve_model<-()]: The updated [`SumExp::SumExp`] object
# #' @export
# calcurve_model <- function(sumexp, mat_id) {
#   stopifnot(inherits(sumexp, "SumExp"))
#   stopifnot(is.character(mat_id), length(mat_id) == 1)
#   SumExp::row_df(sumexp)[[paste0(".calcurve_model_", mat_id)]]
# }
# #' @rdname calcurve_model
# #' @export
# `calcurve_model<-` <- function(sumexp, mat_id, value) {
#   stopifnot(inherits(sumexp, "SumExp"))
#   stopifnot(is.character(mat_id), length(mat_id) == 1)
#   nm <- paste0(".calcurve_model_", mat_id)
#   stopifnot(is.list(value))
#   i_match <- match(names(value), rownames(sumexp))
#   if (any(is.na(i_match))) {
#     stop("Some feature IDs in 'value' are not found in 'sumexp'.")
#   }
#   SumExp::row_df(sumexp)[[nm]] <- NA # Allow missing row names in 'value'
#   SumExp::row_df(sumexp)[[nm]][i_match] <- value
#   sumexp
# }

##  MATRIX ID  ---------------------------------------------

#' Get the matrix ID of the blank-subtracted matrix
#'
#' @param mat_id The name of a matrix
#'
#' @returns The name of the blank-subtracted matrix
#' @export
mat_id_of_blank_subtracted <- function(mat_id) {
  paste0(mat_id, "_blk")
}
#' Get the matrix ID before blank subtraction
#' @rdname mat_id_of_blank_subtracted
#' @export
mat_id_before_blank_subtraction <- function(mat_id) {
  sub("_blk$", "", mat_id)
}

#' Get the matrix ID for calibration
#'
#' @param mat_id The name of a matrix
#' @returns The name of the matrix to be used for calibration
#' @export
mat_id_in_calibration <- function(mat_id) {
  paste0(mat_id, "_calib")
}
#' Get the matrix ID before calibration
#' @rdname mat_id_in_calibration
#' @export
mat_id_before_calibration <- function(mat_id) {
  sub("_calib$", "", mat_id)
}


#  -----  UTILS  ---------------------------------------------------------------

##  REPLACE INSTEAD OF DELETE  -----------------------------

#' Extract a subset and replace others with NA
#'
#' @param mat A matrix
#' @param i A numeric vector of row indices to be retained or a logical vector or `TRUE`
#' @param j A numeric vector of column indices to be retained or a logical vector or `TRUE`
#' @returns The matrix with the specified rows and columns retained and others replaced with NA
#' @export
extract_with_na <- function(mat, i = TRUE, j = TRUE) {
  stopifnot(is.matrix(mat))
  stopifnot(is.logical(i) || is.numeric(i))
  stopifnot(is.logical(j) || is.numeric(j))
  if (is.numeric(i)) {
    i_all <- seq_len(nrow(mat))
    i <- i_all %in% i
  } else {
    stopifnot(length(i) == nrow(mat) || length(i) == 1)
  }
  i[is.na(i)] <- FALSE
  if (is.numeric(j)) {
    j_all <- seq_len(ncol(mat))
    j <- j_all %in% j
  } else {
    stopifnot(length(j) == ncol(mat) || length(j) == 1)
  }
  j[is.na(j)] <- FALSE
  mat_out <- matrix(NA, nrow = nrow(mat), ncol = ncol(mat))
  mat_out[i, j] <- mat[i, j]
  dimnames(mat_out) <- dimnames(mat)
  labelled::label_attribute(mat_out) <- labelled::label_attribute(mat)
  mat_out
}

##  COMPUTE  -----------------------------------------------

#' Compute the average plus the standard deviation multiplied by `times`
#'
#' @param v A numeric vector
#' @param times Multiplication factor to the standard deviation.
#' @param na.rm A logical value indicating whether to remove NA values
#'
#' @returns A numeric vector of the average plus the standard deviation multiplied by `times`
#' @export
avg_plus_std_times <- function(v, times, na.rm = TRUE) { # nolint
  s <- stats::sd(v, na.rm = na.rm)
  m <- mean(v, na.rm = na.rm)
  s * times + m
}

#' Calculate RSD%
#'
#' @param x A numeric vector
#' @param na.rm A logical value indicating whether to remove NA values
#' @returns A numeric value of RSD%
.rsd_perc <- function(x, na.rm = FALSE) { # nolint
  100 * stats::sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm)
}
