# ------------------------------------------------------------------------------------------- #
# Functions to show in reports
# ------------------------------------------------------------------------------------------- #
box::use(util = ./msdial_utils)

#  -----  UTILS  ---------------------------------------------------------------

#' Check if
.chq_if_a_single_char <- function(x) {
  x_chr <- deparse1(substitute(x))
  if (is.character(x) && length(x) == 1) {
    return(TRUE)
  }
  stop(paste("`", x_chr, "` should be a single character", sep = ""))
}

#' Extract the label attribute of a variable if it has
#'
#' @param x An object that may have a label attribute
#' @param default A default label if the object does not have a label attribute
#'
#' @returns A character of the label attribute or the default label
#' @export
label_if_has <- function(x, default = deparse1(substitute(x))) {
  lab <- labelled::get_label_attribute(x)
  if (!is.null(lab)) {
    return(lab)
  }
  default
}

#' Calculate RSD%
#'
#' @param x A numeric vector
#' @param na.rm A logical value indicating whether to remove NA values
#' @returns A numeric value of RSD%
#' @export
rsd_perc <- function(x, na.rm = FALSE) {
  100 * stats::sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm)
}

#' RSD% across all samples
#'
#' @param mat A numeric matrix
#' @returns A numeric vector of RSD% across the samples in columns
#' @export
compute_rsd_per_feature <- function(mat) {
  apply(mat, 1, rsd_perc) |>
    labelled::set_label_attribute("RSD%")
}

#' Make a frequency `kable` table by factor level
#'
#' @param x A factor or character variable
#' @param what What to be counted
#' @param exclude Levels to be excluded
#' @param lab The label of the variable
#' @param ... Arguments passed to `janitor::tabyl`
#'
#' @returns A `kable` table with the number of samples and percentage of each level
#' @export
kable_number_of <- function(x, what = "Samples", exclude = NULL, lab, ...) {
  if (missing(lab)) {
    lab <- label_if_has(x, default = deparse1(substitute(x)))
  } else {
    .chq_if_a_single_char(lab)
  }
  tb <- janitor::tabyl(x, ...) |>
    janitor::adorn_pct_formatting() |>
    dplyr::filter(!x %in% exclude)
  knitr::kable(
    tb,
    row.names = FALSE,          # To avoid excluded row numbers
    col.names = c(lab, paste("Number of", what), "Percent"),
    align = "lrr"
  ) |>
    kableExtra::kable_styling(full_width = FALSE)
}

#' Get the colors of classes
#'
#' @param sumexp A [`SumExp::SumExp`] object
#' @param color_cat A named character vector of colors for each control category
#'
#' @returns A named character vector of colors for each class
#' @md
#' @export
get_colors_of_classes <- function(sumexp, color_cat) {
  df1 <- SumExp::col_df(sumexp)
  by_cat <- split(df1, util$ctrl_smpl_cat(sumexp)) |>
    lapply(\(.x) unique(.x$Class))
  # Fix the colors of the classes given in `color_cat`
  color_given <- by_cat[names(by_cat) %in% names(color_cat)] |>
    purrr::imap(~ stats::setNames(rep(color_cat[[.y]], length(.x)), .x)) |> # Class = "color"
    purrr::flatten()
  # Remaining classes
  rest <- unlist(by_cat[! names(by_cat) %in% names(color_cat)])
  n <- length(rest)
  # Use ggplot2 default colors excluding the first red, which is similar to 'red'
  ggcolor <- grDevices::hcl(seq(15, 375, length.out = n + 2), 100, 65)[2:(n + 1)]
  names(ggcolor) <- rest
  # A named character vector (`purrr::flatten()` gives a list, which ggplot2's manual scales
  # do not accept as colors)
  unlist(c(color_given, ggcolor))
}

#  -----  QC SAMPLES  ----------------------------------------------------------

#' Extract QC samples into list
#'
#' @param se A [`SumExp::SumExp`] object
#' @param min_n The minimum number of samples of a QC class to be included. RSD% needs >= 2.
#'
#' @returns A list of SumExp objects divided by QC Classes. An empty list without QC samples.
#' @md
#' @export
extract_qc_samples_to_list <- function(se, min_n = 1) {
  se <- util$extract_ctrl_smpl_cat(se, "QC")
  qc_id <- SumExp::col_df(se)$Class
  qc_ids <- names(which(table(qc_id) >= min_n))
  # Column-wise split
  lapply(stats::setNames(nm = intersect(unique(qc_id), qc_ids)), \(ii) se[, qc_id == ii])
}

#' Whether there are enough QC samples to evaluate RSD%
#'
#' @param se A [`SumExp::SumExp`] object
#' @returns `TRUE` if at least one QC class has two or more samples
#' @export
has_enough_qc <- function(se) {
  length(extract_qc_samples_to_list(se, min_n = 2)) > 0
}

#' Message shown instead of QC-based figures and tables
#' @export
not_enough_qc_msg <- "Not enough QCs to evaluate (at least two QC samples of the same class are needed)."

#' Extract quantitative standards in QC samples
#'
#' @param sumexp A [`SumExp::SumExp`] object
#' @returns A list of SumExp objects with quantitative standards in QC samples. Only QC classes
#'   with two or more samples, as RSD% cannot be computed otherwise.
#' @md
#' @export
extract_quant_qc <- function(sumexp) {
  se_lst <- extract_qc_samples_to_list(sumexp, min_n = 2)
  lapply(se_lst, \(se) {
    se[util$is_targeted_feature(se), ]
  })
}

#' Calculate RSD of the quantification standard samples
#'
#' @param qc_se_lst A list of [`SumExp::SumExp`] objects with quantitative standards in QC samples
#' @param mat_ids A character vector of the matrix IDs
#' @returns A tibble with RSD% of the quantification standard samples. The first two columns are
#'   `QC` and `feature_id`.
#' @md
#' @export
calc_rsd_qstd <- function(qc_se_lst, mat_ids) {
  out <- lapply(qc_se_lst, \(qc_se) {
    sapply(stats::setNames(nm = mat_ids), function(nm) {
      apply(qc_se[[nm]], 1, rsd_perc)
    }) |>    # Rows are features, columns are matrix IDs
      tibble::as_tibble(rownames = "feature_id")
  }) |>
    # `QC` has the name of the QC samples
    dplyr::bind_rows(.id = "QC")
  for (ii in mat_ids) {
    out[[ii]] <- labelled::copy_labels(qc_se_lst[[1]][[ii]], out[[ii]])
  }
  out  
}

#' Colors of the `sign` levels from [mark_lowest_rsd_norm()]
#' @export
norm_sign_colors <- c("#00BFC4", "#F8766D", "#7CAE00", "#7f7f7f") |>
  stats::setNames(c(paste(intToUtf8(0x2193), c("LOESS", "Closest", "Multivariate")), "All 0s"))

#' Mark the normalization method with the lowest RSD% per feature
#'
#' @param rsd_df A data frame from [calc_rsd_qstd()]
#' @param labs A named character vector: names are labels, values are matrix IDs (columns of
#'   `rsd_df`). `raw` is not a normalization method and is never marked.
#' @returns `rsd_df` with a factor column `sign`, e.g. "↓ LOESS", or "All 0s" when no method has
#'   a valid RSD%
#' @md
#' @export
mark_lowest_rsd_norm <- function(rsd_df, labs) {
  norm_labs <- labs[labs != "raw"]
  levs <- c(paste(intToUtf8(0x2193), names(norm_labs)), "All 0s")   # Down arrow
  m <- as.matrix(rsd_df[unname(norm_labs)])
  m[!is.finite(m)] <- Inf
  i_min <- max.col(-m, ties.method = "first")
  sign <- ifelse(rowSums(is.finite(m)) == 0, "All 0s", levs[i_min])
  rsd_df$sign <- factor(sign, levels = levs)
  rsd_df
}

#' Plot the RSD% of various data sets of features
#'
#' @param rsd_df A data frame with columns `feature_id` as well as all in `mat_ids`
#' @param mat_ids A character vector of assay IDs to be plotted in the x-axis
#' @returns A ggplot object
#' @export
ggplot_rsdp_metab <- function(rsd_df, mat_ids) {
  nms <- names(mat_ids)
  if (is.null(nms)) {
    nms <- labelled::get_variable_labels(rsd_df[mat_ids])
  }
  ids <- unname(mat_ids)    # Duplicated by `dplyr::all_of`
  rsd_df <- rsd_df |>
    dplyr::mutate(
      # NA was generated when all values were zero
      dplyr::across(
        dplyr::all_of(ids),
        \(.x) ifelse(is.na(.x), 0, .x)
      ),
    ) |>
    tidyr::pivot_longer(
      dplyr::all_of(ids),
      names_to = "Data",
      values_to = "RSD%"
    ) |>
    dplyr::mutate(Data = factor(Data, levels = ids, labels = nms))
  ggplot2::ggplot(rsd_df) +
    ggplot2::aes(x = Data, y = `RSD%`) +
    ggplot2::geom_line(ggplot2::aes(group = feature_id), alpha = 0.5)
}

#' Axis labels in scientific notation, as the interactive plots (plotly's `exponentformat: "E"`):
#' e.g. "1.5E+6", "5E-2"; zero is "0"
#'
#' @param x Numeric axis breaks
#' @returns A character vector
#' @export
label_e <- function(x) {
  out <- vapply(x, \(v) format(v, scientific = TRUE, trim = TRUE, drop0trailing = TRUE), "")
  out <- toupper(sub("e([+-])0*([0-9])", "e\\1\\2", out))
  out[!is.na(x) & x == 0] <- "0"
  out[is.na(x)] <- NA_character_
  out
}

#  -----  CALIBRATION CURVE  ---------------------------------------------------

#' Calibration curve line
#'
#' @param lim_df A data frame with columns `lloq`, `max_c_conc`, and `calcurve_model`, and
#'   optionally `min_c_conc` (the start of the line for curves without an LLOQ, which were kept
#'   although not valid)
#' @param log_scale A logical value indicating whether the calibration curve is fitted in log scale
#' @param color,alpha,... Arguments passed to `ggplot2::geom_line`
#' @returns A [`ggplot2::geom_line`] object.
#' @md
#' @export
geom_calibration_curve_line <- function(lim_df, log_scale = FALSE, color = "cadetblue", alpha = 0.7, ...) {
  # Artificial concentration values for smooth calibration curve line
  ccline_df <- purrr::pmap(lim_df, \(lloq, max_c_conc, calcurve_model, min_c_conc = NA, ...) {
    from <- if (is.finite(lloq)) lloq else min_c_conc
    if (!is.list(calcurve_model) || !is.finite(from) || !is.finite(max_c_conc)) return(NULL)
    conc <- if (log_scale) {
      # Equally spaced concentration values in log scale
      seq(log(from), log(max_c_conc), length.out = 200)
    } else {
      # Concentration values in linear scale
      sort(c(
        seq(from, max_c_conc, length.out = 100),
        seq(from, min(from * 10, max_c_conc), length.out = 100)[-c(1, 100)]   # Zoom-in
      ))
    }
    
    .y_value <- stats::predict.lm(calcurve_model$inv_model, data.frame(conc = conc))
    if (log_scale) {
      .y_value <- expm1(.y_value)    # Return to original scale
      conc <- exp(conc)    # Return to original scale
    }
    tibble::tibble(conc = conc, .y_value = .y_value, ...)
  }) |>
    dplyr::bind_rows()
  # No curve to draw: an empty layer, since `CoordZoomInByCC` finds the other layers by position
  if (nrow(ccline_df) == 0) {
    ccline_df <- tibble::tibble(conc = numeric(), .y_value = numeric(), feature_id = character(),
                                feature_name = character())
  }
  ccline_df <- dplyr::arrange(ccline_df, feature_id, conc)      # To make sure

  ggplot2::geom_line(
    ggplot2::aes(x = conc, y = .y_value), data = ccline_df,
    color = color,
    alpha = alpha,
    ...
  )
}

#' Plot the calibration curve with the samples
#'
#' @description
#' Plot the calibration curve for each feature together with the samples. The X-axis is the
#' concentration and the Y-axis is the peak area of the intensity.
#'
#' @param x_se A [`SumExp::SumExp`] object having the data of the samples to be plotted
#' @param cc_se A [`SumExp::SumExp`] object of the calibration curve samples. The features in
#'   `x_se` should be a subset of `cc_se`.
#' @param mat_id A character of the assay ID to be plotted in the y-axis
#' @param colors_of_classes A named character vector of colors for each class
#' @param log_scale A logical value indicating whether the calibration curve has been fitted in log
#'   scale
#'
#' @returns A ggplot object.
#' @md
#' @export
ggplot_calcurve_samples <- function(x_se, cc_se, mat_id, colors_of_classes, log_scale = FALSE) {
  .chq_if_a_single_char(mat_id)
  feature_ids <- rownames(x_se)           # Limited to those features in the `x_se`
  cc_se <- cc_se[feature_ids, ]
  cc_df <- SumExp::as_tibble(cc_se) |>
    # Drop concentrations outside the range
    tidyr::drop_na(tidyselect::all_of(unname(mat_id))) |>
    dplyr::mutate(
      txt = paste(
        "batch: ", batch_id, "\n",
        "inj: ", injection_order, ", conc: ", c_conc, "\n",
        "name: ", sample_name, "\n",
        "int: ", round(.data[[mat_id]], 0)
      )
    )   # Label for the points
  # LLOQ, LOD, max_c_conc of each `feature_id`
  lim_df <- SumExp::row_df(x_se) |>
    # `feature_name` is added for further usage, e.g. `ggplot2::facet_wrap(~ feature_name)`
    dplyr::select(feature_name, lloq, lod, min_c_conc, max_c_conc, calcurve_model) |>
    tibble::rownames_to_column("feature_id")
  # The samples to show with the calibration curve
  x_df <- SumExp::as_tibble(x_se) |>
    tidyr::drop_na(conc) |>
    dplyr::mutate(
      txt = paste(
        "batch: ", batch_id, "\n",
        "inj: ", injection_order, ", conc: ", conc, "\n",
        "name: ", sample_name, "\n",
        "int: ", round(.data[[mat_id]], 0)
      )
    )    # Label for the points
  
  out <- ggplot2::ggplot(x_df, ggplot2::aes(x = conc, y = .data[[mat_id]])) +
    geom_calibration_curve_line(lim_df, log_scale = log_scale) +
    # Shade the region below the LLOQ
    ggplot2::geom_rect(
      ggplot2::aes(xmin = 0, xmax = lloq, ymin = 0, ymax = Inf),
      data = lim_df,
      fill = "grey30",
      alpha = 0.5,
      inherit.aes = FALSE
    ) +
    # Shade the region below the LOD
    ggplot2::geom_rect(
      ggplot2::aes(xmin = 0, xmax = lod, ymin = 0, ymax = Inf),
      data = lim_df,
      fill = "grey25",
      alpha = 0.5,
      inherit.aes = FALSE
    )
  # Peak areas in scientific notation, as in the interactive plots
  out <- out + if (log_scale) {
    ggplot2::scale_x_log10() + ggplot2::scale_y_log10(labels = label_e)
  } else {
    ggplot2::scale_y_continuous(labels = label_e)
  }
  out <- suppressWarnings(     # Unknown aesthetics: text
    out +
      # Remaining calibration samples
      ggplot2::geom_point(ggplot2::aes(x = c_conc, text = txt), data = cc_df) +
      # Samples to measure
      ggplot2::geom_point(ggplot2::aes(color = Class, text = txt)) +
      ggplot2::guides(text = "none")
  )
  # Rug plot at the left and bottom edges of the plot to show the density of points
  out <- out +
    # Rug plot at the left and bottom edges of the plot to show the density of points
    ggplot2::geom_rug(color = grDevices::rgb(0.5, 0, 0, alpha = 0.2)) +
    ggplot2::labs(
      x = label_if_has(x_df$conc, "Concentration"),
      y = label_if_has(x_se[[mat_id]], mat_id),
    ) 
  if (missing(colors_of_classes)) {
    return(out)
  }
  out + ggplot2::scale_color_manual(values = colors_of_classes)
}

#' @inheritParams ggplot2::facet_wrap
#' @param ... parameters to be passed to `ggplot2::facet_wrap`
#' @export
#' @rdname ggplot_calcurve_samples
ggplot_calcurve_samples_facet <- function(x_se,
                                          cc_se,
                                          mat_id,
                                          colors_of_classes,
                                          scales = "free",
                                          ncol = 3,
                                          log_scale = FALSE,
                                          ...) {
  ggplot_calcurve_samples(x_se, cc_se, mat_id, colors_of_classes, log_scale) +
    ggplot2::facet_wrap(~ feature_name, scales = scales, ncol = ncol, ...)
}

#' ggproto for zooming in by the calibration curve
CoordZoomInByCC <- ggplot2::ggproto(
  "CoordZoomInByCC", ggplot2::CoordCartesian,
  # Data for calibration points. Matched with `ggplot_calcurve_samples`
  i_data = 5,   # The index of the data for calibration points
  n_cc = 4,   # Number of calibration points
  c_conc = "c_conc",       # Concentration column name
  mat_id = NULL,           # Matrix ID column name
  feat_id = ".row_id",     # Feature ID column name
  facet_by = "feature_name",    # Facet by feature name
 
  # Set updated limits by the calibration points
  setup_panel_params = function(self, scale_x, scale_y, params = list()) {
    # Iteratively run for each panel
    self$panel_counter <- self$panel_counter + 1
    df_cc <- self$df_cc
    if (nrow(self$layout) > 1) {   # Multiple panels
      f <- self$layout[[self$facet_by]][self$panel_counter]
      df_cc <- df_cc[df_cc[[self$facet_by]] == f, , drop = FALSE]
    }
    xlim <- c(0, max(df_cc[[self$c_conc]], na.rm = TRUE))
    ylim <- c(0, max(df_cc[[self$mat_id]], na.rm = TRUE))
    # The same format as CoordCartesian$setup_panel_params
    c(ggplot2:::view_scales_from_scale(scale_x, xlim, params$expand[c(4, 2)]),
      ggplot2:::view_scales_from_scale(scale_y, ylim, params$expand[c(3, 1)]),
      reverse = self$reverse %||% "none"
    )
  },
 
  # Filter out the data above the upper-bound
  setup_data = function(self, data, params) {
    df_cc <- data[[self$i_data]]    # Data for calibration points
    # Find upper-bound, which is the `n_cc`th lowest concentration
    df_cc_range <- df_cc[, c(self$feat_id, self$c_conc)]
    names(df_cc_range) <- c("feat_id", "c_conc")
    df_cc_range <- dplyr::distinct(df_cc_range) |>
      dplyr::summarise(.ub = c_conc[order(c_conc)][self$n_cc], .by = feat_id)
    # Filter out the concentration above the bound
    df_cc <- dplyr::right_join(df_cc_range, df_cc, by = c(feat_id = self$feat_id)) |>
      dplyr::filter(c_conc <= .ub) |>
      dplyr::select(-.ub)
    # Save the filtered data and make it accessible to `setup_panel_params`
    self$df_cc <- df_cc
    data[[self$i_data]] <- df_cc
    data
  },

  # Initialize the panel counter and save the layout for `setup_panel_params`
  setup_layout = function(self, layout, params) {
    self$panel_counter <- 0
    self$layout <- layout    # To be used in `setup_panel_params`
    # Copy from `Coord` in ggplot2
    scales <- layout[c("SCALE_X", "SCALE_Y")]
    layout$COORD <- vctrs::vec_match(scales, ggplot2:::unique0(scales))
    layout
  }
)

#' Create a ggproto object for zooming in by the calibration curve points
#'
#' @param n_cc Number of calibration points to be presented
#' @param mat_id Matrix ID column name
#' @inheritParams ggplot2::coord_cartesian
#' @export
coord_zoom_in_by_cc <- function(n_cc = 4,
                                mat_id = NULL,
                                expand = TRUE,
                                default = FALSE,
                                clip = "on") {
  ggplot2::ggproto(
    NULL, CoordZoomInByCC,
    n_cc = n_cc,
    mat_id = mat_id,
    expand = expand,
    default = default,
    clip = clip
  )
}

#' [ggplot2::geom_text()] for the best model of the calibration curve
#'
#' @param x_se A [`SumExp::SumExp`] object
#' @param hjust,vjust,size,color,... Parameters to be passed to `ggplot2::geom_text()`
#' @returns A [ggplot2::geom_text()] output
#' @md
#' @seealso [ggplot_calcurve_samples()]
#' @export
geom_best_model_text <- function(x_se, hjust = 1, vjust = 0, size = 4, color = "red3", ...) {
  show_df <- SumExp::row_df(x_se) |>
    # `feature_name` is added for further usage, e.g. `ggplot2::facet_wrap(~ feature_name)`
    dplyr::select(feature_name, calcurve_model) |>
    tibble::rownames_to_column("feature_id")
  # Text for the selected calibration curve models
  best_model_text_df <- show_df |>
    dplyr::mutate(
      best_model_name = purrr::map_chr(calcurve_model, \(m) m$best_model_name),
      R2 = purrr::map_dbl(calcurve_model, \(m) m$R2s[[m$best_model_name]]),
      txt = paste0(best_model_name, "\n R2=", sprintf("%.3f", R2)),
      .keep = "unused"
    )
  ggplot2::geom_text(         # Display the best model name
    ggplot2::aes(x = Inf, y = 0, label = txt),
    data = best_model_text_df,
    hjust = hjust,
    vjust = vjust,
    size = size,
    color = color,
    ...
  )
}

#  -----  INJECTION ORDER  -----------------------------------------------------

#' Create a data frame for the injection order plot
#'
#' @param sumexp A [`SumExp::SumExp`] object
#' @param inj_ord A `tidyselect` expression for the injection order in the
#'   `SumExp::col_df(sumexp)`
#' @param mat_ids Matrix IDs to be included in the plot
#' @returns A tibble with the data for the injection order plot. The columns include:
#'   - `Data`: The name of the data in the `sumexp`
#'   - `Value`: The value of the data
#'   - `sum_value`: The sum of the `Value` per injection and per `Data`. It is for smoothed line
#' @md
#' @export
df_for_injection_order <- function(sumexp, mat_ids = names(sumexp), inj_ord = injection_order) {
  labs <- SumExp::name_labs(sumexp)
  stopifnot(all(mat_ids %in% labs))
  labs <- labs[match(mat_ids, labs)]   # The same order as `mat_ids`

  SumExp::as_tibble(sumexp) |>
    tidyr::pivot_longer(cols = dplyr::all_of(mat_ids),
                        names_to = "Data",
                        values_to = "Value") |>
    # Decorate the names of the data in the `sumexp`
    dplyr::mutate(Data = factor(Data, levels = labs, labels = names(labs))) |>
    # For smoothed line across the injections
    dplyr::mutate(sum_value = sum(Value, na.rm = TRUE), .by = c({{ inj_ord }}, Data))
}

#' Create a column plot for the injection order
#'
#' @param data A tibble with the data for the injection order plot. It can be created by
#'   [df_for_injection_order()]
#' @param fill A `tidyselect` expression for the color aesthetic of the columns in the plot
#' @param inj_ord A `tidyselect` expression for the injection order
#'
#' @returns A [ggplot2::ggplot()] object
#' @seealso [df_for_injection_order()]
#' @md
#' @export
ggplot_col_injection_order <- function(data, fill, inj_ord = injection_order) {
  ggplot2::ggplot(data, ggplot2::aes(x = {{ inj_ord }})) +
    ggplot2::geom_col(ggplot2::aes(y = Value, fill = {{ fill }})) +
    ggplot2::labs(
      x = label_if_has(dplyr::pull(data, {{ inj_ord }}), "Injection order"),
      fill = label_if_has(dplyr::pull(data, {{ fill }}), deparse(substitute(fill)))
    ) +
    ggplot2::geom_smooth(ggplot2::aes(y = sum_value),
                         method = "loess", formula = "y ~ x", se = FALSE) +
    ggplot2::facet_wrap(~Data , scales = "free_y")
}

#  -----  PCA  -----------------------------------------------------------------

#' Create a PCA plot
#'
#' @param sumexp A [`SumExp::SumExp`] object
#' @param mat_id The matrix ID in the `sumexp` to be used for PCA
#' @param ... Additional arguments to [ggfortify::autoplot.prcomp()]
#'
#' @returns A [ggplot2::ggplot()] object
#' @md
#' @export
ggplot_pca <- function(sumexp, mat_id, ...) {
  mat <- sumexp[[mat_id]]
  lab <- labelled::get_label_attribute(mat)
  mat <- mat[rowSums(mat > 0) > 0, ]    # Remove all-zero rows
  pca_res <- tryCatch({
    stats::prcomp(t(mat), scale. = TRUE)
  }, error = function(e) {
    warning("PCA failed. The error message is: ", e$message)
    NULL
  })
  if (is.null(pca_res)) {
    return(NULL)
  }
  # `autoplot.prcomp` is exported via `S3method`, so it is not available in the namespace
  ggfortify:::autoplot.prcomp(pca_res, data = SumExp::col_df(sumexp), ...) +
    ggplot2::labs(title = lab)
}

#  -----  SHARED INTERACTIVE PLOTS (app and internal report)  ----------------------------------

#' Sample groups for plots: the class of study samples, otherwise the control category
#'
#' @param col_df The `col_df` of a [`SumExp::SumExp`] object (with `Class` and `.ctrl_cat`)
#' @returns A character vector: e.g. "Healthy", "QC", "Blank", "Calibrant"
#' @export
sample_group <- function(col_df) {
  ctrl <- as.character(col_df$.ctrl_cat)
  dplyr::case_when(ctrl == "" ~ as.character(col_df$Class), ctrl == "CalCurve" ~ "Calibrant", TRUE ~ ctrl)
}

#' Colours of the sample groups from [sample_group()]
#'
#' The app's colour scheme (as [get_colors_of_classes()]): blanks and QCs get the fixed colours of
#' `constants.yml`, the other groups (study classes, then calibrants) ggplot2's default hues
#' without the first red, which is similar to the blanks' red.
#'
#' @param groups The sample groups (any order, with duplicates)
#' @param fixed Fixed colours of control groups, as `COLORS$.ctrl_cat` in `constants.yml`
#' @returns A named character vector of colours
#' @export
sample_group_colors <- function(groups, fixed = c(Blank = "red", QC = "blue")) {
  rest <- c(sort(setdiff(unique(groups), c(names(fixed), "Calibrant"))),
            intersect("Calibrant", unique(groups)))
  n <- length(rest)
  hues <- grDevices::hcl(seq(15, 375, length.out = n + 2), 100, 65)[seq_len(n) + 1]
  c(stats::setNames(hues, rest), fixed)
}

#' ggplot2's default discrete colours, named (for batches and internal standards)
#'
#' @param x The names (levels)
#' @export
hue_colors <- function(x) stats::setNames(scales::hue_pal()(length(x)), x)

#' Interactive stacked bar plot of the internal standards per sample along the injection order,
#' with a LOESS trend of the total (the same as the internal report's "Internal standards over
#' the sequence" viewer)
#'
#' @param is_se A [`SumExp::SumExp`] object of the internal standards
#' @param mat_id The matrix to show, e.g. "raw" or "loess_norm"
#' @param colour_by "group" (sample group), "batch" or "is" (the contribution of each IS)
#' @param title The plot title
#' @param height Height of the plot in pixels
#' @returns A plotly object
#' @export
plotly_is_bars <- function(is_se, mat_id, colour_by = c("group", "batch", "is"), title = mat_id,
                           height = 480) {
  colour_by <- match.arg(colour_by)
  cd <- SumExp::col_df(is_se)
  v <- is_se[[mat_id]]
  v[is.na(v)] <- 0
  io <- cd$injection_order
  tot <- colSums(v)
  who <- paste0("<b>", cd$sample_name, "</b><br>", cd$Class, " · batch ", cd$batch_id, " · inj. ", io,
                "<br>total ", signif(tot, 4))
  p <- plotly::plot_ly(height = height)
  if (colour_by == "is") {
    nm <- SumExp::row_df(is_se)$feature_name
    col <- hue_colors(nm)
    for (i in seq_along(nm)) {
      p <- plotly::add_bars(p, x = io, y = v[i, ], name = nm[i], marker = list(color = col[[i]]),
                            text = paste0(who, "<br><b>", nm[i], "</b> ", signif(v[i, ], 4)),
                            textposition = "none", hoverinfo = "text")
    }
  } else {
    key <- if (colour_by == "batch") paste("Batch", cd$batch_id) else sample_group(cd)
    col <- if (colour_by == "batch") hue_colors(sort(unique(key))) else sample_group_colors(key)
    for (g in names(col)[names(col) %in% key]) {
      k <- key == g
      p <- plotly::add_bars(p, x = io[k], y = tot[k], name = g, marker = list(color = col[[g]]), text = who[k],
                            textposition = "none", hoverinfo = "text")
    }
  }
  fit <- tryCatch(stats::loess(tot ~ io, span = 0.3), error = function(e) NULL)
  if (!is.null(fit)) {
    o <- order(io)
    p <- plotly::add_lines(p, x = io[o], y = stats::predict(fit)[o], name = "Trend (LOESS)",
                           line = list(color = "#2F5BEA", width = 3), hoverinfo = "skip")
  }
  plotly::layout(p, title = list(text = title, x = 0.02), barmode = "stack", bargap = 0,
                 hovermode = "closest", font = list(family = "Lato"),
                 xaxis = list(title = "Injection order"),
                 yaxis = list(title = "Summed IS peak area", exponentformat = "E"),
                 legend = list(orientation = "v", x = 1.02, y = 1),
                 margin = list(t = 50, r = 20, b = 60, l = 80))
}

#' Interactive bar plot of a volumetric internal standard per sample along the injection order
#'
#' @param y The raw peak areas (one per sample)
#' @param smp A data frame with `sample_name`, `Class`, `batch_id`, `injection_order` and `group`
#' @param name The name of the volumetric internal standard
#' @param colour_by `"group"` (sample group, i.e. the type of injection) or `"batch"`
#' @param switchable Whether the plot gets its own Colour buttons (Sample group / Batch), e.g. in
#'   the report; then `colour_by` is the colouring shown first
#' @param height Height of the plot in pixels
#' @returns A plotly object
#' @export
plotly_vis_bars <- function(y, smp, name, colour_by = c("group", "batch"), switchable = FALSE,
                            height = 420) {
  colour_by <- match.arg(colour_by)
  txt <- paste0("<b>", smp$sample_name, "</b><br>", smp$Class, " · batch ", smp$batch_id,
                " · inj. ", smp$injection_order, "<br>peak area ", signif(y, 4))
  # Colours and groups of both colourings (as in the plots of the internal standards)
  batch <- paste("Batch", smp$batch_id)
  sets <- list(
    group = list(key = smp$group, col = sample_group_colors(smp$group)),
    batch = list(key = batch,
                 col = hue_colors(paste("Batch", stringr::str_sort(unique(as.character(smp$batch_id)), numeric = TRUE))))
  )
  shown <- if (switchable) names(sets) else colour_by
  p <- plotly::plot_ly(height = height)
  n_traces <- c(group = 0L, batch = 0L)
  for (s in shown) {
    col <- sets[[s]]$col
    for (g in names(col)[names(col) %in% sets[[s]]$key]) {
      k <- sets[[s]]$key == g
      p <- plotly::add_bars(p, x = smp$injection_order[k], y = y[k], name = g, text = txt[k],
                            textposition = "none", hoverinfo = "text",
                            marker = list(color = col[[g]]), visible = s == colour_by)
      n_traces[[s]] <- n_traces[[s]] + 1L
    }
  }
  menus <- if (switchable) {
    vis <- function(s) rep(names(n_traces), n_traces) == s
    list(list(type = "buttons", direction = "right", x = 1, y = 1.02, xanchor = "right",
              yanchor = "bottom", active = match(colour_by, names(n_traces)) - 1L,
              buttons = list(
                list(label = "Sample group", method = "restyle", args = list("visible", as.list(vis("group")))),
                list(label = "Batch", method = "restyle", args = list("visible", as.list(vis("batch"))))
              )))
  }
  plotly::layout(p, title = list(text = name, x = 0.02), barmode = "stack",
                 bargap = 0, hovermode = "closest", font = list(family = "Lato"),
                 xaxis = list(title = "Injection order"),
                 yaxis = list(title = paste("Peak area,", name), exponentformat = "E"),
                 legend = list(orientation = "v", x = 1.02, y = 1), updatemenus = menus,
                 margin = list(t = if (switchable) 70 else 50))
}

#' Colours of the concentrations replaced by the LOD/LLOQ rules (as the cells of the exported
#' tables: red, yellow and blue), and of the measured ones
#' @export
imputed_colors <- c("Measured" = "grey25", "< LOD: LLOQ/4" = "#E05555",
                    "< LLOQ: LLOQ/2" = "#E8B730", "Set to LLOQ" = "#4A9FD8")

#' Whether each concentration is measured or replaced by an LOD/LLOQ rule
#'
#' @param conc Concentrations
#' @param lloq The LLOQ of the chemical of each concentration
#' @returns A factor with the levels `names(imputed_colors)`, `NA` for missing concentrations
#' @export
imputed_kind <- function(conc, lloq) {
  kind <- dplyr::case_when(
    is.na(conc) ~ NA_character_,
    !is.na(lloq) & conc == lloq / 4 ~ "< LOD: LLOQ/4",
    !is.na(lloq) & conc == lloq / 2 ~ "< LLOQ: LLOQ/2",
    !is.na(lloq) & conc == lloq ~ "Set to LLOQ",
    TRUE ~ "Measured"
  )
  factor(kind, levels = names(imputed_colors))
}

#' Interactive plot of one feature in the blanks compared to the other sample groups (QC/QA)
#'
#' Two views: `"groups"`, a bar per sample group (mean, with the SD as error bar) and the samples
#' as points; `"samples"`, a bar per sample along the injection order. Both show the threshold
#' of the blank filter, `factor` times the mean of the blanks, as a dashed line.
#'
#' @param y The values of the feature (one per sample)
#' @param smp A data frame with `sample_name`, `Class`, `batch_id`, `injection_order` and `group`
#'   ([sample_group()]), one row per value
#' @param name The name of the feature (title)
#' @param factor The blank filtering factor
#' @param view `"groups"` or `"samples"`
#' @param subtitle A line under the title, e.g. whether the blank filter removed the feature
#' @param y_title The title of the y-axis
#' @param height Height of the plot in pixels
#' @returns A plotly object
#' @export
plotly_blank_comparison <- function(y, smp, name, factor, view = c("groups", "samples"),
                                    subtitle = "", y_title = "Peak area", height = 480) {
  view <- match.arg(view)
  col <- sample_group_colors(smp$group)
  # Blanks first, then QCs, the study classes and calibrants
  levs <- c(intersect(c("Blank", "QC"), smp$group),
            setdiff(names(col), c("Blank", "QC", "Calibrant")) |> intersect(smp$group),
            intersect("Calibrant", smp$group))
  threshold <- factor * mean(y[smp$group == "Blank"], na.rm = TRUE)
  txt <- paste0("<b>", smp$sample_name, "</b><br>", smp$Class, " · batch ", smp$batch_id,
                " · inj. ", smp$injection_order, "<br>", signif(y, 4))
  p <- plotly::plot_ly(height = height)
  if (view == "groups") {
    for (i in seq_along(levs)) {
      k <- smp$group == levs[i] & !is.na(y)
      p <- plotly::add_bars(p, x = i, y = mean(y[k]), name = levs[i], width = 0.7,
                            error_y = list(type = "data", array = stats::sd(y[k]), color = "grey30"),
                            marker = list(color = grDevices::adjustcolor(col[[levs[i]]], 0.55),
                                          line = list(color = col[[levs[i]]], width = 1.5)),
                            hovertemplate = paste0("<b>", levs[i], "</b><br>mean %{y:.4g} (n = ", sum(k),
                                                   ")<extra></extra>"),
                            legendgroup = levs[i])
      p <- plotly::add_markers(p, x = i + stats::runif(sum(k), -0.25, 0.25), y = y[k], name = levs[i],
                               text = txt[k], hoverinfo = "text",
                               marker = list(color = col[[levs[i]]], size = 6, opacity = 0.8,
                                             line = list(color = "white", width = 0.5)),
                               legendgroup = levs[i], showlegend = FALSE)
    }
    xaxis <- list(title = "", tickvals = seq_along(levs), ticktext = levs, zeroline = FALSE,
                  range = c(0.4, length(levs) + 0.6))
  } else {
    for (g in levs) {
      k <- smp$group == g
      # Width of one injection (each group's bars would otherwise get their own width)
      p <- plotly::add_bars(p, x = smp$injection_order[k], y = y[k], name = g, text = txt[k],
                            textposition = "none", hoverinfo = "text",
                            marker = list(color = col[[g]]), width = 0.9)
    }
    xaxis <- list(title = "Injection order")
  }
  # Threshold of the blank filter as a dashed line (a trace, so that it follows the log scale)
  if (is.finite(threshold)) {
    xs <- if (view == "groups") c(0.4, length(levs) + 0.6) else range(smp$injection_order) + c(-1, 1)
    p <- plotly::add_lines(p, x = xs, y = c(threshold, threshold),
                           name = paste0("Blank filter threshold (", factor, " × blank mean)"),
                           line = list(color = "#c23636", dash = "dash", width = 2),
                           hovertemplate = paste0("threshold ", signif(threshold, 4), "<extra></extra>"))
  }
  notes <- list(list(text = subtitle, x = 0, y = 1.02, xref = "paper", yref = "paper",
                     xanchor = "left", yanchor = "bottom", showarrow = FALSE, font = list(size = 13)))
  axis_buttons <- list(
    list(label = "Linear", method = "relayout", args = list(list(yaxis.type = "linear"))),
    list(label = "Log", method = "relayout", args = list(list(yaxis.type = "log")))
  )
  plotly::layout(
    p, title = list(text = name, x = 0.02), barmode = "overlay", bargap = if (view == "samples") 0,
    hovermode = "closest", font = list(family = "Lato"), xaxis = xaxis,
    yaxis = list(title = y_title, exponentformat = "E"), annotations = notes,
    legend = list(orientation = "v", x = 1.02, y = 1), margin = list(t = 80),
    updatemenus = list(list(type = "buttons", direction = "right", x = 1, y = 1.1,
                            xanchor = "right", yanchor = "bottom", buttons = axis_buttons))
  )
}

#' Interactive plot of the concentrations of one chemical per class: a box per class and the
#' samples as points, coloured by measured or replaced value ([imputed_kind()])
#'
#' @param conc,lloq The concentrations and the LLOQ (of the batch) of each sample
#' @param cats The class (x-axis category) of each sample
#' @param cat_colors Named colours of the classes (boxes)
#' @param hover The hover text of each sample (HTML)
#' @param name The name of the chemical (title)
#' @param y_title The title of the y-axis
#' @param log Whether the y-axis starts in log scale (a button switches it)
#' @param height Height of the plot in pixels
#' @returns A plotly object
#' @export
plotly_conc_by_class <- function(conc, lloq, cats, cat_colors, hover, name, y_title, log = FALSE,
                                 height = 460) {
  ok <- !is.na(conc)
  levs <- intersect(names(cat_colors), unique(cats[ok]))
  levs <- c(levs, sort(setdiff(unique(cats[ok]), levs)))
  pos <- match(cats, levs)
  kind <- imputed_kind(conc, lloq)
  p <- plotly::plot_ly(height = height)
  for (i in seq_along(levs)) {
    k <- ok & pos == i
    col <- if (levs[i] %in% names(cat_colors)) cat_colors[[levs[i]]] else "grey60"
    p <- plotly::add_trace(p, type = "box", x = rep(i, sum(k)), y = conc[k], name = levs[i],
                           boxpoints = FALSE, fillcolor = grDevices::adjustcolor(col, 0.45),
                           line = list(color = "grey30", width = 1), width = 0.6,
                           hoverinfo = "skip", showlegend = FALSE)
  }
  jitter <- stats::runif(length(conc), -0.22, 0.22)
  for (kd in levels(kind)) {
    k <- ok & kind %in% kd
    if (!any(k)) next
    p <- plotly::add_markers(p, x = pos[k] + jitter[k], y = conc[k], name = kd, text = hover[k],
                             hoverinfo = "text",
                             marker = list(color = imputed_colors[[kd]], size = 7, opacity = 0.85,
                                           line = list(color = "white", width = 0.5)))
  }
  axis_buttons <- list(
    list(label = "Linear", method = "relayout", args = list(list(yaxis.type = "linear"))),
    list(label = "Log", method = "relayout", args = list(list(yaxis.type = "log")))
  )
  plotly::layout(
    p, title = list(text = name, x = 0.02), hovermode = "closest", font = list(family = "Lato"),
    xaxis = list(title = "", tickvals = seq_along(levs), ticktext = levs, tickangle = -45,
                 range = c(0.4, length(levs) + 0.6), zeroline = FALSE),
    yaxis = list(title = y_title, type = if (log) "log" else "linear", exponentformat = "E"),
    legend = list(orientation = "v", x = 1.02, y = 1, title = list(text = "Value")),
    margin = list(t = 70),
    updatemenus = list(list(type = "buttons", direction = "right", x = 1, y = 1.02,
                            xanchor = "right", yanchor = "bottom", active = if (log) 1 else 0,
                            buttons = axis_buttons))
  )
}

#' Why a calibration curve is not valid, or was kept or replaced, as one sentence per chemical
#'
#' @param row_df The `row_df` of calibrated data (`has_proper_range`, `src_calcurve`, and from
#'   processing with this version also `range_issue` and `kept_calcurve`)
#' @param batch_view Whether the rows are those of a batch (`src_calcurve` "Global" then means
#'   that the batch's own curve was replaced by the global one)
#' @returns A character vector, `""` for a valid curve used as is
#' @export
calib_issue_text <- function(row_df, batch_view = TRUE) {
  n <- nrow(row_df)
  issue <- if (is.null(row_df$range_issue)) rep(NA_character_, n) else as.character(row_df$range_issue)
  kept <- if (is.null(row_df$kept_calcurve)) rep(FALSE, n) else row_df$kept_calcurve %in% TRUE
  src <- if (is.null(row_df$src_calcurve)) rep(NA_character_, n) else row_df$src_calcurve
  valid <- row_df$has_proper_range %in% TRUE
  dplyr::case_when(
    !valid ~ paste0("Not valid: ", dplyr::coalesce(issue, "no valid calibration range")),
    kept ~ paste0("Kept although not valid: ", dplyr::coalesce(issue, "no valid calibration range")),
    batch_view & src %in% "Global" & !is.na(issue) ~
      paste0("Batch curve not valid (", issue, "): the global curve is used"),
    batch_view & src %in% "Global" ~ "The global curve is used",
    TRUE ~ ""
  )
}

#' A note for calibration curves without any quantifiable sample, one per chemical
#'
#' @param row_df The `row_df` of calibrated data (`no_quant_samples`, from processing with this
#'   version)
#' @returns A character vector, `""` when there are quantifiable samples
#' @export
calib_range_note <- function(row_df) {
  nq <- if (is.null(row_df$no_quant_samples)) rep(FALSE, nrow(row_df)) else row_df$no_quant_samples %in% TRUE
  ifelse(nq & row_df$has_proper_range %in% TRUE,
         "No sample or QC at or above the LLOQ: the full calibration range is shown", "")
}

#' RSD% of the volumetric internal standards (vIS) within groups of samples
#'
#' The groups are all samples, each sample type, each class of the study samples, each QC group
#' and each batch; groups of fewer than two samples are left out.
#'
#' @param mat The raw peak areas of the vIS (vIS in rows, samples in columns)
#' @param names The names of the vIS
#' @param smp The `col_df` of the samples (`sample_type`, `Class`, `.ctrl_cat`, `batch_id`), one
#'   row per column of `mat`
#' @returns A tibble with the columns `vIS`, `Grouping`, `Group`, `n` and `RSD%` (1 decimal)
#' @export
vis_rsd_table <- function(mat, names, smp) {
  cat <- smp$.ctrl_cat
  groupings <- list(
    list("All samples", rep("All", nrow(smp)), TRUE),
    list("Sample type", as.character(smp$sample_type), TRUE),
    list("Class", as.character(smp$Class), cat == ""),
    list("QC group", as.character(smp$Class), cat == "QC"),
    list("Batch", paste("Batch", smp$batch_id), TRUE)
  )
  purrr::map_dfr(groupings, function(g) {
    keys <- unique(g[[2]][g[[3]]])
    purrr::map_dfr(keys, function(k) {
      cols <- g[[3]] & g[[2]] == k
      if (sum(cols) < 2) return(NULL)
      tibble::tibble(vIS = names, Grouping = g[[1]], Group = k, n = sum(cols),
                     `RSD%` = round(apply(mat[, cols, drop = FALSE], 1, rsd_perc, na.rm = TRUE), 1))
    })
  })
}

#' The outcome of each feature, for the m/z and retention time map
#'
#' @param row_df The `row_df` of the parsed data (feature IDs as row names)
#' @param calib_rows `NULL` before calibration, or a data frame of `feature_id`, `to_export` and
#'   `has_proper_range` (one row per feature and batch) from the calibrated data
#' @param removed `NULL`, or a data frame of removed targeted features (`feature_id`, `reason`)
#' @returns A tibble of `feature_id`, `label`, `.rt`, `mz` and `outcome`
#' @export
mz_rt_outcome <- function(row_df, calib_rows = NULL, removed = NULL) {
  std <- row_df$.std_type
  type <- dplyr::case_when(
    std %in% "vIS" ~ "Volumetric IS",
    util$is_internal_std(std) ~ "Internal standard",
    util$is_targeted_feature(std) ~ "Targeted",
    TRUE ~ "Non-targeted"
  )
  out <- tibble::tibble(
    feature_id = rownames(row_df),
    label = ifelse(type == "Non-targeted", sprintf("Unk_%.4f_%.2f", row_df$mz, row_df$.rt), row_df$feature_name),
    .rt = row_df$.rt, mz = row_df$mz, type = type
  )
  if (is.null(calib_rows) || nrow(calib_rows) == 0) {
    return(dplyr::transmute(out, feature_id, label, .rt, mz, outcome = type))
  }
  res <- calib_rows |>
    dplyr::group_by(feature_id) |>
    dplyr::summarise(quantified = any(to_export %in% TRUE), valid = any(has_proper_range %in% TRUE))
  rem <- if (is.null(removed)) tibble::tibble(feature_id = character(), reason = character()) else
    dplyr::distinct(removed[, c("feature_id", "reason")], feature_id, .keep_all = TRUE)
  out |>
    dplyr::left_join(res, by = "feature_id") |>
    dplyr::left_join(rem, by = "feature_id") |>
    dplyr::transmute(feature_id, label, .rt, mz, outcome = dplyr::case_when(
      type != "Targeted" ~ type,
      !is.na(reason) ~ paste0("Removed (", reason, ")"),
      quantified %in% TRUE ~ "Quantified",
      valid %in% TRUE ~ "Valid curve, not found in samples",
      TRUE ~ "No valid calibration"
    ))
}

#' Interactive map of the features by retention time and m/z, coloured by outcome
#'
#' @param df A tibble from [mz_rt_outcome()]
#' @param height Height of the plot in pixels
#' @returns A plotly object. Non-targeted features are hidden until switched on in the legend.
#' @export
plotly_mz_rt_map <- function(df, height = 520) {
  style <- tibble::tribble(
    ~outcome,                             ~color,    ~symbol,
    "Quantified",                         "#2E9E5B", "circle",
    "Targeted",                           "#2E9E5B", "circle",
    "Valid curve, not found in samples",  "#F7B32B", "circle",
    "No valid calibration",               "#c23636", "circle",
    "Removed (Blank filter)",             "#7E57C2", "x",
    "Removed (QC RSD%)",                  "#455A64", "x",
    "Internal standard",                  "#0D121C", "diamond",
    "Volumetric IS",                      "#6FAAF7", "diamond",
    "Non-targeted",                       "#BDBDBD", "circle"
  )
  p <- plotly::plot_ly(height = height)
  for (k in seq_len(nrow(style))) {
    d <- df[df$outcome == style$outcome[k], ]
    if (nrow(d) == 0) next
    nt <- style$outcome[k] == "Non-targeted"
    p <- plotly::add_markers(
      p, data = d, x = ~.rt, y = ~mz, name = paste0(style$outcome[k], " (", nrow(d), ")"),
      marker = list(color = style$color[k], symbol = style$symbol[k], size = if (nt) 5 else 9,
                    opacity = if (nt) 0.5 else 0.9),
      visible = if (nt) "legendonly" else TRUE,
      text = ~paste0("<b>", label, "</b><br>RT ", round(.rt, 2), " min · m/z ", round(mz, 5)),
      hoverinfo = "text"
    )
  }
  plotly::layout(p, xaxis = list(title = "Retention time [min]"), yaxis = list(title = "m/z"),
                 legend = list(orientation = "v", x = 1.02, y = 1), font = list(family = "Lato"))
}

#' Interactive plot of one internal standard: box plot per sample group and scatter plot along
#' the injection order (the same layout as the internal report's IS viewer)
#'
#' @param y The peak areas (one per sample)
#' @param smp A data frame with `sample_name`, `Class`, `batch_id`, `injection_order` and `group`
#'   (see [sample_group()]), in the order of `y`
#' @param name The name of the internal standard
#' @param outlier A logical vector: outlier samples (drawn as crosses)
#' @param failed Whether the internal standard failed (shown in the title)
#' @param colors Named colours of the groups (see [sample_group_colors()])
#' @param height Height of the plot in pixels
#' @returns A plotly object with a log / linear switch
#' @export
plotly_is_distribution <- function(y, smp, name, outlier = rep(FALSE, length(y)), failed = FALSE,
                                   colors = sample_group_colors(smp$group), height = 480) {
  txt <- paste0("<b>", smp$sample_name, "</b><br>", smp$Class, " · batch ", smp$batch_id,
                " · inj. ", smp$injection_order, "<br>peak area ", signif(y, 4),
                ifelse(outlier, "<br><b>outlier sample</b>", ""))
  p <- plotly::plot_ly(height = height)
  for (g in unique(smp$group)) {
    k <- smp$group == g
    col <- if (g %in% names(colors)) unname(colors[[g]]) else "#999999"
    p <- p |>
      plotly::add_trace(type = "box", y = y[k], name = g, legendgroup = g, marker = list(color = col),
                        line = list(color = col), fillcolor = col, opacity = 0.6, boxpoints = FALSE,
                        showlegend = FALSE, hoverinfo = "y", xaxis = "x", yaxis = "y") |>
      plotly::add_trace(type = "scatter", mode = "markers", x = smp$injection_order[k], y = y[k],
                        name = g, legendgroup = g, text = txt[k], hoverinfo = "text",
                        marker = list(color = col, size = 6, symbol = ifelse(outlier[k], "x", "circle")),
                        xaxis = "x2", yaxis = "y")
  }
  plotly::layout(
    p,
    title = list(text = if (failed) paste0(name, " <span style='color:#c23636'>(failed: removed before normalization)</span>") else name,
                 x = 0.02),
    font = list(family = "Lato"), hovermode = "closest", margin = list(t = 70, r = 20, b = 60, l = 80),
    xaxis = list(domain = c(0, 0.27), tickangle = -30), xaxis2 = list(domain = c(0.32, 1), title = "Injection order"),
    yaxis = list(title = "Peak area", type = "log", exponentformat = "E"),
    legend = list(orientation = "v", x = 1.02, y = 1),
    # Log / linear switch at the top right, away from the title at the top left
    updatemenus = list(list(type = "buttons", direction = "right", x = 1, y = 1.02, xanchor = "right",
                            yanchor = "bottom", pad = list(t = 0, r = 0),
                            buttons = list(list(label = "Log", method = "relayout", args = list("yaxis.type", "log")),
                                           list(label = "Linear", method = "relayout", args = list("yaxis.type", "linear")))))
  )
}
