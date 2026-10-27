# ------------------------------------------------------------------------------------------- #
# Functions to show in reports
# ------------------------------------------------------------------------------------------- #
box::use(util = ./msdial_utils)



#' Numbers in scientific notation for hover texts, e.g. "1.234E+06"
#' @param x Numbers
#' @export
sci_txt <- function(x) ifelse(is.na(x), "NA", toupper(formatC(x, format = "e", digits = 3)))
#' Numbers for hover texts: scientific from 1000 on (e.g. "1.234E+03"), otherwise 4 significant
#' digits (e.g. "0.02345")
#' @param x Numbers
#' @export
num_txt <- function(x) ifelse(is.na(x), "NA", ifelse(abs(x) >= 1000, sci_txt(x), as.character(signif(x, 4))))
#  -----  COLOURS  -------------------------------------------------------------

#' Colour-blind safe colours of Okabe & Ito (2008), named
#' @export
okabe_ito <- c(orange = "#E69F00", skyblue = "#56B4E9", green = "#009E73", yellow = "#F0E442",
               blue = "#0072B2", vermillion = "#D55E00", purple = "#CC79A7", black = "#000000",
               grey = "#999999")

#' `n` colour-blind safe colours for categories: Okabe-Ito (up to 7), Paul Tol's "muted" (up to
#' 9), viridis for more (no palette keeps many categories apart: use the legend / hover then).
#' Without the pale yellow of Okabe-Ito and the pale sand of Tol (hard to see): black and brown.
#'
#' @param n The number of colours
#' @param exclude Colours not to use (e.g. those of blanks and QCs)
#' @export
cb_colors <- function(n, exclude = character(0)) {
  if (n <= 0) return(character(0))
  exclude <- toupper(exclude)
  oi <- setdiff(unname(okabe_ito[c("blue", "orange", "green", "vermillion", "skyblue", "purple", "black")]),
                exclude)
  tol <- c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#661100", "#CC6677", "#882255", "#AA4499")
  # Vermillion taken (the blanks): no orange or reddish colours, which are hard to tell from it
  if (toupper(okabe_ito[["vermillion"]]) %in% exclude) {
    oi <- setdiff(oi, okabe_ito[["orange"]])
    tol <- setdiff(tol, c("#661100", "#CC6677", "#882255"))
  }
  # Blue taken (the QCs): sky blue, which is close to it, last
  if (toupper(okabe_ito[["blue"]]) %in% exclude) oi <- c(setdiff(oi, okabe_ito[["skyblue"]]), intersect(okabe_ito[["skyblue"]], oi))
  if (n <= length(oi)) oi[seq_len(n)] else if (n <= length(tol)) tol[seq_len(n)] else grDevices::hcl.colors(n, "viridis")
}
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
  # Remaining classes. The calibration levels (one class each) get grey shades, so that the study
  # classes keep the few distinct colour-blind safe colours (with the levels among them, there
  # were too many classes for the palettes, and similar viridis purples were used instead)
  cal <- if ("CalCurve" %in% names(by_cat)) unique(as.character(by_cat[["CalCurve"]])) else character(0)
  rest <- setdiff(unlist(by_cat[! names(by_cat) %in% c(names(color_cat), "CalCurve")]), cal)
  # Colour-blind safe colours other than those of the fixed classes
  ggcolor <- cb_colors(length(rest), exclude = unname(unlist(color_cat)))
  names(ggcolor) <- rest
  if (length(cal) > 0) {
    ggcolor <- c(ggcolor, stats::setNames(grDevices::gray.colors(length(cal), start = 0.3, end = 0.75), cal))
  }
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
    rsd <- matrix(vapply(stats::setNames(nm = mat_ids), function(nm) {
      as.numeric(apply(qc_se[[nm]], 1, rsd_perc))
    }, numeric(nrow(qc_se))), nrow = nrow(qc_se), ncol = length(mat_ids),
    dimnames = list(rownames(qc_se), unname(mat_ids)))   # Features x matrix IDs
    # `feature_id` also without features (no targeted compounds), for the joins of the callers
    tibble::as_tibble(rsd) |> tibble::add_column(feature_id = rownames(qc_se) %||% character(), .before = 1)
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
norm_sign_colors <- c(okabe_ito[["skyblue"]], okabe_ito[["orange"]], okabe_ito[["green"]], "#7f7f7f") |>
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
  ccline_df <- calcurve_line_df(lim_df, log_scale)
  ggplot2::geom_line(
    ggplot2::aes(x = conc, y = .y_value), data = ccline_df,
    color = color,
    alpha = alpha,
    ...
  )
}

#' Points of the smooth calibration curve line of each chemical (see
#' [geom_calibration_curve_line()]; also for [plotly_calcurve_samples()])
#'
#' @inheritParams geom_calibration_curve_line
#' @returns A tibble of `conc`, `.y_value` and the other columns of `lim_df` (e.g. `feature_id`)
#' @export
calcurve_line_df <- function(lim_df, log_scale = FALSE) {
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
  dplyr::arrange(ccline_df, feature_id, conc)      # To make sure
}

#' Title of a single-chemical plotly plot as a coloured band over the plot (the colour tells the
#' state of its calibration curve, as the facet strips of the ggplot versions)
.title_band <- function(title, fill) {
  list(
    shapes = list(list(type = "rect", xref = "paper", yref = "paper", x0 = 0, x1 = 1, y0 = 1.01, y1 = 1.09,
                       fillcolor = fill, line = list(width = 0))),
    annotations = list(list(text = title, xref = "paper", yref = "paper", x = 0.5, y = 1.05,
                            xanchor = "center", yanchor = "middle", showarrow = FALSE,
                            font = list(size = 14)))
  )
}

#' Plotly shapes of the regions below the LLOQ and below the LOD (grey, the latter darker)
#'
#' @param lod,lloq The LOD and the LLOQ (`NA` or `NULL`: none)
#' @param xs The concentrations shown (on a log axis, the regions start a decade below the lowest)
#' @param log_scale Whether the x axis is in log scale. The shapes are placed by the concentration
#'   also then (plotly.js 2.11 of the app and the reports; log10 values are misplaced there and
#'   stretch the axis)
#' @returns A list of plotly shapes
#' @export
llox_shapes <- function(lod, lloq, xs, log_scale = FALSE) {
  lim <- c(lod[1], lloq[1])
  x_from <- if (log_scale) suppressWarnings(min(xs[xs > 0], lim, na.rm = TRUE)) / 10 else 0
  shade <- function(x1) {
    if (length(x1) == 0 || !is.finite(x1) || !is.finite(x_from)) return(NULL)
    list(type = "rect", xref = "x", yref = "paper", x0 = x_from, x1 = x1, y0 = 0, y1 = 1, layer = "below",
         fillcolor = "rgba(64,64,64,0.25)", line = list(width = 0))
  }
  Filter(Negate(is.null), list(shade(lloq[1]), shade(lod[1])))
}

#' Plotly shapes of the ULOQ: the region above it (olive) and a dashed line at it
#'
#' @param uloq The ULOQ (`NA` or `NULL`: none, e.g. a quadratic curve)
#' @param xs The concentrations shown (the region ends at the highest)
#' @param log_scale Not used: the shapes are placed by the concentration also on a log axis (see
#'   [llox_shapes()])
#' @returns A list of plotly shapes
#' @export
uloq_shapes <- function(uloq, xs, log_scale = FALSE) {
  if (length(uloq) == 0 || !is.finite(uloq)) return(list())
  x_max <- suppressWarnings(max(xs, na.rm = TRUE))
  c(
    if (is.finite(x_max) && x_max > uloq) {
      list(list(type = "rect", xref = "x", yref = "paper", x0 = uloq, x1 = x_max, y0 = 0, y1 = 1,
                layer = "below", fillcolor = "rgba(153,153,51,0.15)", line = list(width = 0)))
    },
    list(list(type = "line", xref = "x", yref = "paper", x0 = uloq, x1 = uloq, y0 = 0, y1 = 1,
              line = list(color = "#999933", width = 1.5, dash = "dash")))
  )
}

#' Interactive plot of the calibration points of one chemical (app 4.4), drawn with plotly
#' directly (faster than converting a ggplot)
#'
#' @param d A data frame of the calibration points: `.cc_pt` (concentration), the column `y`,
#'   `c_type` (place of the point, e.g. "Within limits"), `Batch`, `injection_order`, `sample_name`
#' @param y The name of the signal column
#' @param title The name of the chemical
#' @param title_fill The colour of the title band
#' @param log_scale Whether the curve is fitted in log scale (log axes)
#' @param x_title,y_title The titles of the axes
#' @param height Height of the plot in pixels
#' @param lod,lloq,uloq The limits of the curve, shown as regions as in [plotly_calcurve_samples()]
#'   (`NA`: none)
#' @returns A plotly object
#' @export
plotly_calcurve_points <- function(d, y, title, title_fill = "lightgray", log_scale = FALSE,
                                   x_title = "Calibrant Concentration", y_title = "Signal", height = 400,
                                   lod = NA, lloq = NA, uloq = NA) {
  type_cols <- c("Within limits" = okabe_ito[["green"]], "Above the ULOQ" = "#999933", "LOD = LLOQ" = okabe_ito[["orange"]],
                 "Out-of-range" = "gray", "Cal0" = okabe_ito[["vermillion"]], "LOD" = okabe_ito[["blue"]])
  batches <- stringr::str_sort(unique(as.character(d$Batch)), numeric = TRUE)
  symbols <- c("circle", "triangle-up", "square", "diamond", "star", "cross", "x", "hexagon",
               "triangle-down", "pentagon", "hourglass", "bowtie")
  sym <- stats::setNames(rep_len(symbols, length(batches)), batches)
  txt <- paste0("Batch: ", d$Batch, "<br>inj: ", d$injection_order, ", conc: ", d$.cc_pt,
                "<br>Name: ", d$sample_name, "<br>int: ", num_txt(d[[y]]))
  p <- plotly::plot_ly(height = height)
  for (ty in intersect(names(type_cols), unique(d$c_type))) {
    k <- d$c_type == ty
    p <- plotly::add_markers(p, x = d$.cc_pt[k], y = d[[y]][k], name = ty, legendgroup = ty,
                             marker = list(color = type_cols[[ty]], size = 8,
                                           symbol = unname(sym[as.character(d$Batch[k])])),
                             text = txt[k], hoverinfo = "text")
  }
  # The batches (shapes) in the legend, without points of their own
  if (length(batches) > 1) {
    for (b in batches) {
      p <- plotly::add_markers(p, x = NA_real_, y = NA_real_, name = paste("Batch", b), legendgroup = "batch",
                               marker = list(color = "black", size = 8, symbol = sym[[b]]), hoverinfo = "skip")
    }
  }
  band <- .title_band(title, title_fill)
  plotly::layout(
    p, font = list(family = "Lato"), hovermode = "closest", annotations = band$annotations,
    shapes = c(llox_shapes(lod, lloq, d$.cc_pt, log_scale), uloq_shapes(uloq, d$.cc_pt, log_scale), band$shapes),
    xaxis = list(title = x_title, type = if (log_scale) "log" else "linear", exponentformat = "E"),
    yaxis = list(title = y_title, type = if (log_scale) "log" else "linear", exponentformat = "E"),
    legend = list(orientation = "v", x = 1.02, y = 1, xanchor = "left", tracegroupgap = 10),
    margin = list(t = 60, b = 60, l = 70, r = 150)
  )
}

#' Interactive plot of the samples of one chemical on its calibration curve (app 4.5), drawn with
#' plotly directly: the curve, the calibration points, the samples by class and the regions below
#' the LOD and the LLOQ (shaded)
#'
#' @inheritParams ggplot_calcurve_samples
#' @param title_fill The colour of the title band
#' @param height Height of the plot in pixels
#' @returns A plotly object
#' @export
plotly_calcurve_samples <- function(x_se, cc_se, mat_id, colors_of_classes, log_scale = FALSE,
                                    title_fill = "lightgray", height = 400) {
  .chq_if_a_single_char(mat_id)
  fid <- rownames(x_se)[1]
  x_se <- x_se[fid, ]
  cc_df <- SumExp::as_tibble(cc_se[fid, ]) |> tidyr::drop_na(tidyselect::all_of(unname(mat_id)))
  rd <- SumExp::row_df(x_se)
  lim_df <- rd |> dplyr::select(feature_name, lloq, lod, min_c_conc, max_c_conc, calcurve_model) |>
    tibble::rownames_to_column("feature_id")
  x_df <- SumExp::as_tibble(x_se)
  # Values below the LOD / LLOQ without a value ("n.d." / "n.q."): at their calculated
  # concentration (before the LOD/LLOQ rules), i.e. on the curve
  x_df$.flag <- ""
  if (all(c("conc_flag", "conc0") %in% names(x_df))) {
    cens <- is.na(x_df$conc) & !is.na(x_df$conc_flag)
    x_df$conc[cens] <- x_df$conc0[cens]
    x_df$.flag[cens] <- paste0(" (", x_df$conc_flag[cens], ")")
  }
  x_df <- x_df[!is.na(x_df$conc), ]
  hover <- function(df, conc) paste0("batch: ", df$batch_id, "<br>inj: ", df$injection_order, ", conc: ", conc,
                                     "<br>name: ", df$sample_name, "<br>int: ", num_txt(df[[mat_id]]))
  p <- plotly::plot_ly(height = height)
  line <- calcurve_line_df(lim_df, log_scale)
  if (nrow(line) > 0) {
    p <- plotly::add_lines(p, x = line$conc, y = line$.y_value, name = "Calibration curve", hoverinfo = "skip",
                           line = list(color = "rgba(95,158,160,0.8)", width = 2))
  }
  p <- plotly::add_markers(p, x = cc_df$c_conc, y = cc_df[[mat_id]], name = "Calibration points",
                           marker = list(color = "black", size = 6), text = hover(cc_df, cc_df$c_conc),
                           hoverinfo = "text")
  classes <- unique(as.character(x_df$Class))
  classes <- c(intersect(names(colors_of_classes), classes), setdiff(classes, names(colors_of_classes)))
  for (cl in classes) {
    k <- as.character(x_df$Class) == cl
    p <- plotly::add_markers(p, x = x_df$conc[k], y = x_df[[mat_id]][k], name = cl,
                             marker = list(color = if (cl %in% names(colors_of_classes)) colors_of_classes[[cl]] else "grey50",
                                           size = 7, opacity = 0.85),
                             text = hover(x_df[k, ], paste0(signif(x_df$conc[k], 4), x_df$.flag[k])), hoverinfo = "text")
  }
  # Regions below the LLOQ and below the LOD (darker), and above the ULOQ, as in the reports
  xs <- c(x_df$conc, cc_df$c_conc)
  band <- .title_band(rd$feature_name[1], title_fill)
  shapes <- c(llox_shapes(rd$lod[1], rd$lloq[1], xs, log_scale),
              uloq_shapes(rd$uloq[1], c(xs, line$conc), log_scale), band$shapes)
  plotly::layout(
    p, font = list(family = "Lato"), hovermode = "closest", shapes = shapes, annotations = band$annotations,
    xaxis = list(title = label_if_has(x_df$conc, "Concentration"), type = if (log_scale) "log" else "linear",
                 exponentformat = "E"),
    yaxis = list(title = label_if_has(x_se[[mat_id]], mat_id), type = if (log_scale) "log" else "linear",
                 exponentformat = "E"),
    legend = list(orientation = "v", x = 1.02, y = 1, xanchor = "left"),
    margin = list(t = 60, b = 60, l = 70, r = 150)
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
  x_df <- SumExp::as_tibble(x_se)
  # Values below the LOD / LLOQ without a value ("n.d." / "n.q."): shown at their calculated
  # concentration (before the LOD/LLOQ rules), i.e. on the curve
  x_df$.flag <- ""
  if (all(c("conc_flag", "conc0") %in% names(x_df))) {
    cens <- is.na(x_df$conc) & !is.na(x_df$conc_flag)
    x_df$conc[cens] <- x_df$conc0[cens]
    x_df$.flag[cens] <- paste0(" (", x_df$conc_flag[cens], ")")
  }
  x_df <- x_df |>
    tidyr::drop_na(conc) |>
    dplyr::mutate(
      txt = paste(
        "batch: ", batch_id, "\n",
        "inj: ", injection_order, ", conc: ", paste0(conc, .flag), "\n",
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
sample_group_colors <- function(groups, fixed = c(Blank = okabe_ito[["vermillion"]], QC = okabe_ito[["blue"]])) {
  rest <- c(sort(setdiff(unique(groups), c(names(fixed), "Calibrant"))),
            intersect("Calibrant", unique(groups)))
  c(stats::setNames(cb_colors(length(rest), exclude = unname(fixed)), rest), fixed)
}

#' Colour-blind safe colours, named (for batches, internal standards, QC groups); see [cb_colors()]
#'
#' @param x The names (levels)
#' @export
hue_colors <- function(x) stats::setNames(cb_colors(length(x)), x)

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
                "<br>total ", sci_txt(tot))
  p <- plotly::plot_ly(height = height)
  if (colour_by == "is") {
    nm <- SumExp::row_df(is_se)$feature_name
    col <- hue_colors(nm)
    for (i in seq_along(nm)) {
      p <- plotly::add_bars(p, x = io, y = v[i, ], name = nm[i], marker = list(color = col[[i]]),
                            text = paste0(who, "<br><b>", nm[i], "</b> ", sci_txt(v[i, ])),
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
                " · inj. ", smp$injection_order, "<br>peak area ", sci_txt(y))   # Scientific, as 3.5
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
    list(list(type = "buttons", direction = "right", x = 0.55, y = 1.02, xanchor = "center",
              yanchor = "bottom", active = match(colour_by, names(n_traces)) - 1L,
              buttons = list(
                list(label = "Sample group", method = "restyle", args = list("visible", as.list(vis("group")))),
                list(label = "Batch", method = "restyle", args = list("visible", as.list(vis("batch"))))
              )))
  }
  plotly::layout(p, title = list(text = name, x = 0.02), barmode = "stack",
                 bargap = 0, hovermode = "closest", font = list(family = "Lato"),
                 xaxis = list(title = "Injection order"),
                 yaxis = list(title = paste("Peak area,", name), exponentformat = "E", hoverformat = ".3e"),
                 legend = list(orientation = "v", x = 1.02, y = 1), updatemenus = menus,
                 margin = list(t = if (switchable) 70 else 50))
}

#' Colours of the concentrations replaced by the LOD/LLOQ rules (as the cells of the exported
#' tables: red, yellow and blue), and of the measured ones
#' @export
imputed_colors <- c("Measured" = "grey25", "Not detected: LLOQ/4" = okabe_ito[["vermillion"]],
                    "Detected: LLOQ/2" = okabe_ito[["orange"]], "Set to LLOQ" = okabe_ito[["skyblue"]])

#' Whether each concentration is measured or replaced by an LOD/LLOQ rule
#'
#' @param conc Concentrations
#' @param lloq The LLOQ of the chemical of each concentration
#' @returns A factor with the levels `names(imputed_colors)`, `NA` for missing concentrations
#' @export
imputed_kind <- function(conc, lloq) {
  kind <- dplyr::case_when(
    is.na(conc) ~ NA_character_,
    !is.na(lloq) & conc == lloq / 4 ~ "Not detected: LLOQ/4",
    !is.na(lloq) & conc == lloq / 2 ~ "Detected: LLOQ/2",
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
                                    subtitle = "", y_title = "Peak area", height = 480, log = FALSE) {
  view <- match.arg(view)
  col <- sample_group_colors(smp$group)
  # Blanks first, then QCs, the study classes and calibrants
  levs <- c(intersect(c("Blank", "QC"), smp$group),
            setdiff(names(col), c("Blank", "QC", "Calibrant")) |> intersect(smp$group),
            intersect("Calibrant", smp$group))
  threshold <- factor * mean(y[smp$group == "Blank"], na.rm = TRUE)
  txt <- paste0("<b>", smp$sample_name, "</b><br>", smp$Class, " · batch ", smp$batch_id,
                " · inj. ", smp$injection_order, "<br>", num_txt(y))
  p <- plotly::plot_ly(height = height)
  if (view == "groups") {
    for (i in seq_along(levs)) {
      k <- smp$group == levs[i] & !is.na(y)
      p <- plotly::add_bars(p, x = i, y = mean(y[k]), name = levs[i], width = 0.7,
                            error_y = list(type = "data", array = stats::sd(y[k]), color = "grey30"),
                            marker = list(color = grDevices::adjustcolor(col[[levs[i]]], 0.55),
                                          line = list(color = col[[levs[i]]], width = 1.5)),
                            hovertemplate = paste0("<b>", levs[i], "</b><br>mean ", num_txt(mean(y[k])), " (n = ", sum(k),
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
                           hovertemplate = paste0("threshold ", num_txt(threshold), "<extra></extra>"))
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
    yaxis = list(title = y_title, exponentformat = "E", type = if (log) "log" else "linear"),
    annotations = notes, legend = list(orientation = "v", x = 1.02, y = 1), margin = list(t = 80),
    # Linear / log at the top centre: away from the title (left) and plotly's toolbar (right);
    # `log`: starts in log scale (the choice of the user, kept by the app)
    updatemenus = list(list(type = "buttons", direction = "right", x = 0.55, y = 1.1,
                            xanchor = "center", yanchor = "bottom", buttons = axis_buttons,
                            active = if (log) 1L else 0L))
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
#' @param flag `NULL`, or "n.d." / "n.q." per sample for the values below the LOD / LLOQ without
#'   a value (not shown; their number is given under the title)
#' @returns A plotly object
#' @export
plotly_conc_by_class <- function(conc, lloq, cats, cat_colors, hover, name, y_title, log = FALSE,
                                 height = 460, flag = NULL) {
  n_flag <- if (is.null(flag)) integer() else table(flag[is.na(conc) & !is.na(flag)])
  flag_note <- if (length(n_flag) == 0) "" else {
    paste0("Not shown (no value): ", paste(names(n_flag), n_flag, collapse = ", "))
  }
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
    annotations = if (nzchar(flag_note)) {
      list(list(text = flag_note, x = 0, y = 1.02, xref = "paper", yref = "paper", xanchor = "left",
                yanchor = "bottom", showarrow = FALSE, font = list(size = 12, color = "grey40")))
    },
    margin = list(t = 70),
    updatemenus = list(list(type = "buttons", direction = "right", x = 0.55, y = 1.02,
                            xanchor = "center", yanchor = "bottom", active = if (log) 1 else 0,
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

#' Icons of the categories of the calibration notes (`util$calib_note_categories`): the inner
#' markup of a 24 x 24 SVG
.note_icon_svg <- c(
  notvalid = '<polygon points="7,1.5 17,1.5 22.5,7 22.5,17 17,22.5 7,22.5 1.5,17 1.5,7" fill="#D55E00"/><path d="M8 8l8 8M16 8l-8 8" stroke="#fff" stroke-width="2.6" stroke-linecap="round"/>',
  kept = '<path d="M12 2L23 21.5H1Z" fill="#E69F00" stroke="#E69F00" stroke-width="1" stroke-linejoin="round"/><path d="M12 8.5v6" stroke="#1a1a1a" stroke-width="2.6" stroke-linecap="round"/><circle cx="12" cy="18" r="1.5" fill="#1a1a1a"/>',
  global = '<circle cx="12" cy="12" r="11" fill="#0072B2"/><g fill="none" stroke="#fff" stroke-width="1.6"><circle cx="12" cy="12" r="6.5"/><ellipse cx="12" cy="12" rx="2.8" ry="6.5"/><path d="M5.5 12h13"/></g>',
  uloq = '<rect x="3.5" y="3.5" width="17" height="17" rx="2" transform="rotate(45 12 12)" fill="#999933"/><g stroke="#fff" stroke-width="2.2" stroke-linecap="round" stroke-linejoin="round" fill="none"><path d="M8 7.5h8"/><path d="M12 17v-6.5M9 13.3l3-3 3 3"/></g>',
  detection = '<circle cx="12" cy="12" r="11" fill="#7A7A7A"/><g stroke="#fff" stroke-width="1.7" fill="none" stroke-linecap="round"><path d="M5 12c2-3.5 4.4-5 7-5s5 1.5 7 5c-2 3.5-4.4 5-7 5s-5-1.5-7-5z"/><circle cx="12" cy="12" r="2"/><path d="M6 18L18 6"/></g>',
  removed = '<rect x="1.5" y="1.5" width="21" height="21" rx="4" fill="#CC79A7"/><path d="M6 6.5h12l-4.5 5.5v5l-3 1.5v-6.5z" fill="#fff"/>'
)

#' The calibration notes as icons, one HTML snippet per chemical
#'
#' One icon per category that has a note (in the order of `util$calib_note_categories`); hovering
#' an icon shows its note. The notes are also in the cell as hidden text, so that the table can be
#' searched and sorted by them. Use in a `DT::datatable()` with this column not escaped.
#'
#' @param notes A tibble of `util$calib_notes()`, optionally with a `removed` column
#' @param size The size of the icons in pixels
#' @returns A character vector, `""` when there is nothing to note
#' @export
calib_notes_html <- function(notes, size = 18) {
  cats <- intersect(names(util$calib_note_categories), names(notes))
  icon <- function(cat, txt) {
    sprintf('<span title="%s" style="display:inline-block;margin-right:3px;vertical-align:middle;line-height:0;cursor:help"><svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" viewBox="0 0 24 24" role="img" aria-label="%s">%s</svg></span>',
            htmltools::htmlEscape(txt, attribute = TRUE), size, size,
            htmltools::htmlEscape(util$calib_note_categories[[cat]], attribute = TRUE), .note_icon_svg[[cat]])
  }
  vapply(seq_len(nrow(notes)), function(i) {
    has <- cats[vapply(cats, \(cat) !is.na(notes[[cat]][i]) && nzchar(notes[[cat]][i]), logical(1))]
    if (length(has) == 0) return("")
    txt <- vapply(has, \(cat) notes[[cat]][i], "")
    paste0(paste(mapply(icon, has, txt), collapse = ""),
           '<span style="display:none">', htmltools::htmlEscape(paste(txt, collapse = "; ")), "</span>")
  }, "")
}

#' A legend of the icons of [calib_notes_html()], as one line of HTML
#' @param cats The categories to show (names of `util$calib_note_categories`)
#' @export
calib_notes_legend_html <- function(cats = names(util$calib_note_categories)) {
  items <- vapply(cats, function(cat) {
    sprintf('<span style="white-space:nowrap;margin-right:14px"><svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 24 24" style="vertical-align:-3px;margin-right:4px" aria-hidden="true">%s</svg>%s</span>',
            .note_icon_svg[[cat]], util$calib_note_categories[[cat]])
  }, "")
  paste0('<div class="q-note-legend" style="font-size:12.5px;color:#555;margin:4px 0 6px">Notes (hover an icon for the full note): ',
         paste(items, collapse = " "), "</div>")
}

#' A note on the range of the calibration curves, one per chemical: no quantifiable sample, and
#' the levels dropped by `--enforce_linear_range`
#'
#' @param row_df The `row_df` of calibrated data (`no_quant_samples` and `linear_range_note`, from
#'   processing with this version)
#' @returns A character vector, `""` when there is nothing to note
#' @export
calib_range_note <- function(row_df) {
  valid <- row_df$has_proper_range %in% TRUE
  nq <- if (is.null(row_df$no_quant_samples)) rep(FALSE, nrow(row_df)) else row_df$no_quant_samples %in% TRUE
  nq_txt <- ifelse(nq & valid, "No sample or QC at or above the LLOQ: the full calibration range is shown", "")
  lin <- if (is.null(row_df$linear_range_note)) rep(NA_character_, nrow(row_df)) else as.character(row_df$linear_range_note)
  lin[!valid] <- NA_character_
  dplyr::case_when(is.na(lin) ~ nq_txt, nq_txt == "" ~ lin, TRUE ~ paste0(nq_txt, ". ", lin))
}

#' RSD% of the targeted compounds among the QC replicates under each normalization method, with
#' the method of the lowest RSD% (app 3.8 and 3.9; prepared by process.R)
#'
#' @param norm_se The normalized data (`to_report[["normalized"]]`), with the QC samples
#' @returns A list of `df` (a tibble: `QC`, `feature_id`, one column per method, `sign`) and `labs`
#'   (the method matrix IDs, named by the first word of their labels)
#' @export
qc_rsd_by_method <- function(norm_se) {
  labs <- SumExp::name_labs(norm_se)
  names(labs) <- gsub(" .*", "", names(labs))   # First word only
  df <- extract_quant_qc(norm_se) |>
    calc_rsd_qstd(labs) |>
    mark_lowest_rsd_norm(labs)
  list(df = df, labs = labs)
}

#' RSD% of the raw peak areas of each internal standard within each QC group and each sample class
#' (app 3.4; prepared by process.R)
#'
#' @param is_se A [`SumExp::SumExp`] object of the internal standards (with the `raw` matrix)
#' @returns A list of `rsd` (a matrix: groups in rows, internal standards (names) in columns; 2
#'   decimals) and `n` (the number of samples of each group)
#' @export
is_rsd_by_group <- function(is_se) {
  rsd_rows <- function(se) apply(se[["raw"]], 1, \(.x) round(rsd_perc(.x), 2))
  qc_lst <- extract_qc_samples_to_list(is_se)
  m_rsd1 <- if (length(qc_lst) > 0) {
    m <- do.call(cbind, lapply(qc_lst, rsd_rows))   # A matrix even for a single value
    stopifnot(identical(rownames(m), rownames(is_se)))
    data.frame(m, check.names = FALSE)
  } else {
    data.frame(row.names = rownames(is_se))
  }
  rownames(m_rsd1) <- SumExp::row_df(is_se)$feature_name
  se <- util$exclude_ctrl_smpl_cat(is_se, "QC")
  m_rsd2 <- stats::setNames(nm = unique(SumExp::col_df(se)$Class)) |>
    lapply(function(cls) rsd_rows(se[, SumExp::col_df(se)$Class == cls])) |>
    do.call(cbind, args = _)
  stopifnot(identical(rownames(m_rsd2), rownames(se)))
  rownames(m_rsd2) <- SumExp::row_df(se)$feature_name
  m_rsd2 <- data.frame(m_rsd2, check.names = FALSE)
  n <- c(vapply(qc_lst, ncol, integer(1)),
         vapply(colnames(m_rsd2), \(cls) sum(SumExp::col_df(se)$Class == cls), integer(1)))
  list(rsd = t(cbind.data.frame(m_rsd1, m_rsd2)), n = n)
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
    "Quantified",                         "#009E73", "circle",          # Colour-blind safe (Okabe-Ito)
    "Targeted",                           "#009E73", "circle",
    "Valid curve, not found in samples",  "#E69F00", "circle",
    "No valid calibration",               "#D55E00", "circle",
    "Removed (Blank filter)",             "#CC79A7", "x",
    "Removed (QC RSD%)",                  "#0072B2", "x",
    "Internal standard",                  "#0D121C", "diamond",
    "Volumetric IS",                      "#56B4E9", "diamond",
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
  # WebGL (scattergl): fast also with tens of thousands of features
  plotly::toWebGL(p) |>
    plotly::layout(xaxis = list(title = "Retention time [min]"), yaxis = list(title = "m/z"),
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
                " · inj. ", smp$injection_order, "<br>peak area ", sci_txt(y),
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
    # The groups panel keeps its x-axis when zooming (only the injection order zooms); the
    # shared y-axis (peak area) zooms in both panels
    xaxis = list(domain = c(0, 0.27), tickangle = -30, fixedrange = TRUE),
    xaxis2 = list(domain = c(0.32, 1), title = "Injection order"),
    yaxis = list(title = "Peak area", type = "log", exponentformat = "E", hoverformat = ".3e"),
    legend = list(orientation = "v", x = 1.02, y = 1),
    # Log / linear switch at the top centre, away from the title (left) and plotly's toolbar (right)
    updatemenus = list(list(type = "buttons", direction = "right", x = 0.55, y = 1.02, xanchor = "center",
                            yanchor = "bottom", pad = list(t = 0, r = 0),
                            buttons = list(list(label = "Log", method = "relayout", args = list("yaxis.type", "log")),
                                           list(label = "Linear", method = "relayout", args = list("yaxis.type", "linear")))))
  )
}

#' Interactive histogram of pre-binned values (bars with the bin range and count on hover)
#'
#' @param x The values (`NA` are left out)
#' @param binwidth Width of the bins
#' @param x_title,y_title Titles of the axes
#' @param what What is counted, for the hover text (e.g. "features")
#' @param color Colour of the bars
#' @param height Height of the plot in pixels
#' @returns A plotly object
#' @export
plotly_binned_hist <- function(x, binwidth, x_title, y_title = "Number of features", what = "features",
                               color = "#6FAAF7", height = 400) {
  x <- x[is.finite(x)]
  if (length(x) == 0) {
    return(plotly::plotly_empty(type = "scatter", mode = "markers", height = height))   # No data: no plotly warning
  }
  br <- seq(floor(min(x) / binwidth) * binwidth, ceiling(max(x) / binwidth) * binwidth + binwidth, by = binwidth)
  h <- graphics::hist(x, breaks = br, plot = FALSE, right = FALSE)
  keep <- h$counts > 0
  txt <- paste0(num_txt(h$breaks[-length(h$breaks)]), " – ", num_txt(h$breaks[-1]), "<br>",
                h$counts, " ", what)
  plotly::plot_ly(height = height) |>
    plotly::add_bars(x = h$mids[keep], y = h$counts[keep], width = binwidth, text = txt[keep],
                     textposition = "none", hoverinfo = "text", marker = list(color = color),
                     showlegend = FALSE) |>
    plotly::layout(xaxis = list(title = x_title), yaxis = list(title = y_title), bargap = 0,
                   font = list(family = "Lato"), margin = list(t = 30))
}

#' Counts of the histograms of [plotly_value_distribution()]: all values (linear scale) and the
#' positive values (bins of log10), counted once (e.g. when the file is read), not when drawn
#'
#' @inheritParams plotly_value_distribution
#' @returns A list (class `quanta_value_bins`) of `n`, `n_pos`, `n_na`, and `lin` / `log` (each
#'   `NULL` without values, or a list of `breaks`, `counts`, `mids`)
#' @export
value_distribution_bins <- function(values, bins = 50) {
  v <- as.vector(values)
  n_na <- sum(!is.finite(v))
  v <- v[is.finite(v)]
  pos <- v[v > 0]
  count <- function(x, log) {
    if (length(x) == 0) return(NULL)
    lx <- if (log) log10(x) else x
    br <- seq(min(lx), max(lx), length.out = bins + 1)
    if (diff(range(br)) == 0) br <- c(br[1] - 0.5, br[1] + 0.5)
    h <- graphics::hist(lx, breaks = br, plot = FALSE, include.lowest = TRUE)
    list(breaks = h$breaks, counts = h$counts, mids = h$mids)
  }
  structure(list(n = length(v), n_pos = length(pos), n_na = n_na, lin = count(v, FALSE), log = count(pos, TRUE)),
            class = "quanta_value_bins")
}

#' Interactive distribution of all values of a matrix: histograms in linear scale (left) and of
#' the positive values in log scale (right), from pre-binned counts (also for large data)
#'
#' @param values The values (a matrix or vector; `NA` are left out), or their counts from
#'   [value_distribution_bins()]
#' @param bins The number of bins of each histogram
#' @param x_title The title of the x-axes (the values)
#' @param height Height of the plot in pixels
#' @returns A plotly object
#' @export
plotly_value_distribution <- function(values, bins = 50, x_title = "Intensities", height = 420) {
  # Counted here, or before (`values` from [value_distribution_bins()], e.g. when the file was read)
  d <- if (inherits(values, "quanta_value_bins")) values else value_distribution_bins(values, bins)
  n_na <- d$n_na
  bars <- function(h, log) {
    if (is.null(h)) return(plotly::plotly_empty(type = "scatter", mode = "markers"))   # No warning when empty
    lab <- function(b) sci_txt(if (log) 10^b else b)
    txt <- paste0(lab(h$breaks[-length(h$breaks)]), " – ", lab(h$breaks[-1]), "<br>", h$counts, " intensities")
    p <- plotly::plot_ly() |>
      plotly::add_bars(x = h$mids, y = h$counts, width = diff(h$breaks)[1], text = txt,
                       textposition = "none", hoverinfo = "text", showlegend = FALSE,
                       marker = list(color = "#6FAAF7", line = list(color = "#3F7BD0", width = 0.5)))
    # Log panel: bins of log10(value), labelled with the values (e.g. 1E+4)
    ax <- if (log) {
      k <- seq(floor(min(h$breaks)), ceiling(max(h$breaks)))
      list(title = paste(x_title, "(log scale)"), tickvals = k, ticktext = label_e(10^k))
    } else {
      list(title = x_title, exponentformat = "E")
    }
    plotly::layout(p, xaxis = ax, yaxis = list(title = "N intensities"))
  }
  note <- paste0(format(d$n, big.mark = ","), " intensities",
                 if (d$n_pos < d$n) paste0(" · ", format(d$n - d$n_pos, big.mark = ","), " ≤ 0 (not in log scale)"),
                 if (n_na > 0) paste0(" · ", format(n_na, big.mark = ","), " missing"))
  panel_title <- function(txt, x) list(text = paste0("<b>", txt, "</b>"), x = x, y = 1.02, xref = "paper",
                                       yref = "paper", xanchor = "center", yanchor = "bottom", showarrow = FALSE)
  p <- plotly::subplot(bars(d$lin, FALSE), bars(d$log, TRUE), nrows = 1, margin = 0.06, titleX = TRUE, titleY = TRUE)
  p$height <- height   # Of the widget (`height` in `layout()` is deprecated)
  p |>
    plotly::layout(
      bargap = 0, font = list(family = "Lato"), margin = list(t = 70),
      annotations = list(panel_title("Linear scale", 0.22), panel_title("Logarithmic scale", 0.78),
                         list(text = note, x = 0, y = 1.12, xref = "paper", yref = "paper", xanchor = "left",
                              yanchor = "bottom", showarrow = FALSE, font = list(size = 12, color = "grey40")))
    )
}

#' Interactive plot of one compound's concentrations in the QC samples along the injection order:
#' a line per QC group; measured values filled, values replaced by the LOD/LLOQ rules open
#'
#' @param d A data frame of the QC samples of the compound: `sample_name`, `Class` (QC group),
#'   `batch_id`, `injection_order`, `conc`, `kind` ([imputed_kind()]) and `flag` ("n.d." / "n.q."
#'   for values without a value, otherwise `NA`)
#' @param name The name of the compound (title)
#' @param y_title The title of the y-axis
#' @param height Height of the plot in pixels
#' @returns A plotly object
#' @export
plotly_qc_conc <- function(d, name, y_title = "Concentration", height = 440) {
  d <- d[order(d$injection_order), ]
  ok <- !is.na(d$conc)
  n_flag <- table(d$flag[!ok & !is.na(d$flag)])
  note <- if (length(n_flag) == 0) "" else paste0("Not shown (no value): ", paste(names(n_flag), n_flag, collapse = ", "))
  groups <- sort(unique(as.character(d$Class)))
  col <- hue_colors(groups)
  p <- plotly::plot_ly(height = height)
  for (g in groups) {
    k <- ok & d$Class == g
    if (!any(k)) next
    measured <- d$kind[k] %in% "Measured"
    txt <- paste0("<b>", d$sample_name[k], "</b><br>", g, " · batch ", d$batch_id[k], " · inj. ",
                  d$injection_order[k], "<br>conc. ", num_txt(d$conc[k]), " (", d$kind[k], ")")
    p <- plotly::add_trace(p, type = "scatter", mode = "lines+markers", x = d$injection_order[k],
                           y = d$conc[k], name = g, text = txt, hoverinfo = "text",
                           line = list(color = col[[g]], width = 1),
                           marker = list(color = ifelse(measured, col[[g]], "white"), size = 8,
                                         line = list(color = col[[g]], width = 1.5)))
  }
  axis_buttons <- list(
    list(label = "Linear", method = "relayout", args = list(list(yaxis.type = "linear"))),
    list(label = "Log", method = "relayout", args = list(list(yaxis.type = "log")))
  )
  plotly::layout(
    p, title = list(text = paste0(name, if (nzchar(note)) paste0("<br><sup>", note, "</sup>")), x = 0.02),
    hovermode = "closest", font = list(family = "Lato"),
    xaxis = list(title = "Injection order"), yaxis = list(title = y_title, exponentformat = "E"),
    legend = list(orientation = "v", x = 1.02, y = 1,
                  title = list(text = "QC group<br><sup>open points: replaced<br>by the LOD/LLOQ rules</sup>")),
    margin = list(t = 70),
    updatemenus = list(list(type = "buttons", direction = "right", x = 0.55, y = 1.02, xanchor = "center",
                            yanchor = "bottom", buttons = axis_buttons))
  )
}
#' Number of calibration curve samples per concentration, per batch and in total (app 2.2 and the
#' internal report)
#'
#' @param se A [`SumExp::SumExp`] object with all samples
#' @param unit The concentration unit, for the column name
#' @returns A tibble: the concentration (as text, without trailing zeros), one column per batch
#'   ("Batch 1", ...; "All samples" in one-batch mode) and `Total`; no rows without calibration
#'   curve samples
#' @export
calib_samples_per_batch <- function(se, unit = "") {
  cd <- SumExp::col_df(se)
  is_cc <- util$ctrl_smpl_cat(se) == "CalCurve"
  has_unit <- length(unit) == 1 && !is.na(unit) && nzchar(unit)
  conc_col <- if (has_unit) paste0("Concentration [", unit, "]") else "Concentration"
  # No calibration samples, or no concentrations (not read in non-target mode)
  if (!any(is_cc) || is.null(util$spiked_conc_pts(se))) {
    return(tibble::tibble(!!conc_col := character(), Total = integer()))
  }
  b <- as.character(cd$batch_id[is_cc])
  tibble::tibble(conc = util$spiked_conc_pts(se)[is_cc],
                 Batch = ifelse(b == "all", "All samples", paste("Batch", b))) |>
    dplyr::count(conc, Batch) |>
    dplyr::mutate(Batch = factor(Batch, levels = unique(Batch[order(stringr::str_rank(Batch, numeric = TRUE))]))) |>
    dplyr::arrange(Batch) |>
    tidyr::pivot_wider(names_from = Batch, values_from = n, values_fill = 0) |>
    dplyr::arrange(conc) |>
    dplyr::mutate(Total = rowSums(dplyr::across(-conc)),
                  conc = format(conc, scientific = FALSE, drop0trailing = TRUE, trim = TRUE)) |>
    dplyr::rename(!!conc_col := conc)
}