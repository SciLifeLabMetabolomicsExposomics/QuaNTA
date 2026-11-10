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

#' Text typed by the user, as plain text in the markdown of a report
#'
#' Every punctuation character gets a backslash, so pandoc shows it as it is: without this, LaTeX
#' commands in the text (e.g. `\input{<file>}`) would run when the PDF is made, and HTML would be
#' kept in the HTML report.
#'
#' @param x A character string (`NULL` / `NA`: "")
#' @export
md_text <- function(x) {
  x <- if (length(x) == 0 || is.na(x[1])) "" else as.character(x[1])
  gsub("([[:punct:]])", "\\\\\\1", x)
}
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
      .y_value <- exp(.y_value)    # Return to original scale (log-log fit: ln(signal) on ln(conc))
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

#  -----  CALIBRATION CURVE VIEWER  ----------------------------------------------
# The calibration curve of one compound (app 4.4, internal report 4.4, tutorial) is drawn in the
# browser by `www/calcurve_viewer.js`, from the data prepared here: one "view" per curve (a batch,
# the global curve, or the calibration points of all batches), each with the calibration points and
# the samples of all compounds, so that switching compounds, curves and modes needs no server.

#' The model of calibration curves as text, e.g. "linear-1_div_x2" -> "Linear, weight 1/x²"
#'
#' @param name Model names (`best_model_name` of the fitted curves; `NA`: none)
#' @returns A character vector
#' @export
calib_model_label <- function(name) {
  vapply(name, function(m) {
    if (is.na(m)) return(NA_character_)
    p <- strsplit(sub("_div_", "/", m), "-", fixed = TRUE)[[1]]
    w <- if (length(p) > 1) sub("x2$", "x²", p[2]) else NA_character_
    paste0(toupper(substr(p[1], 1, 1)), substring(p[1], 2), if (!is.na(w)) paste0(", weight ", w) else "")
  }, "", USE.NAMES = FALSE)
}

#' The roles of the calibration points (inspection mode of the viewer) and the kinds of the sample
#' concentrations (as [imputed_kind()], and without a value), in the order of the viewer (a
#' function: `imputed_colors` is defined further down)
.calib_point_types <- c("Within limits", "Above the ULOQ", "LOD = LLOQ", "Out-of-range", "Cal0", "LOD")
.calib_conc_kinds <- function() c(names(imputed_colors), "n.d. (no value)", "n.q. (no value)")

#' The compounds of a view: limits, model and notes, one row per compound (see [calib_view()])
#'
#' @param rd A `row_df` of calibrated data (`calcurve_model`, limits, `src_calcurve`, ...)
#' @param batch_view As in [calib_issue_text()]: the rows of a batch (blue title when the batch
#'   uses the global curve) or of the global curve itself
#' @noRd
.calib_view_rd <- function(rd, batch_view = TRUE) {
  n <- nrow(rd)
  col <- function(nm, default = NA) if (is.null(rd[[nm]])) rep(default, n) else rd[[nm]]
  models <- col("calcurve_model", list(NULL))
  has_m <- vapply(models, function(m) is.list(m) && !is.null(m$best_model_name), logical(1))
  valid <- col("has_proper_range", FALSE) %in% TRUE
  kept <- col("kept_calcurve", FALSE) %in% TRUE
  src <- as.character(col("src_calcurve", NA))
  data.frame(
    feature_id = rownames(rd), lod = col("lod"), lloq = col("lloq"),
    uloq = ifelse(valid, col("uloq"), NA), min_c_conc = col("min_c_conc"), max_c_conc = col("max_c_conc"),
    valid = valid, kept = kept, src = src,
    model = ifelse(has_m, calib_model_label(vapply(models, \(m) if (is.list(m)) m$best_model_name %||% NA_character_ else NA_character_, "")), NA),
    r2 = vapply(models, \(m) if (is.list(m) && !is.null(m$best_model_name)) unname(m$R2s[[m$best_model_name]]) else NA_real_, 0),
    coef = I(lapply(models, \(m) if (is.list(m) && !is.null(m$inv_model)) unname(stats::coef(m$inv_model)) else NULL)),
    why = sub("^Not valid: ", "", calib_issue_text(rd, batch_view = batch_view)),
    nq = calib_range_note(rd),
    fill = dplyr::case_when(!valid ~ "lightcoral", kept ~ "#FFD9A0", batch_view & src %in% "Global" ~ "lightblue",
                            TRUE ~ "lightgray"),
    stringsAsFactors = FALSE
  )
}

#' The information of samples for the viewer (hover texts, colours by class or QC)
#' @noRd
.calib_sample_info <- function(col_df, is_qc) {
  data.frame(name = col_df$sample_name, cls = as.character(col_df$Class), qc = is_qc,
             batch = as.character(col_df$batch_id), inj = col_df$injection_order, stringsAsFactors = FALSE)
}

#' One curve of the calibration curve viewer (a view): its compounds, calibration points and samples
#'
#' Made by [calib_view_of_batch()], [calib_view_all_batches()], [calib_view_of_global()] or
#' [calib_view_removed()]; given to [calib_viewer_data()].
#'
#' @param rd The compounds, one row each (see `.calib_view_rd()`)
#' @param cal The calibration points: `sig` (a matrix, compounds x points), `used` (a logical
#'   matrix: in the curve), `conc` (the spiked concentrations) and `info` (a data frame per point:
#'   `name`, `batch`, `inj`)
#' @param smp The QCs and study samples, or `NULL`: `conc` (reported), `conc0` (calculated), `flag`
#'   ("n.d." / "n.q.", or `NA`) and `sig` (matrices, compounds x samples), and `info` (a data frame
#'   per sample: `name`, `cls`, `qc`, `batch`, `inj`)
#' @returns A list of `rd`, `cal` and `smp`
#' @export
calib_view <- function(rd, cal, smp = NULL) {
  stopifnot(nrow(cal$sig) == nrow(rd), ncol(cal$sig) == length(cal$conc), is.null(smp) || nrow(smp$sig) == nrow(rd))
  list(rd = rd, cal = cal, smp = smp)
}

#' The view of one batch: the curve each compound uses there (the batch's own or the global one),
#' the batch's calibration points, and its QCs and study samples
#'
#' @param x The calibration of the batch: a list of `calcurve` and `concn` ([`SumExp::SumExp`]
#'   objects, as in the processed data `proc.rds`)
#' @param mat_blk The ID of the blank-subtracted matrix calibrated (e.g. "loess_norm_blk")
#' @returns A view ([calib_view()])
#' @export
calib_view_of_batch <- function(x, mat_blk) {
  mat_cal <- util$mat_id_in_calibration(mat_blk)
  cs <- x$concn
  cc <- x$calcurve[rownames(cs), ]
  ccd <- SumExp::col_df(cc)
  calib_view(
    rd = .calib_view_rd(SumExp::row_df(cs)),
    cal = list(sig = cc[[mat_blk]], used = !is.na(cc[[mat_cal]]), conc = util$spiked_conc_pts(cc),
               info = data.frame(name = ccd$sample_name, batch = as.character(ccd$batch_id), inj = ccd$injection_order)),
    smp = list(conc = cs[["conc"]], conc0 = cs[["conc0"]],
               flag = if ("conc_flag" %in% names(cs)) cs[["conc_flag"]] else NULL, sig = cs[[mat_cal]],
               info = .calib_sample_info(SumExp::col_df(cs), util$ctrl_smpl_cat(cs) == "QC"))
  )
}

#' The view of the calibration points of all batches, for the compounds of a batch that use the
#' global curve (fitted on all of them): the global limits, model and samples of the batch
#'
#' @param lst The calibration of all batches (a list per batch, as `x` of [calib_view_of_batch()])
#' @param batch The batch
#' @inheritParams calib_view_of_batch
#' @returns A view ([calib_view()]), `NULL` when no compound of the batch uses the global curve
#' @export
calib_view_all_batches <- function(lst, batch, mat_blk) {
  v <- calib_view_of_batch(lst[[batch]], mat_blk)
  keep <- v$rd$src %in% "Global"
  if (!any(keep) || length(lst) < 2) return(NULL)
  ids <- v$rd$feature_id
  # The calibration samples of all batches, each once (a batch without its own holds the global ones)
  seen <- character(0)
  parts <- list()
  for (b in names(lst)) {
    cc <- lst[[b]]$calcurve
    new <- !colnames(cc) %in% seen
    if (!any(new)) next
    seen <- c(seen, colnames(cc)[new])
    ccd <- SumExp::col_df(cc)[new, , drop = FALSE]
    sig <- cc[[mat_blk]][match(ids, rownames(cc)), new, drop = FALSE]
    parts[[b]] <- list(sig = sig, conc = util$spiked_conc_pts(cc)[new],
                       info = data.frame(name = ccd$sample_name, batch = as.character(ccd$batch_id), inj = ccd$injection_order))
  }
  sig <- do.call(cbind, lapply(parts, `[[`, "sig"))
  conc <- unlist(lapply(parts, `[[`, "conc"), use.names = FALSE)
  # In the curve: within the range of the global curve (as the fit of processing)
  used <- !is.na(sig) & outer(v$rd$min_c_conc, conc, `<=`) & outer(v$rd$max_c_conc, conc, `>=`)
  used[is.na(used)] <- FALSE
  v$cal <- list(sig = sig, used = used, conc = conc, info = do.call(rbind, lapply(parts, `[[`, "info")))
  sub_rows <- function(l) lapply(l, function(m) if (is.matrix(m)) m[keep, , drop = FALSE] else m)
  calib_view(v$rd[keep, , drop = FALSE], sub_rows(v$cal), sub_rows(v$smp))
}

#' The view of the global curve (all batches together), from its compact copy in the processed
#' data (`to_report[["global calibration"]][[mat_blk]]`, see `compact_global_calibration()` in
#' process.R)
#'
#' @param gcal The compact global calibration
#' @param samples A data frame of all samples, row names = sample IDs (`sample_name`, `Class`,
#'   `batch_id`, `injection_order`, and `.ctrl_cat`)
#' @returns A view ([calib_view()])
#' @export
calib_view_of_global <- function(gcal, samples) {
  L <- as.data.frame(gcal$limits)
  rownames(L) <- L$feature_id
  ids <- L$feature_id
  # The fitted curve as in a `row_df` (model name, R², coefficients)
  rd <- L
  rd$calcurve_model <- lapply(seq_len(nrow(L)), function(i) {
    if (is.na(L$model[i])) return(NA)
    list(best_model_name = L$model[i], R2s = stats::setNames(L$r2[i], L$model[i]),
         inv_model = structure(list(coefficients = L$coef[[i]]), class = "lm"))
  })
  rd$min_c_conc <- L$lloq   # The curve starts at the LLOQ
  rd$src_calcurve <- ifelse(L$has_proper_range %in% TRUE, "Global", NA)
  pts <- colnames(gcal$cal_signal)
  sm <- colnames(gcal$conc0)
  info <- function(id) samples[id, , drop = FALSE]
  si <- info(sm)
  calib_view(
    rd = .calib_view_rd(rd, batch_view = FALSE),
    cal = list(sig = gcal$cal_signal[ids, , drop = FALSE], used = gcal$cal_used[ids, , drop = FALSE],
               conc = unname(gcal$cal_conc[pts]),
               info = data.frame(name = info(pts)$sample_name, batch = as.character(info(pts)$batch_id),
                                 inj = info(pts)$injection_order)),
    smp = list(conc = (gcal$conc %||% gcal$conc0)[ids, , drop = FALSE], conc0 = gcal$conc0[ids, , drop = FALSE],
               flag = if (is.null(gcal$conc_flag)) NULL else gcal$conc_flag[ids, , drop = FALSE],
               sig = gcal$signal[ids, , drop = FALSE], info = .calib_sample_info(si, si$.ctrl_cat %in% "QC"))
  )
}

#' The view of compounds removed before calibration (blank filter, QC RSD% filter): their
#' calibration points only, without a curve
#'
#' @param se The normalized data with the calibration samples (`to_report[["normalized"]]`)
#' @param mat_id The matrix of the signal (e.g. "loess_norm")
#' @param ids,reasons The compounds and why they were removed
#' @param batch The batch of the calibration points (`NULL`: all batches)
#' @returns A view ([calib_view()]), `NULL` without compounds
#' @export
calib_view_removed <- function(se, mat_id, ids, reasons, batch = NULL) {
  ids <- intersect(ids, rownames(se))
  if (length(ids) == 0) return(NULL)
  cd <- SumExp::col_df(se)
  keep <- util$ctrl_smpl_cat(se) == "CalCurve"
  if (!is.null(batch)) keep <- keep & as.character(cd$batch_id) == batch
  sig <- se[[mat_id]][ids, keep, drop = FALSE]
  rd <- data.frame(row.names = ids, has_proper_range = rep(FALSE, length(ids)))
  v <- .calib_view_rd(rd)
  v$why <- paste("Removed before calibration:", reasons[match(ids, names(reasons))])
  v$fill <- "#D9D9D9"   # Grey, as their rows in table 4.2
  calib_view(v, list(sig = sig, used = matrix(FALSE, nrow(sig), ncol(sig)), conc = util$spiked_conc_pts(se)[keep],
                     info = data.frame(name = cd$sample_name[keep], batch = as.character(cd$batch_id[keep]),
                                       inj = cd$injection_order[keep])))
}

#' The curves of one batch for the calibration curve viewer, as the app shows them (app 4.4 and
#' the tutorial): the curve each compound uses in the batch; for compounds on the global curve
#' with several batches, first the calibration points of all batches; the compounds removed
#' before calibration last, with their calibration points only
#'
#' @param lst The calibration of all batches of one data version (`proc[[mat_blk]]`)
#' @param batch The batch
#' @param to_report The processed data (`removed features`, `qc rsd filter`, `normalized`)
#' @param norm_method The data version (e.g. "loess_norm")
#' @returns A list of `compounds` and `views`, for [calib_viewer_data()]
#' @export
calib_views_of_batch <- function(lst, batch, to_report, norm_method) {
  mat_blk <- util$mat_id_of_blank_subtracted(norm_method)
  x <- lst[[batch]]
  rd <- SumExp::row_df(x$calcurve)
  # Removed before calibration (blank filter / QC RSD% filter), as at the end of table 4.2
  rem <- to_report[["removed features"]]
  rem <- if (is.null(rem)) data.frame(feature_id = character(), feature_name = character()) else
    rem[rem$mat_id == norm_method & rem$targeted, ]
  norm <- to_report[["normalized"]]
  removed_v <- if (nrow(rem) > 0 && !is.null(norm) && norm_method %in% names(norm)) {
    rf <- to_report[["qc rsd filter"]]
    why <- ifelse(rem$reason == "QC RSD%", paste0("QC RSD% ", round(rem$qc_rsd, 1), "% > ", rf$max_targeted, "%"),
                  "blank filter")
    calib_view_removed(norm, norm_method, rem$feature_id, stats::setNames(why, rem$feature_id),
                       batch = if (batch %in% as.character(SumExp::col_df(norm)$batch_id)) batch)
  }
  # Why the whole batch uses the global curve (saved by process.R), e.g. too few replicates; a
  # batch with some compounds on the global curve gives their reasons per compound
  reason <- SumExp::metadata(x$concn)$calibration_note
  whole <- identical(as.character(SumExp::col_df(x$concn)$calibration[1]), "Global")
  why_global <- if (whole && length(reason) == 1 && !is.na(reason)) paste0(" Reason for the global curve: ", reason, ".") else ""
  multi <- length(lst) > 1
  views <- list()
  if (multi) {
    views[["All batches (global curve)"]] <- list(
      parts = list(calib_view_all_batches(lst, batch, mat_blk)),
      info = paste0("Concentrations in batch ", batch, " were calculated with the global curve, fitted on the ",
                    "calibration points of all batches shown here.", why_global))
  }
  views[[if (multi) paste0("Batch ", batch, " only") else "All samples"]] <- list(
    parts = list(calib_view_of_batch(x, mat_blk), removed_v),
    info = paste0("Only batch ", batch, "'s calibration points are shown: the concentrations were calculated with the ",
                  "global curve, fitted on the calibration points of all batches.", why_global),
    info_warn = TRUE)
  list(compounds = data.frame(id = c(rownames(x$calcurve), rem$feature_id), name = c(rd$feature_name, rem$feature_name)),
       views = views)
}

#' The data of the calibration curve viewer (`www/calcurve_viewer.js`)
#'
#' @param compounds A data frame of the compounds, in the order of the viewer (`id`, `name`)
#' @param views A named list of views (the names are shown in the viewer's "Curve" choice), each
#'   a list of `parts` ([calib_view()] objects, e.g. a batch and the compounds removed before
#'   calibration) and optionally `info` (a note shown with the compounds that use the global curve;
#'   red with `info_warn = TRUE`)
#' @param log_scale Whether the curves were fitted in log scale (log axes at the start)
#' @param unit The concentration unit
#' @param y_title The title of the y axis
#' @param class_colors The colours of the classes of the study samples (named); the QCs are blue
#' @returns A list, for [calib_viewer_json()]
#' @export
calib_viewer_data <- function(compounds, views, log_scale, unit, y_title, class_colors) {
  r6 <- function(x) { x <- as.numeric(x); x[!is.finite(x)] <- NA; signif(x, 6) }
  part_data <- function(v) {
    rd <- v$rd
    conc <- v$cal$conc
    entries <- lapply(seq_len(nrow(rd)), function(i) {
      r <- rd[i, ]
      within <- conc >= r$min_c_conc & conc <= r$max_c_conc
      type <- dplyr::case_when(
        conc == r$lod & conc == r$lloq & within ~ "LOD = LLOQ",
        within & conc > dplyr::coalesce(r$uloq, Inf) ~ "Above the ULOQ",
        within ~ "Within limits",
        conc == r$lod ~ "LOD",
        conc == 0 ~ "Cal0",
        TRUE ~ "Out-of-range"
      )
      e <- list(fill = r$fill, valid = r$valid, kept = r$kept, src = r$src, why = r$why, nq = r$nq,
                lod = r6(r$lod), lloq = r6(r$lloq), uloq = r6(r$uloq), min = r6(r$min_c_conc), top = r6(r$max_c_conc),
                model = r$model, r2 = r6(r$r2), coef = I(r6(r$coef[[1]])),
                cal = list(y = I(r6(v$cal$sig[i, ])), type = I(match(type, .calib_point_types) - 1L),
                           used = I(as.integer(v$cal$used[i, ] %in% TRUE))))
      if (!is.null(v$smp)) {
        # Reported concentrations; the values replaced by the LOD/LLOQ rules marked (`kind`, with
        # the calculated value `x0`); values without one (n.d. / n.q.) at the calculated value
        x <- v$smp$conc[i, ]
        x0 <- v$smp$conc0[i, ]
        flag <- if (is.null(v$smp$flag)) rep(NA_character_, length(x)) else v$smp$flag[i, ]
        kind <- match(as.character(imputed_kind(x, r$lloq)), .calib_conc_kinds()) - 1L
        cens <- is.na(x) & !is.na(flag)
        kind[cens] <- match(paste0(flag[cens], " (no value)"), .calib_conc_kinds()) - 1L
        x[cens] <- x0[cens]
        e$smp <- list(x = I(r6(x)), y = I(r6(v$smp$sig[i, ])), kind = I(kind), x0 = I(ifelse(kind > 0, r6(x0), NA)))
      }
      e
    })
    names(entries) <- rd$feature_id
    si <- v$smp$info
    list(cal_info = list(x = I(r6(conc)), name = I(v$cal$info$name), batch = I(v$cal$info$batch), inj = I(v$cal$info$inj)),
         smp_info = if (!is.null(si)) list(name = I(si$name), cls = I(si$cls),
                                           grp = I(ifelse(si$qc, "QC", si$cls)), batch = I(si$batch), inj = I(si$inj)),
         c = entries)
  }
  list(
    log_scale = isTRUE(log_scale), unit = unit %||% "", y_title = y_title,
    group_colors = as.list(c(class_colors, QC = okabe_ito[["blue"]])),
    types = I(.calib_point_types), kinds = I(.calib_conc_kinds()),
    compounds = lapply(seq_len(nrow(compounds)), \(k) list(id = compounds$id[k], name = compounds$name[k])),
    views = unname(lapply(names(views), function(nm) {
      v <- views[[nm]]
      list(label = nm, info = v$info %||% "", info_warn = isTRUE(v$info_warn),
           parts = unname(lapply(Filter(Negate(is.null), v$parts), part_data)))
    }))
  )
}

#' The concentration unit of calibrated data: from its metadata, or from the label of the
#' concentrations ("Concentration [ng/mL]")
#'
#' @param concn A [`SumExp::SumExp`] object of calibrated samples (`concn` of the processed data)
#' @returns A character string, `""` when unknown
#' @export
conc_unit <- function(concn) {
  u <- SumExp::metadata(concn)$concentration_unit
  if (length(u) == 1 && !is.na(u) && nzchar(u)) return(u)
  lab <- if ("conc" %in% names(concn)) labelled::get_label_attribute(concn[["conc"]]) else NULL
  if (length(lab) == 1 && grepl("\\[.+\\]", lab)) sub(".*\\[(.+)\\].*", "\\1", lab) else ""
}

#' The data of the calibration curve viewer as JSON (for `QuantaCalViewer()` in the browser)
#'
#' @param data The output of [calib_viewer_data()]
#' @returns A JSON string
#' @export
calib_viewer_json <- function(data) {
  as.character(jsonlite::toJSON(data, auto_unbox = TRUE, na = "null", null = "null", digits = NA))
}

#  -----  FIGURES DRAWN IN THE BROWSER (app 3.5, 4.5, 4.6)  -------------------------
# The data of all internal standards (3.5) or compounds (4.5, 4.6) is sent once; `www/quanta_figs.js`
# draws the one selected in the table, without the server (as the calibration curve viewer).

#' Colours as "#RRGGBB" (R colour names such as "grey25" are not known in the browser)
#' @noRd
.hex_colors <- function(x) {
  if (length(x) == 0) return(list())
  as.list(stats::setNames(grDevices::rgb(t(grDevices::col2rgb(unname(x))), maxColorValue = 255), names(x)))
}
.r6 <- function(x) { x <- as.numeric(x); x[!is.finite(x)] <- NA; signif(x, 6) }

#' The data of figure 3.5 of the app (the peak areas of the internal standard selected in 3.1, by
#' sample group and along the injection order), for `QuantaFigs.isDistribution()`
#'
#' @param se A [`SumExp::SumExp`] object of the internal standards (with `raw`), rows in the order
#'   of table 3.1
#' @param outlier `NULL`, or a logical vector named by sample ID: outlier samples
#' @param failed `NULL`, or a logical vector: failed internal standards (rows of `se`)
#' @param rsd `NULL`, or the RSD% of each IS per QC group and sample class for table 3.4 (a list of
#'   `rsd`, a matrix with the groups in rows and the IS in columns in the order of `se`, and `n`,
#'   as [is_rsd_by_group()])
#' @returns A list, for [calib_viewer_json()]
#' @export
is_distribution_data <- function(se, outlier = NULL, failed = NULL, rsd = NULL) {
  cd <- SumExp::col_df(se)
  grp <- sample_group(cd)
  y <- se[["raw"]]
  out <- if (is.null(outlier)) rep(FALSE, ncol(se)) else unname(outlier[colnames(se)] %in% TRUE)
  list(
    names = I(SumExp::row_df(se)$feature_name),
    failed = I(if (is.null(failed)) rep(FALSE, nrow(se)) else unname(failed) %in% TRUE),
    y = lapply(seq_len(nrow(y)), \(i) I(.r6(y[i, ]))),
    smp = list(name = I(cd$sample_name), cls = I(as.character(cd$Class)), batch = I(as.character(cd$batch_id)),
               inj = I(cd$injection_order), grp = I(grp), out = I(out)),
    colors = .hex_colors(sample_group_colors(grp)),
    # Table 3.4: the groups, their number of samples, and the RSD% of each IS (a column each)
    rsd = if (!is.null(rsd)) list(
      groups = I(rownames(rsd$rsd)), n = I(unname(as.integer(rsd$n))),
      v = lapply(seq_len(ncol(rsd$rsd)), \(j) I(.r6(rsd$rsd[, j])))
    )
  )
}

#' The data of figures 4.5 and 4.6 of the app, for `QuantaFigs.concByClass()` and
#' `QuantaFigs.qcConc()`: the concentrations of every compound in the QCs and study samples of a
#' batch (4.5), and in the QCs of all batches, each with its own calibration (4.6)
#'
#' Each value has a kind (an index of `kinds`, as in [calib_viewer_data()]): measured, replaced
#' by an LOD/LLOQ rule, or without a value ("n.d." / "n.q.").
#'
#' @param lst The calibration of all batches of one data version (`proc[[mat_blk]]`)
#' @param batch The batch of figure 4.5 (`NULL`: no data for 4.5, e.g. figure 4.6 alone in the
#'   internal report and the tutorial)
#' @param class_colors The colours of the classes (named), for 4.5
#' @param qc_limit The limit (%) of the green area of 4.6, see [qc_chart_limit()]
#' @returns A list, for [calib_viewer_json()]
#' @export
conc_figs_data <- function(lst, batch = NULL, class_colors = character(), qc_limit = 30) {
  kinds <- .calib_conc_kinds()
  part <- function(se) {
    conc <- se[["conc"]]
    flag <- if ("conc_flag" %in% names(se)) se[["conc_flag"]] else NULL
    lloq <- SumExp::row_df(se)$lloq
    entries <- lapply(seq_len(nrow(se)), function(i) {
      x <- conc[i, ]
      f <- if (is.null(flag)) rep(NA_character_, length(x)) else as.character(flag[i, ])
      k <- match(as.character(imputed_kind(x, lloq[i])), kinds) - 1L
      cens <- is.na(x) & !is.na(f)
      k[cens] <- match(paste0(f[cens], " (no value)"), kinds) - 1L
      list(y = I(.r6(x)), kind = I(k))
    })
    names(entries) <- rownames(se)
    cd <- SumExp::col_df(se)
    list(info = list(name = I(cd$sample_name), cls = I(as.character(cd$Class)), batch = I(as.character(cd$batch_id)),
                     inj = I(cd$injection_order)),
         c = entries)
  }
  qcs <- lapply(Filter(\(x) !is.null(x$concn), lst), \(x) util$extract_ctrl_smpl_cat(x$concn, "QC"))
  qcs <- Filter(\(se) ncol(se) > 0, qcs)
  qc_groups <- sort(unique(unlist(lapply(qcs, \(se) as.character(SumExp::col_df(se)$Class)))))
  first <- if (!is.null(batch)) lst[[batch]]$concn else Filter(Negate(is.null), lapply(lst, `[[`, "concn"))[[1]]
  unit_label <- labelled::get_label_attribute(first[["conc"]]) %||% "Concentration"
  unit_label <- sub(" \\[\\]$", "", unit_label)   # No unit given: no empty brackets
  rds <- lapply(c(if (!is.null(batch)) list(lst[[batch]]$concn), qcs), SumExp::row_df)
  ids <- unlist(lapply(rds, rownames))
  nms <- unlist(lapply(rds, \(rd) as.character(rd$feature_name)))
  keep <- !duplicated(ids)
  list(
    names = as.list(stats::setNames(nms[keep], ids[keep])),   # Compound ID -> name (plot titles)
    kinds = I(kinds), kind_colors = .hex_colors(imputed_colors), class_colors = .hex_colors(class_colors),
    qc_colors = .hex_colors(hue_colors(qc_groups)), y_title = unit_label, qc_limit = qc_limit,
    samples = if (!is.null(batch)) part(lst[[batch]]$concn),
    qc = unname(lapply(qcs, part))
  )
}

#' The limit (%) of the green area of figure 4.6: the limit of the QC RSD% filter for targeted
#' compounds if that filter was used, otherwise 30
#'
#' @param rf The `qc rsd filter` element of the `to_report` list of `process.R`
#' @returns A number
#' @export
qc_chart_limit <- function(rf) {
  if (isTRUE(rf$enabled) && !isTRUE(rf$skipped) && isTRUE(!is.na(rf$max_targeted))) rf$max_targeted else 30
}

#' The compounds with a value in the QC samples (any batch), from [conc_figs_data()]: the
#' compounds figure 4.6 can show
#'
#' @param data The output of [conc_figs_data()]
#' @returns A data frame of `id` and `name`, sorted by name
#' @export
conc_figs_qc_compounds <- function(data) {
  ids <- unique(unlist(lapply(data$qc, function(p) {
    names(p$c)[vapply(p$c, function(e) any(!is.na(e$y) | !is.na(e$kind)), logical(1))]
  })))
  d <- data.frame(id = ids, name = vapply(ids, \(i) data$names[[i]] %||% i, ""), stringsAsFactors = FALSE)
  d[order(d$name), , drop = FALSE]
}

#' Figure 4.6 of one compound as HTML for a page (the tutorial): `www/quanta_figs.js` (linked),
#' plotly.js and the data
#'
#' @param id The ID of the element
#' @param data The output of [conc_figs_data()] (`batch = NULL` is enough)
#' @param compound The ID of the compound
#' @param script_src The address of `quanta_figs.js`
#' @returns An HTML tag list (with plotly.js as a dependency)
#' @export
qc_conc_html <- function(id, data, compound, script_src) {
  htmltools::attachDependencies(htmltools::tagList(
    htmltools::tags$script(src = script_src),
    htmltools::div(id = id, style = "min-height: 500px;"),
    htmltools::tags$script(htmltools::HTML(sprintf(
      "(function () { var d = %s; function go() { QuantaFigs.qcConc(document.getElementById('%s'), d, %s); }
       if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', go); else go(); })();",
      calib_viewer_json(data), id, jsonlite::toJSON(compound, auto_unbox = TRUE))))
  ), plotly_js_dependency())
}

#' The calibration curve viewer as HTML for a page (internal report, tutorial): the script
#' (`www/calcurve_viewer.js`, given or linked), plotly.js and the data
#'
#' @param id The ID of the element
#' @param data The output of [calib_viewer_data()]
#' @param script The path of `calcurve_viewer.js` to include in the page (`NULL`: linked by
#'   `script_src` instead)
#' @param script_src The address of the script when it is not included
#' @param height The height of the plot in pixels
#' @returns An HTML tag list (with plotly.js as a dependency)
#' @export
calib_viewer_html <- function(id, data, script = NULL, script_src = NULL, height = 520) {
  js <- if (!is.null(script)) htmltools::tags$script(htmltools::HTML(paste(readLines(script, warn = FALSE, encoding = "UTF-8"), collapse = "\n")))
        else htmltools::tags$script(src = script_src)
  htmltools::attachDependencies(htmltools::tagList(
    js,
    htmltools::div(id = id),
    htmltools::tags$script(htmltools::HTML(sprintf(
      "(function () { var d = %s; function go() { QuantaCalViewer(document.getElementById('%s'), d, { nav: true, keys: true, height: %d }); }
       if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', go); else go(); })();",
      calib_viewer_json(data), id, as.integer(height))))
  ), plotly_js_dependency())
}

#' plotly.js of the R package plotly (the version of all plots of the app and the reports), for a
#' page with plots drawn without a plotly widget (the calibration curve viewer)
#'
#' @returns A list of HTML dependencies
#' @export
plotly_js_dependency <- function() {
  # The package adds plotly.js to a plot when it builds it (not to its widget dependencies)
  deps <- plotly::plotly_build(plotly::plot_ly(type = "scatter", mode = "markers"))$dependencies
  Filter(function(d) d$name %in% c("typedarray", "plotly-main"), deps)
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
    # A list: two scales cannot be added to each other, only to a plot
    list(ggplot2::scale_x_log10(), ggplot2::scale_y_log10(labels = label_e))
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

#  -----  INJECTION ORDER  -----------------------------------------------------

#  -----  PCA  -----------------------------------------------------------------

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
  # Part by part (a matrix in blocks of columns), so that large data is not copied as a whole; the
  # breaks come from the range of all values, so the counts are those of all values at once
  blocks <- if (is.matrix(values)) split(seq_len(ncol(values)), ceiling(seq_len(ncol(values)) / 200)) else list(NULL)
  part <- function(b) {
    v <- if (is.null(b)) as.vector(values) else as.vector(values[, b])
    finite <- is.finite(v)
    v <- v[finite]
    list(v = v, pos = log10(v[v > 0]), n_na = sum(!finite))
  }
  n <- n_pos <- n_na <- 0
  r_lin <- r_log <- NULL
  for (b in blocks) {
    p <- part(b)
    n <- n + length(p$v); n_pos <- n_pos + length(p$pos); n_na <- n_na + p$n_na
    if (length(p$v) > 0) r_lin <- range(c(r_lin, p$v))
    if (length(p$pos) > 0) r_log <- range(c(r_log, p$pos))
  }
  count <- function(r, what) {
    if (is.null(r)) return(NULL)
    br <- seq(r[1], r[2], length.out = bins + 1)
    if (diff(range(br)) == 0) br <- c(br[1] - 0.5, br[1] + 0.5)
    out <- NULL
    for (b in blocks) {
      x <- part(b)[[what]]
      if (length(x) == 0) next
      h <- graphics::hist(x, breaks = br, plot = FALSE, include.lowest = TRUE)
      if (is.null(out)) out <- list(breaks = h$breaks, counts = h$counts, mids = h$mids) else out$counts <- out$counts + h$counts
    }
    out
  }
  structure(list(n = n, n_pos = n_pos, n_na = n_na, lin = count(r_lin, "v"), log = count(r_log, "pos")),
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
