# ------------------------------------------------------------------------------------------- #
# Functions to read and write MS-DIAL raw/processed data
# ------------------------------------------------------------------------------------------- #

box::use(
  io = ./check_io_exist,
  util = ./msdial_utils
)


# -----  PROJECT PARAMETERS  ---------------------------------------------------

#' Find the `userin` has the required inputs
#'
#' @param userin A list of user input 
#' @param ... The required parameters in a character vector
#'
#' @returns TRUE if all required inputs are provided
.stop_unless_has_required_inputs <- function(userin, ...) {
  for (p in c(...)) {
    if (is.null(userin[[p]])) stop(paste(p, "is required, but not provided."))
  } 
  invisible(TRUE)
}

#' Get the required input from the user
#'
#' @param ... The required parameters in a character vector
#' @returns A list of user input
#' @export
get_user_input <- function(..., params_file = Sys.getenv("QUANTA_PARAMS_FILE", unset = "params.yml")) {
  if (!file.exists(params_file)) stop("Parameter file not found: ", params_file)
  userin <- yaml::read_yaml(params_file)
  .stop_unless_has_required_inputs(userin, ...)
  userin
}

#' Get the file name of the parsed data
#' 
#' The file name has the same name as the `input_file` in the `user_inputs` with a `suffix`
#' and `.rds` added to it on the directory `intermediate_dir`.
#'
#' @param user_inputs A list of user inputs including `input_file` and `intermediate_dir`.
#' @param suffix A suffix to add to the file name to distinguish between data at different
#'   stages, such as raw and processed data.
#'
#' @returns A text string of the file name
#' @export
get_raw_data_file_name <- function(user_inputs, suffix = "") {
  .stop_unless_has_required_inputs(user_inputs, "input_file", "intermediate_dir")
  file <- user_inputs$input_file
  dir <- io$mkdir_if_not_exist(user_inputs$intermediate_dir)
  # Expected output file name from `read-msdial.R`
  path <- file.path(dir, basename(file)) |> 
    tools::file_path_sans_ext(compression = TRUE)
  paste0(path, suffix, ".rds")
}

#' Read the parsed MS-DIAL data
#'
#' @param user_inputs A list of user inputs including `input_file` and `intermediate_dir`.
#'
#' @returns A [`SumExp::SumExp`] object
#' @export
read_parsed_msdial_data <- function(user_inputs) {
  sumexp_file <- get_raw_data_file_name(user_inputs, suffix = "")
  stopifnot("Run `read-msdial.R first" = file.exists(sumexp_file))
  # Load the parsed data
  sumexp <- readRDS(sumexp_file)
  # Confirm the input file is the same as the one used in the parsing
  m5_f <- digest::digest(user_inputs$input_file, algo = "md5", file = TRUE)
  m5_se <- SumExp::metadata(sumexp)$file_md5    # Saved by `read-msdial.R`
  if (m5_f != m5_se) {
    stop("The input file is different from the one used in the parsing.",
         "Please re-run `read-msdial.R` first.")
  }
  sumexp
}


# -----  READ MS-DIAL FILES  ---------------------------------------------------

#' Identify three column sections in a MS-DIAL output file
#' 
#' MS-DIAL output files have three sections in columns. Information about "features",
#' "samples", and "group-wise summary" are stored in these sections. The 2nd section is
#' identified by the columns that have no NA values in the second row.
#' 
#' @param msdial_file Path to the MS-DIAL output file
#' 
#' @returns A list of three column indices
#' @export
get_three_section_indices <- function(msdial_file) {
  # Read the second (`File type`) line to identify three sections
  file_type_line <- utils::read.delim(
    msdial_file, header = FALSE, nrow = 5L, na.strings = c("NA", "")
  )[2, ]
  i_2nd <- range(which(! is.na(file_type_line)))    # Not NA is the 2nd column sections
  stopifnot(file_type_line[i_2nd[1L]] == "File type")    # As expected
  list(
    "1st" = 1L:i_2nd[1L],
    "2nd" = (i_2nd[1L] + 1L):i_2nd[2L],         # "+ 1L" for the title column of the rows
    "3rd" = (i_2nd[2L] + 1L):length(file_type_line)
  )
}

#' Fetch sample information from the first 5 lines
#'
#' @param msdial_file Path to the MS-DIAL output file
#' @param indices The indices of the columns that contain sample information. If it not
#'   provided, the 2nd section identified by `get_three_section_indices` will be used.
#' 
#' @returns A data frame with sample information
#' @export
fetch_sample_info <- function(msdial_file, indices) {
  if (missing(indices)) indices <- get_three_section_indices(msdial_file)[["2nd"]]
  # Read sample information lines (first 5 lines)
  sample_lines <- utils::read.delim(
    msdial_file, 
    header = FALSE,
    nrow = 5L,
    na.strings = c("NA", "")
  )
  header <- sample_lines[, indices[1L] - 1L]
  
  # Extract sample information  
  sinfo <- sample_lines[, indices, drop = FALSE] |> 
    t() |>     # It is provided in rows
    as.data.frame()
  colnames(sinfo) <- header          # There was no header in `sinfo` before
  stopifnot(
    "Deviation from the expected names for samples" =
      identical(
        colnames(sinfo)[-5], 
        c("Class", "File type", "Injection order", "Batch ID")
      )
  )
  colnames(sinfo)[5] <- "sample_name"
  # Syntactically valid names in R of sample names as `sample_id`s in row names
  rownames(sinfo) <- make.names(sinfo$sample_name, unique = TRUE)
  sinfo <- sinfo |> 
    dplyr::select(   # Syntactically valid column names in R
      Class,
      sample_type = "File type",
      injection_order = "Injection order",
      batch_id = "Batch ID",
      sample_name,
    )
  # For plots and tables
  sinfo <- sinfo |> 
    labelled::set_variable_labels(  
      Class = "Class",
      sample_type = "Sample Type",
      injection_order = "Injection Order",
      batch_id = "Batch ID",
      sample_name = "Sample ID",
    )
  sinfo
}

#' Fetch data of the selected columns of a MS-DIAL output file
#' 
#' @param msdial_file Path to the MS-DIAL output file
#' @param indices The indices of the selected columns
#' @param skip The number of lines to skip before reading the data. The default is 4. The data
#'   in MS_DIAL files starts from the 5th line.
#' 
#' @returns A tibble 
#' @seealso `get_three_section_indices` `fetch_sample_info`
#' @export
fetch_data_of_columns <- function(msdial_file, indices, skip = 4L) {
  stopifnot(!missing(indices))
  # Count total number of columns in the file
  l1 <- readr::read_tsv(
    msdial_file,
    skip = skip,
    n_max = 1L,
    col_names = FALSE,
    col_types = "c"
  )
  coltypes <- rep("-", ncol(l1))      # Skip the rest columns
  coltypes[indices] <- "?"
  # Read lines of the selected columns
  readr::read_tsv(
    msdial_file, 
    skip = skip,
    col_types = paste0(coltypes, collapse = ""),
    guess_max = Inf,
    name_repair = "unique_quiet"
  )
}


# -----  EXPORT DATA  ----------------------------------------------------------

#' Find the number of decimal places for rounding
#' 
#' The number of decimal places is determined by the median of the log10 of the absolute
#' values. The maximum number of decimal places is 4.
.find_rounding_decimal_places <- function(x) {
  x <- abs(x)
  x[x == 0] <- NA
  logx <- log10(x)
  m <- stats::median(logx, na.rm = TRUE)
  max(0, -floor(m) + 4)
}

#' Get sample information rows
.get_sample_info_rows <- function(sumexp) {
  s_rows <- SumExp::col_df(sumexp) |> 
    dplyr::select(
      "Class" = "Class",
      "Sample Type" = "sample_type",
      "Injection Order" = "injection_order",
      "Batch ID" = "batch_id",
    )
  if ("org_batch_id" %in% names(SumExp::col_df(sumexp))) {  # One batch mode for multiple batches
    s_rows <- s_rows |> 
      dplyr::mutate(
        "Given Batch ID" = SumExp::col_df(sumexp)[["org_batch_id"]],
      )
  }
  return(t(s_rows))  # Horizontal table
}

#' Append exporting table with feature table
.append_matrix_with_feature_table <- function(sumexp, mat_id, feat_tbl) {
  mat_x <- sumexp[[mat_id]]
  df_x <- mat_x |>  
    round(.find_rounding_decimal_places(mat_x)) |>     # Reduce the number of decimals
    tibble::as_tibble()
  # Original sample ID
  colnames(df_x) <- SumExp::col_df(sumexp)$sample_name
  df_x |> 
    dplyr::mutate("Alignment ID" = SumExp::row_df(sumexp)$alignment_id, .before = 1) |> 
    dplyr::right_join(feat_tbl, y = _, by = "Alignment ID")
}

#' Write numeric values, sample info and feature data to a worksheet
#'
#' The sample info rows are written first, followed by the feature and numeric data. The column
#' names of the feature data and the sample IDs of the numeric data are written in between the
#' sample info rows and the feature data like original input file.
#'
#' @param wb An openxlsx workbook object
#' @param sheetName The name of the sheet to write to. A new sheet name.
#' @param sumexp A [`SumExp::SumExp`] object
#' @param mat_id The name of the data in `sumexp` (or assay) to be exported
#' @param feat_tbl A data frame of the feature table to be exported. It should have a column
#'   `Alignment ID` that matches the one in `sumexp`.
#'
#' @returns The workbook object with the data written to the specified sheet
.write_data_to_new_worksheet <- function(wb, sheetName, sumexp, mat_id, feat_tbl) {
  openxlsx::addWorksheet(wb = wb, sheetName = sheetName)
  .write_data_wb_sheet <- function(..., rowNames = FALSE) {
    openxlsx::writeData(wb = wb, sheet = sheetName, ..., rowNames = rowNames)
  }
  # Write sample info rows
  sinfo <- .get_sample_info_rows(sumexp)
  .write_data_wb_sheet(
    x = sinfo,
    startCol = ncol(feat_tbl),
    colNames = FALSE,    # Column names are given in the middle of rows
    rowNames = TRUE
  )

  # Convert to integer format for columns in `sinfo`
  i_to_update <- which(rownames(sinfo) %in% c("Injection Order", "Given Batch ID"))
  for (ii in i_to_update) {
    row_as_int <- as.integer(sinfo[ii, ])

    .write_data_wb_sheet(
      x = as.data.frame(t(row_as_int)),
      startCol = ncol(feat_tbl) + 1L,
      startRow = ii,
      colNames = FALSE
    )
  }

  # Write feature data table
  df_x <- .append_matrix_with_feature_table(sumexp, mat_id, feat_tbl)
  .write_data_wb_sheet(
    x = df_x,
    startRow = nrow(sinfo) + 1,
    colNames = TRUE
  )
  return(wb)
}

#' Remove calibration curve samples
.remove_calcurve_samples <- function(sumexp) {
  sumexp[, !util$is_calcurve_sample(sumexp)]
}

#' Reorder the samples as QC first and blank last
.reorder_samples_as_qc_first <- function(sumexp) {
  batch_ids <- as.character(SumExp::col_df(sumexp)[["batch_id"]])
  is_qc <- util$ctrl_smpl_cat(sumexp) == "QC"
  is_blank <- util$ctrl_smpl_cat(sumexp) == "Blank"
  ord <- order(batch_ids, !is_qc, is_blank)  # TRUE is larger. 
  sumexp[, ord]
}

#' Export data with the feature table to a Excel `xlsx` file
#'
#' @param sumexp_lst A [ㅏSumExp::SumExp`] object or a list of such objects.
#'   Each object in the list corresponds to a different batch
#' @param mat_id The name of the data in `sumexp` (or assay) to be exported
#' @param org_feature_table The original feature table provided in the MS-DIAL output file.
#'   Information unsaved during processing will be copied from this table.
#' @param out_file Path to the output file
#' @param is_closest_norm A logical value indicating whether the data is closest internal standard normalized
#' @export
export_data_with_feature_table_xlsx <- function(sumexp_lst, 
                                                mat_id, 
                                                org_feature_table, 
                                                out_file, 
                                                is_closest_norm = FALSE) {
  stopifnot(inherits(sumexp_lst, "SumExp") || is.list(sumexp_lst))
  
  #' Prepare feature table for export
  prepare_feature_table <- function(sumexp) {
    se_df <- if (is_closest_norm) {
      SumExp::row_df(sumexp) |>
        dplyr::select(`Alignment ID` = alignment_id, `Metabolite name` = feature_name,
                      "Target IS" = closest_istd, "Target IS (Rt)" = closest_istd_rt)
    } else {
      SumExp::row_df(sumexp) |>
        dplyr::select(`Alignment ID` = alignment_id, `Metabolite name` = feature_name)
    }
    feat_tbl <- dplyr::inner_join(org_feature_table, se_df, by = c("Alignment ID", "Metabolite name"))
    stopifnot(identical(feat_tbl$`Alignment ID`, as.vector(se_df$`Alignment ID`))) # as.vector() for rm label
    return(feat_tbl)
  }

  #' Filter and reorder the samples
  filter_and_reorder_samples <- function(sumexp) {
    sumexp |> 
      .remove_calcurve_samples() |> 
      .reorder_samples_as_qc_first()
  }

  # Prepare the data table
  if (inherits(sumexp_lst, "SumExp")) {
    se <- filter_and_reorder_samples(sumexp_lst)
    wb <- openxlsx::createWorkbook()
    wb <- .write_data_to_new_worksheet(
      wb = wb, 
      sheetName = "Data", 
      sumexp = se,
      mat_id = mat_id,
      feat_tbl = prepare_feature_table(se)
    )
    openxlsx::saveWorkbook(wb, file = out_file, overwrite = TRUE)
  } else {  # list of SumExp
    wb <- openxlsx::createWorkbook()
    for (ii in names(sumexp_lst)) {
      se <- filter_and_reorder_samples(sumexp_lst[[ii]])
      wb <- .write_data_to_new_worksheet(
        wb = wb, 
        sheetName = paste("Batch", ii),
        sumexp = se,
        mat_id = mat_id,
        feat_tbl = prepare_feature_table(se)
      )
    }
    openxlsx::saveWorkbook(wb, file = out_file, overwrite = TRUE)
  }
}

#' Create a summary table for the chemicals
#'
#' @param sumexp A [`SumExp::SumExp`] object. 
#'  `SumExp::row_df(sumexp)` should have the columns `feature_name`, `lod`, `lloq`, and
#'  `calcurve_model`.
#'
#' @returns A tibble with the summary of the chemical for non-control samples only. The columns
#'   include: `chem_id`, `chem_name`, `lod`, `lloq`, `n_det`, `median`, `mean`, `min`, `max`, 
#'            `best_model`, `model_r2`, `n_conc`
#'
#'
#' @export
tbl_chemical_summary <- function(sumexp) {
  # Non-control samples only
  sumexp <- util$extract_ctrl_smpl_cat(sumexp, "")
  # Information about the chemicals
  chemicals <- SumExp::row_df(sumexp) |>
    tibble::as_tibble(rownames = "chem_id") |>
    dplyr::rename(chem_name = "feature_name")
  # Concentration
  mat <- sumexp[["conc"]]
  mat_ge_lloq <- ifelse(mat >= chemicals$lloq, mat, NA_real_)
  # Signal matrix
  mat_signal <- sumexp[[util$src_mat_id_for_conc(sumexp)]]
  # No need but to be safe
  stopifnot(exprs = {
    identical(colnames(mat), colnames(mat_signal))
    identical(rownames(mat), rownames(mat_signal))
  })

  .skip_all_na <- function(x, fun) {
    if (all(is.na(x))) {
      return(NA_real_)
    } else {
      return(fun(x, na.rm = TRUE))
    }
  }
  # Summary about the concentration ranges
  conc_summary <- chemicals |>
    dplyr::mutate(
      n_det = rowSums(mat_signal >= lod_signal, na.rm = TRUE),
      n_quant = rowSums(mat >= lloq, na.rm = TRUE),
      n_samples = rowSums(!is.na(mat)),
    ) |>
    dplyr::mutate(
      median = apply(mat_ge_lloq, 1, .skip_all_na, fun = stats::median),
      mean = apply(mat_ge_lloq, 1, .skip_all_na, fun = mean),
      min = apply(mat_ge_lloq, 1, .skip_all_na, fun = min),
      max = apply(mat, 1, .skip_all_na, fun = max),
    )
  
  # Write an equation for the calibration curve model
  get_equation <- function(cc_model) {
    num_f <- function(x) {
      format(x, scientific = TRUE, digits = 3)
    }
    e <- environment(cc_model$best_model)
    assign("num_f", num_f, envir = e)   # `e` is not a child of this environment
    if (grepl("^linear", cc_model$best_model_name)) {
      with(e, paste("y =", num_f(b1), "* x +", num_f(b0)))
    } else {
      with(e, paste("y =", num_f(a), "* x^2 +", num_f(b), "* x +", num_f(cc)))
    }
  }
  # Add summary about the calibration curve models
  m_sum <- lapply(conc_summary$calcurve_model, \(m) {
    out <- if (is.na(m)[1L]) {    # "best_model" if exists
      tibble::tibble(
        best_model = NA_character_,
        model_r2 = NA_real_,
        n_conc = NA_integer_,
        eqn = NA_character_,
      )
    } else {
      tibble::tibble(
        best_model = m$best_model_name,
        model_r2 = m$R2s[[m$best_model_name]],
        n_conc = m$n_conc,
        eqn = get_equation(m),
      )
    }
    stopifnot(nrow(out) == 1)
    out
  }) |>
    dplyr::bind_rows()
  
  cbind(conc_summary, m_sum) |>
    dplyr::select(-calcurve_model) |>
    dplyr::mutate(
      best_model = stringr::str_replace(best_model, "_div_", "/")
    )
}

#' One summary chemical table across batches
.summary_chemical_table <- function(sumexp_lst, is_closest_norm = FALSE) {
  stopifnot(inherits(sumexp_lst, "SumExp") || is.list(sumexp_lst))
  # Summary table for each batch
  chem_col_lst <- lapply(sumexp_lst, \(se) {
    tbl_chemical_summary(se) |> 
      dplyr::mutate(
        unit = SumExp::metadata(se)$concentration_unit,  # Add the unit
      )
  })
  # Summary table for all batches
  tbl <- dplyr::bind_rows(chem_col_lst, .id = "Batch") |>
    dplyr::summarise(
      alignment_id = alignment_id[1L],
      chem_name = chem_name[1L],
      mz = mz[1L],
      .rt = .rt[1L],
      closest_istd = closest_istd[1L],
      closest_istd_rt = closest_istd_rt[1L],
      unit = unit[1L],
      dplyr::across(c(n_det, n_quant, n_samples), ~ sum(.x, na.rm = TRUE)),
      model_r2 = mean(model_r2, na.rm = TRUE),
      .by = alignment_id
    ) |>
    dplyr::mutate(
      perc_detf = round(100 * n_det / n_samples, 1),
      n_d_s = paste0("(", n_det, "/", n_samples, ")"),
      n_q_s = paste0("(", n_quant, "/", n_samples, ")"),
      model_r2 = round(model_r2, 3),
    ) 
  # Tidy up the table
  out <- tbl |>
    dplyr::select(
      "Alignment ID" = alignment_id,
      "Chemical" = chem_name,
      "Average Mz" = mz,
      "Average Rt(min)" = .rt,
      "DF%" = perc_detf,
      "Samples (d/n)" = n_d_s,
      "Quantified (q/n)" = n_q_s,
      "Concentration" = unit,
      "Average R2" = model_r2,
    )
  if (is_closest_norm) {
    out <- dplyr::bind_cols(
      out,
      dplyr::select(tbl, "Target IS" = closest_istd, "Target IS (Rt)" = closest_istd_rt) 
    )
  }
  out
}

#' Per-batch summary chemical table
.per_batch_summary_chemical_table <- function(sumexp, is_closest_norm = FALSE) {
  stopifnot(inherits(sumexp, "SumExp"))
  tbl <- tbl_chemical_summary(sumexp) |> 
    dplyr::mutate(
      unit = SumExp::metadata(sumexp)$concentration_unit,  # Add the unit
    ) |>
    dplyr::mutate(      # Tidy up the table
      perc_detf = round(100 * n_det / n_samples, 1),
      n_d_s = paste0("(", n_det, "/", n_samples, ")"),
      n_q_s = paste0("(", n_quant, "/", n_samples, ")"),
      model_r2 = round(model_r2, 3),
      dplyr::across(c(min, max, mean), ~ round(.x, 4)),
    )
  out <- tbl |> 
    dplyr::select(
      "Alignment ID" = alignment_id,
      "Chemical" = chem_name,
      "Average Mz" = mz,
      "Average Rt(min)" = .rt,
      "DF%" = perc_detf,
      "Samples (d/n)" = n_d_s,
      "Quantified (q/n)" = n_q_s,
      "Concentration" = unit,
      "LOD" = lod,
      "LLOQ" = lloq,
      "Min Conc." = min,
      "Max Conc." = max,
      "Avg. Conc." = mean,
      "R2" = model_r2,
      "Model" = best_model,
      "Equation" = eqn,
      "N of points" = n_conc,
    )
  if (is_closest_norm) {
    out <- dplyr::bind_cols(
      out,
      dplyr::select(tbl, "Target IS" = closest_istd, "Target IS (Rt)" = closest_istd_rt) 
    )
  }
  out
}

#' merge SumExp objects with identical features (rows)
.merge_sumexp_objs_with_identical_features <- function(sumexp_lst) {
  stopifnot(inherits(sumexp_lst, "SumExp") || is.list(sumexp_lst))
  for (ii in seq_along(sumexp_lst)[-1L]) {
    if (! identical(rownames(sumexp_lst[[1L]]), rownames(sumexp_lst[[ii]]))) {
      stop("All `SumExp` objects in `sumexp_lst` must have the same features (rows).",
           "Mismatch found in the ", ii, "-th element.")
    }
  }
  # Prepare
  sumexp_lst <- lapply(sumexp_lst, function(se) {
    # Required by `.append_matrix_with_feature_table`
    SumExp::row_df(se) <- SumExp::row_df(se)[, c("alignment_id"), drop = FALSE]
    methods::validObject(se)
    se
  })
  Reduce(cbind, sumexp_lst)
}

#' Append a column of slope calculated from the equation of the calibration curve model
.append_slope <- function(chem_col) {
  get_slope <- function(eqn, model) {
    split_eqn <- strsplit(eqn, " ")[[1]]
    
    if (grepl("quadratic", model)) { #"y = -1.72e+05 * x^2 + 1.87e+07 * x + 3.35e+06"
      b <- -(as.numeric(split_eqn[7]))
      a <- 2 * (as.numeric(split_eqn[3]))
      slope <- b / a
    } else { #"y = 4.93e+06 * x + -4.07e+05"
      slope <- as.numeric(split_eqn[3])
    }
  }

  chem_col <- chem_col |> 
    dplyr::mutate(
      Slope = purrr::map2(Equation, Model, ~ get_slope(eqn = .x, model = .y)) |>
        unlist()
    )
  return(chem_col)
}

#' Add style to the values that are adjusted to fct times LLOQ
.add_fct_lloq_adjusted_style <- function(wb, sheet, sumexp, fct, col_offset, row_offset, style) {
  lloq <- SumExp::row_df(sumexp)$lloq
  
  for (ii in seq_len(nrow(sumexp))) {
    i_adjusted <- which(sumexp[["conc"]][ii, ] == (lloq[ii] * fct))
    if (length(i_adjusted) > 0) {
      openxlsx::addStyle(
        wb = wb,
        sheet = sheet,
        style = style,
        rows = ii + row_offset,
        cols = i_adjusted + col_offset
      )
    }
  }
}

#' Write one table for one SumExp object to a worksheet
.write_one_sumexp_to_new_worksheet <- function(wb, sheetName, sumexp, is_closest_norm, fct_styles) {
  # Drop all features that are not to be exported per batch
  se <- sumexp[SumExp::row_df(sumexp)$to_export, ]
  
  # Prepare the chemical summary table per batch
  chem_col <- .per_batch_summary_chemical_table(se, is_closest_norm) |>
    .append_slope()

  .write_data_to_new_worksheet(
    wb = wb, 
    sheetName = sheetName, 
    sumexp = se,
    mat_id = "conc",
    feat_tbl = chem_col
  ) 
  
  sinfo <- .get_sample_info_rows(sumexp)
  col_offset <- ncol(chem_col)
  row_offset <- nrow(sinfo) + 1L   # "+ 1L" for the sample ID row

  for (ii in seq_len(nrow(fct_styles))) {
    fct <- fct_styles$fct[ii]
    style <- fct_styles$style[[ii]]
    stopifnot(inherits(style, "Style"))
    .add_fct_lloq_adjusted_style(wb, sheetName, se, fct, col_offset, row_offset, style)
  }
}

#' Export the concentration table
#'
#' @param sumexp_lst A list of [`SumExp::SumExp`] objects to be exported. The name of each element
#'   should be the batch ID.
#' @param file Path to the output file
#' @param is_closest_norm A logical value indicating whether to include the closest internal
#'   standard information in the exported table.
#' @export
export_concentration_xlsx <- function(sumexp_lst, file, is_closest_norm = FALSE) {
  
  stopifnot(is.list(sumexp_lst))
  for (ii in seq_along(sumexp_lst)) {
    se <- sumexp_lst[[ii]]
    if (! inherits(se, "SumExp")) {
      stop("Each element of `sumexp_lst` must be a `SumExp` object.",
           "The ", ii, "-th element is a ", class(se), ".")
    }
  }
  # Leave only the features to export in any batch
  in_any <- sapply(sumexp_lst, \(se) SumExp::row_df(se)$to_export)
  in_any <- rowSums(in_any) > 0L
  sumexp_lst <- lapply(sumexp_lst, \(se) se[in_any, ])
  # Reorder the samples as QC first
  sumexp_lst <- lapply(sumexp_lst, \(se) .reorder_samples_as_qc_first(se))
  
  # Setting up stylization for excel documents
  fct_styles <- tibble::tribble(
    ~fct, ~style,
    1/2,  openxlsx::createStyle(fgFill = "#FCEACF"), # Half of LLOQ
    1/4,  openxlsx::createStyle(fgFill = "#FAAFAF"), # Quarter of LLOQ
    1,    openxlsx::createStyle(fgFill = "#D3EBF2"), # LLOQ
  )

  wb <- openxlsx::createWorkbook()

  # If only 1 batch / all batches evaluated together
  if (length(sumexp_lst) == 1L) {
    .write_one_sumexp_to_new_worksheet(
      wb = wb,
      sheetName = paste("Batch", names(sumexp_lst)[1L]),
      sumexp = sumexp_lst[[1L]],
      is_closest_norm = is_closest_norm,
      fct_styles = fct_styles
    )
    openxlsx::saveWorkbook(wb, file, overwrite = TRUE)

  } else {  # Multiple batches
    # Summary table for all batches
    sum_all_batches <- .summary_chemical_table(sumexp_lst, is_closest_norm)
    # SumExp object with all batches
    merged_se <- .merge_sumexp_objs_with_identical_features(sumexp_lst)
    .write_data_to_new_worksheet(
      wb = wb, 
      sheetName = "Full", 
      sumexp = merged_se, 
      mat_id = "conc", 
      feat_tbl = sum_all_batches
    )
    
    # Write per-batch tables
    for (ii in seq_along(sumexp_lst)) {
      se <- sumexp_lst[[ii]]
      .write_one_sumexp_to_new_worksheet(
        wb = wb,
        sheetName = paste("Batch", names(sumexp_lst)[ii]),
        sumexp = se,
        is_closest_norm = is_closest_norm,
        fct_styles = fct_styles
      )
    }
    openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  }
}
