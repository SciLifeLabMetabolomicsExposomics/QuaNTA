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

#' Required feature columns of a MS-DIAL output file
#'
#' Exact, case-sensitive names. A character vector of alternatives means one of them.
#' @export
required_feature_columns <- list(
  "Alignment ID",
  "Metabolite name",
  c("Average Mz", "Quant mass"),
  "Average Rt(min)",
  "Comment"
)

#' Check the feature columns of a MS-DIAL output file
#'
#' Only columns whose names match exactly (including upper/lower case) are used; similar columns,
#' e.g. "Comment_NEW" or "comment", are ignored. Stops with an explanation when a required column
#' is missing or present more than once, so that no other column can be used by mistake. Similar
#' but ignored columns are reported as a message.
#'
#' @param msdial_file Path to the MS-DIAL output file
#' @param indices The indices of the feature columns (the 1st section, see
#'   [get_three_section_indices()])
#' @returns The names of the feature columns, invisibly
#' @export
check_feature_columns <- function(msdial_file, indices) {
  header <- strsplit(readLines(msdial_file, n = 5L, warn = FALSE)[5L], "\t", fixed = TRUE)[[1L]]
  header <- gsub('^"|"$', "", trimws(header[indices], whitespace = "[\r\n]"))
  similar_to <- function(name) {
    header[grepl(tolower(name), tolower(header), fixed = TRUE) & !header %in% unlist(required_feature_columns)]
  }
  quote <- function(x) paste0('"', x, '"', collapse = ", ")
  problems <- character()
  for (req in required_feature_columns) {
    n_found <- vapply(req, function(r) sum(header == r), integer(1))
    alt <- if (length(req) > 1) paste0(" (or ", quote(req[-1]), ")") else ""
    if (sum(n_found) == 0) {
      similar <- unique(unlist(lapply(req, similar_to)))
      problems <- c(problems, paste0(
        "Missing column ", quote(req[1]), alt, ".",
        if (length(similar) > 0) paste0(" Similar column(s) found but not used: ", quote(similar), ".") else ""
      ))
    } else if (any(n_found > 1)) {
      problems <- c(problems, paste0("Column ", quote(req[n_found > 1]), " is present ", max(n_found),
                                     " times; it must be present exactly once."))
    } else if (sum(n_found > 0) > 1) {
      problems <- c(problems, paste0("Both ", quote(req[n_found > 0]), " are present; keep only one of them."))
    }
  }
  if (length(problems) > 0) {
    stop("The feature columns of the MS-DIAL file are not as expected:\n  - ",
         paste(problems, collapse = "\n  - "),
         "\nColumn names must match exactly, including upper/lower case.", call. = FALSE)
  }
  ignored <- unique(unlist(lapply(unlist(required_feature_columns), similar_to)))
  if (length(ignored) > 0) {
    message("Note: column(s) ", quote(ignored), " are similar to required columns but not used ",
            "(only exact names are used: ", quote(unlist(required_feature_columns)), ").")
  }
  invisible(header)
}

#' Feature columns similar to a required column but not used (not the exact name), e.g.
#' "Comment2" or "comment" for "Comment", shown to the user before processing
#'
#' @inheritParams check_feature_columns
#' @returns A tibble of `column` (the name in the file) and `looks_like` (the required column it
#'   resembles: its name contains that name, ignoring upper/lower case)
#' @export
similar_feature_columns <- function(msdial_file, indices) {
  header <- strsplit(readLines(msdial_file, n = 5L, warn = FALSE)[5L], "\t", fixed = TRUE)[[1L]]
  header <- gsub('^"|"$', "", trimws(header[indices], whitespace = "[\r\n]"))
  req <- unlist(required_feature_columns)
  cand <- header[nzchar(header) & !header %in% req]
  looks <- vapply(cand, function(h) {
    hit <- req[vapply(req, function(r) grepl(tolower(r), tolower(h), fixed = TRUE), logical(1))]
    if (length(hit) == 0) NA_character_ else hit[1]
  }, character(1), USE.NAMES = FALSE)
  tibble::tibble(column = cand, looks_like = looks)[!is.na(looks), ]
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

#' Maximum number of decimals of concentrations and peak areas in the exported tables
.max_decimals <- 3L
#' Decimals of retention times and m/z values in the exported tables, by column name
.rt_columns <- c("Average Rt(min)", "Target IS (Rt)")
.rt_decimals <- 2L
.mz_columns <- "Average Mz"
.mz_decimals <- 5L

#' Round the retention times and m/z values of a table (columns named as in MS-DIAL files)
.round_rt_mz <- function(df) {
  for (cc in intersect(.rt_columns, names(df))) {
    if (is.numeric(df[[cc]])) df[[cc]] <- round(df[[cc]], .rt_decimals)
  }
  for (cc in intersect(.mz_columns, names(df))) {
    if (is.numeric(df[[cc]])) df[[cc]] <- round(df[[cc]], .mz_decimals)
  }
  df
}

#' Find the number of decimal places for rounding
#'
#' The number of decimal places is determined by the median of the log10 of the absolute
#' values (4 significant digits at the median), at most `.max_decimals`.
.find_rounding_decimal_places <- function(x) {
  x <- abs(x)
  x[x == 0] <- NA
  logx <- log10(x)
  m <- stats::median(logx, na.rm = TRUE)
  if (is.na(m)) return(.max_decimals)
  min(.max_decimals, max(0, -floor(m) + 4))
}

#' Columns of text that hold numbers as numbers
#'
#' MS-DIAL writes "null" for missing values, so that columns such as "Reference RT" are read as
#' text and would be stored as text in Excel. A text column is converted when all its values,
#' apart from missing ones ("", "null", "NA", "NaN", "N/A"), are numbers.
#'
#' @param df A data frame
#' @returns `df` with the converted columns
.numbers_as_numeric <- function(df) {
  df[] <- lapply(df, function(x) {
    if (!is.character(x)) return(x)
    y <- trimws(x)
    y[y %in% c("", "null", "NA", "NaN", "N/A")] <- NA_character_
    num <- suppressWarnings(as.numeric(y))
    if (any(!is.na(y)) && all(is.na(y) | !is.na(num))) num else x
  })
  df
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
  if ("blank_source" %in% names(SumExp::col_df(sumexp))) {  # Blank average subtracted
    s_rows <- s_rows |>
      dplyr::mutate("Blank subtracted" = SumExp::col_df(sumexp)[["blank_source"]])
  }
  if ("calibration" %in% names(SumExp::col_df(sumexp))) {  # Calibration used for the batch
    s_rows <- s_rows |>
      dplyr::mutate("Calibration" = SumExp::col_df(sumexp)[["calibration"]])
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

  # Rows of numbers (e.g. injection order, batch ID) as numbers, not text
  for (ii in seq_len(nrow(sinfo))) {
    row_num <- .numbers_as_numeric(data.frame(x = sinfo[ii, ]))$x
    if (!is.numeric(row_num)) next
    .write_data_wb_sheet(
      x = as.data.frame(t(row_num)),
      startCol = ncol(feat_tbl) + 1L,
      startRow = ii,
      colNames = FALSE
    )
  }

  # Write feature data table; feature information with numbers as numbers
  df_x <- .append_matrix_with_feature_table(sumexp, mat_id, feat_tbl) |>
    .numbers_as_numeric() |>
    .round_rt_mz()
  .write_data_wb_sheet(
    x = df_x,
    startRow = nrow(sinfo) + 1,
    colNames = TRUE
  )
  # Concentrations without a value below the LOD / LLOQ: "n.d." / "n.q." (text) in their cells,
  # written per run of neighbouring cells of a chemical (the other cells stay numbers)
  if (mat_id == "conc") {
    flag <- .llox_flag_text(sumexp)
    i_row <- match(SumExp::row_df(sumexp)$alignment_id, df_x$`Alignment ID`) + nrow(sinfo) + 1L
    for (r in which(rowSums(!is.na(flag)) > 0)) {
      js <- which(!is.na(flag[r, ]))
      for (run in split(js, cumsum(c(1L, diff(js) != 1L)))) {
        .write_data_wb_sheet(
          x = as.data.frame(t(unname(flag[r, run]))),
          startCol = ncol(feat_tbl) + run[1L],
          startRow = i_row[r],
          colNames = FALSE
        )
      }
    }
  }
  # R2 always with 3 decimals (numbers formatted "0.000", e.g. 0.950)
  i_r2 <- which(names(df_x) %in% c("R2", "Average R2"))
  if (length(i_r2) > 0 && nrow(df_x) > 0) {
    openxlsx::addStyle(wb, sheetName, openxlsx::createStyle(numFmt = "0.000"),
                       rows = nrow(sinfo) + 1 + seq_len(nrow(df_x)), cols = i_r2,
                       gridExpand = TRUE, stack = TRUE)
  }
  return(wb)
}

#' Remove calibration curve samples
.remove_calcurve_samples <- function(sumexp) {
  sumexp[, !util$is_calcurve_sample(sumexp)]
}

#' Batch IDs as sortable numbers ("2" before "10")
.batch_rank <- function(sumexp) {
  b <- as.character(SumExp::col_df(sumexp)[["batch_id"]])
  match(b, stringr::str_sort(unique(b), numeric = TRUE))
}

#' Injection order as numbers
.injection_order <- function(sumexp) {
  suppressWarnings(as.numeric(as.character(SumExp::col_df(sumexp)[["injection_order"]])))
}

#' Reorder the samples as QCs first (leftmost columns), then the study samples and the blanks
#' last (rightmost columns); within each, by batch and injection order
.reorder_samples_as_qc_first <- function(sumexp) {
  cat <- util$ctrl_smpl_cat(sumexp)
  rank_cat <- ifelse(cat == "QC", 1L, ifelse(cat == "Blank", 3L, 2L))
  ord <- order(rank_cat, .batch_rank(sumexp), .injection_order(sumexp))
  sumexp[, ord]
}

#' Reorder the samples by batch and injection order (the order of the measurements)
.reorder_samples_by_injection <- function(sumexp) {
  sumexp[, order(.batch_rank(sumexp), .injection_order(sumexp))]
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
  # Study samples only (no QCs, blanks or calibration samples)
  sumexp <- util$extract_ctrl_smpl_cat(sumexp, "")
  # Information about the chemicals
  chemicals <- SumExp::row_df(sumexp) |>
    tibble::as_tibble(rownames = "chem_id") |>
    dplyr::rename(chem_name = "feature_name")
  # Concentration
  mat <- sumexp[["conc"]]
  # Samples with a concentration, or "n.d." / "n.q." instead of one
  has_result <- !is.na(mat)
  if ("conc_flag" %in% names(sumexp)) has_result <- has_result | !is.na(sumexp[["conc_flag"]])
  # Measured values at or above the LLOQ, for the min/median/mean/max
  mat_ge_lloq <- measured_conc(sumexp)
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
      n_samples = rowSums(has_result),
    ) |>
    dplyr::mutate(
      median = apply(mat_ge_lloq, 1, .skip_all_na, fun = stats::median),
      mean = apply(mat_ge_lloq, 1, .skip_all_na, fun = mean),
      min = apply(mat_ge_lloq, 1, .skip_all_na, fun = min),
      max = apply(mat_ge_lloq, 1, .skip_all_na, fun = max),
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

#' Measured concentrations of the study samples, for the statistics (min, median, mean, max)
#'
#' Values at or above the LLOQ that were not replaced by the LOD/LLOQ rules (unchanged from
#' `conc0`; e.g. a value set to the LLOQ is left out); QCs, blanks and calibration samples are
#' left out.
#'
#' @param sumexp A [`SumExp::SumExp`] object with the `conc` (and `conc0`) matrices and the `lloq`
#'   per chemical
#' @returns A matrix of the study samples: the measured values, otherwise `NA`
#' @export
measured_conc <- function(sumexp) {
  sumexp <- util$extract_ctrl_smpl_cat(sumexp, "")
  mat <- sumexp[["conc"]]
  measured <- if ("conc0" %in% names(sumexp)) mat == sumexp[["conc0"]] else TRUE
  ifelse(mat >= SumExp::row_df(sumexp)$lloq & measured, mat, NA_real_)
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
  tbl <- dplyr::bind_rows(chem_col_lst, .id = "Batch")
  # Data processed by older versions have no `src_calcurve` with the concentrations
  if (!"src_calcurve" %in% names(tbl)) tbl$src_calcurve <- NA_character_
  tbl <- tbl |>
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
      # Calibration curve used per batch, e.g. "1: Batch; 2: Global"
      calib_per_batch = paste0(Batch, ": ", dplyr::coalesce(src_calcurve, "None"), collapse = "; "),
      .by = alignment_id
    ) |>
    dplyr::mutate(
      perc_detf = round(100 * n_det / n_samples, 1),
      n_d_s = paste0("(", n_det, "/", n_samples, ")"),
      n_q_s = paste0("(", n_quant, "/", n_samples, ")"),
      model_r2 = round(model_r2, 3),
    )
  # Average and median over the measured values of all batches together (not of the batches'
  # averages); each batch's values are those of its own calibration
  measured_all <- do.call(cbind, lapply(sumexp_lst, \(se) {
    m <- measured_conc(se)
    rownames(m) <- SumExp::row_df(se)$alignment_id
    m[as.character(tbl$alignment_id), , drop = FALSE]
  }))
  stat <- function(fun) apply(measured_all, 1, \(x) if (all(is.na(x))) NA_real_ else fun(x, na.rm = TRUE))
  tbl$mean_all <- round(stat(mean), .max_decimals)
  tbl$median_all <- round(stat(stats::median), .max_decimals)
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
      "Avg. Conc." = mean_all,
      "Median Conc." = median_all,
      "Average R2" = model_r2,
      "Calibration per batch" = calib_per_batch,
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
      dplyr::across(c(lod, lloq, min, max, mean, median), ~ round(.x, .max_decimals)),
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
      "Median Conc." = median,
      "R2" = model_r2,
      "Model" = best_model,
      "Equation" = eqn,
      "N of points" = n_conc,
    )
  # ULOQ (empty for a quadratic curve without log calibration; none for older processed data)
  if ("uloq" %in% names(tbl)) out <- dplyr::mutate(out, ULOQ = round(tbl$uloq, .max_decimals), .after = "LLOQ")
  if ("src_calcurve" %in% names(tbl)) {   # Calibration curve used: "Batch", "Global" or none
    out[["Calibration"]] <- dplyr::coalesce(tbl$src_calcurve, "None")
  }
  # The notes on the calibration curve in full (icons in the app and the reports)
  out[["Notes"]] <- util$calib_notes_text(util$calib_notes(tbl, batch_view = TRUE))
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
        unlist() |>
        round(.max_decimals)
    )
  return(chem_col)
}

#' Values replaced after calibration by the LOD/LLOQ rules: colour (as in the reports) and meaning
#' per replacement, given as the fraction of the LLOQ. `flag` and `meaning_flag`: the same values
#' when they are not replaced but reported as "n.d." / "n.q." (`--impute_llox` FALSE)
.imputed_styles <- tibble::tribble(
  ~code, ~fill,     ~flag,  ~meaning,                                            ~meaning_flag,
  1/4,   "#FAAFAF", "n.d.", "Not detected (signal below the LOD signal): replaced by LLOQ/4", "n.d.: not detected (signal below the LOD signal), no value",
  1/2,   "#FCEACF", "n.q.", "Detected, not quantified (signal from the LOD signal to below the mean signal of the LLOQ level): replaced by LLOQ/2", "n.q.: not quantified (signal from the LOD signal to below the mean signal of the LLOQ level), no value",
  1,     "#D3EBF2", NA,     "Signal at or above the LLOQ level, concentration below the LLOQ: set to the LLOQ",
                            "Signal at or above the LLOQ level, concentration below the LLOQ: set to the LLOQ",
  # Not replacements: a value above the ULOQ, within a curve not cut at the ULOQ (code
  # `.above_linear`) or extrapolated beyond the curve (code `.extrapolated`)
  -1,    "#E3E0B8", NA,     "Above the ULOQ (the top of the linear range), within the curve (not replaced)",
                            "Above the ULOQ (the top of the linear range), within the curve (not replaced)",
  Inf,   "#A8A05A", NA,     "Above the ULOQ and the highest calibration level of the curve: extrapolated (not replaced)",
                            "Above the ULOQ and the highest calibration level of the curve: extrapolated (not replaced)",
)
.extrapolated <- Inf
.above_linear <- -1

#' Whether the values below the LOD / LLOQ were replaced (`TRUE`, also for data processed by older
#' versions) or reported as "n.d." / "n.q."
.llox_imputed <- function(sumexp) !isFALSE(SumExp::metadata(sumexp)$llox_imputed)

#' Matrix of the replaced values: the fraction of the LLOQ (see `.imputed_styles`), 0 if not
#' replaced. Values reported as "n.d." / "n.q." have the code of the replacement they replace.
#'
#' @param sumexp A [`SumExp::SumExp`] object with the `conc` matrix and the `lloq` per chemical
.imputed_codes <- function(sumexp) {
  conc <- sumexp[["conc"]]
  lloq <- SumExp::row_df(sumexp)$lloq
  code <- matrix(0, nrow(conc), ncol(conc), dimnames = dimnames(conc))
  for (f in setdiff(.imputed_styles$code, c(.extrapolated, .above_linear))) {
    code[!is.na(conc) & !is.na(lloq) & conc == lloq * f] <- f
  }
  # Values above the ULOQ (none without a ULOQ, e.g. a quadratic curve, or for older processed
  # data): within the curve up to its highest level (when it was not cut at the ULOQ), above it
  # extrapolated
  uloq <- SumExp::row_df(sumexp)$uloq
  if (!is.null(uloq)) {
    uloq <- matrix(uloq, nrow(conc), ncol(conc))
    top <- pmax(uloq, matrix(SumExp::row_df(sumexp)$max_c_conc, nrow(conc), ncol(conc)), na.rm = TRUE)
    above <- !is.na(conc) & !is.na(uloq) & conc > uloq
    code[above] <- .above_linear
    code[above & conc > top] <- .extrapolated
  }
  flag <- .llox_flag_text(sumexp)
  for (ii in which(!is.na(.imputed_styles$flag))) {
    code[flag %in% .imputed_styles$flag[ii]] <- .imputed_styles$code[ii]
  }
  labelled::set_label_attribute(code, "Replaced value (fraction of the LLOQ)")
}

#' "n.d." / "n.q." where a concentration has no value because it is below the LOD / LLOQ,
#' otherwise `NA` (a matrix as `conc`)
.llox_flag_text <- function(sumexp) {
  conc <- sumexp[["conc"]]
  out <- matrix(NA_character_, nrow(conc), ncol(conc), dimnames = dimnames(conc))
  if ("conc_flag" %in% names(sumexp)) {
    flag <- sumexp[["conc_flag"]]
    keep <- !is.na(flag) & is.na(conc)
    out[keep] <- as.character(flag[keep])
  }
  out
}

#' Colour the replaced values of a worksheet written by `.write_data_to_new_worksheet()`
#'
#' @param sumexp The data written, with the `imputed` matrix from `.imputed_codes()`
#' @param feat_tbl The feature table written with it
.add_imputed_styles <- function(wb, sheet, sumexp, feat_tbl) {
  code <- sumexp[["imputed"]]
  # Position of the chemicals and samples in the sheet
  df_x <- .append_matrix_with_feature_table(sumexp, "conc", feat_tbl)
  i_row <- match(SumExp::row_df(sumexp)$alignment_id, df_x$`Alignment ID`) +
    nrow(.get_sample_info_rows(sumexp)) + 1L     # "+ 1L" for the sample ID row
  col_offset <- ncol(feat_tbl)
  for (ii in seq_len(nrow(.imputed_styles))) {
    idx <- which(code == .imputed_styles$code[ii], arr.ind = TRUE)
    if (nrow(idx) == 0) next
    openxlsx::addStyle(
      wb = wb, sheet = sheet, style = openxlsx::createStyle(fgFill = .imputed_styles$fill[ii]),
      rows = i_row[idx[, 1]], cols = idx[, 2] + col_offset, gridExpand = FALSE, stack = TRUE
    )
  }
}

#' Add a worksheet that explains the colours and the order of the samples
.add_legend_worksheet <- function(wb, with_calibrants, imputed = TRUE) {
  openxlsx::addWorksheet(wb, "Legend")
  legend <- data.frame(Colour = "", Meaning = if (imputed) .imputed_styles$meaning else .imputed_styles$meaning_flag)
  openxlsx::writeData(wb, "Legend", legend)
  for (ii in seq_len(nrow(.imputed_styles))) {
    openxlsx::addStyle(wb, "Legend", openxlsx::createStyle(fgFill = .imputed_styles$fill[ii]),
                       rows = ii + 1L, cols = 1L)
  }
  notes <- c(
    if (imputed) {
      "Concentrations of the blank-subtracted, normalized data; values replaced by the LOD/LLOQ rules and values above the ULOQ (within the curve or extrapolated) are coloured."
    } else {
      paste("Concentrations of the blank-subtracted, normalized data. Values below the LOD / LLOQ are not replaced:",
            "they are reported as n.d. / n.q. (text, no value) and left out of the averages and medians. Values above the ULOQ (within the curve or extrapolated) are coloured.")
    },
    "Blank samples: concentrations of their normalized data WITHOUT blank subtraction (\"Blank subtracted\" row: Not subtracted).",
    paste("ULOQ: the top of the linear range of a linear curve (back-calculated levels within the tolerance), or",
          "with log calibration by SCALiR; empty for a quadratic curve (no ULOQ). With 'Enforce linear range',",
          "the curves were cut at the ULOQ."),
    if (with_calibrants) {
      "All samples, including the calibration samples, by batch and injection order."
    } else {
      "QC samples first (left), then the study samples and the blanks last (right); by batch and injection order within each."
    },
    paste0("Concentrations and peak areas have at most ", .max_decimals, " decimals, retention times ",
           .rt_decimals, " and m/z values ", .mz_decimals, ".")
  )
  openxlsx::writeData(wb, "Legend", data.frame(Notes = notes), startRow = nrow(legend) + 3L)
  openxlsx::setColWidths(wb, "Legend", cols = 1:2, widths = c(10, 100))
}

#' The concentrations of the samples together with those of the calibration samples and blanks
#'
#' @param concn The quantified samples of one batch, with the `conc` matrix
#' @param calcurve,blank `NULL`, or the calibration samples / blank samples of the batch, with the
#'   `conc` matrix
#' @returns A [`SumExp::SumExp`] object with the `conc` matrix of all these samples and the chemical
#'   information (`row_df`) and metadata of `concn`
.combine_samples <- function(concn, calcurve = NULL, blank = NULL) {
  # The same sample information in all batches (needed to merge them)
  cols <- intersect(c("sample_name", "Class", "sample_type", ".ctrl_cat", "injection_order",
                      "batch_id", "org_batch_id", "blank_source", "calibration"),
                    names(SumExp::col_df(concn)))
  lab <- labelled::get_label_attribute(concn[["conc"]])
  part <- function(se) {
    cd <- as.data.frame(SumExp::col_df(se))
    for (cc in setdiff(cols, names(cd))) cd[[cc]] <- NA
    cd <- cd[, cols, drop = FALSE]
    cd[] <- lapply(cd, \(x) if (is.factor(x)) as.character(x) else x)
    labelled::var_label(cd) <- NULL
    conc <- se[["conc"]][rownames(concn), , drop = FALSE]
    # "n.d." / "n.q." of the values below the LOD / LLOQ (none for older processed data)
    flag <- .llox_flag_text(se)[rownames(concn), , drop = FALSE]
    SumExp::SumExp(
      conc = labelled::set_label_attribute(conc, lab), conc_flag = flag,
      col_df = cd, row_df = SumExp::row_df(concn), metadata = SumExp::metadata(concn)
    )
  }
  parts <- list(part(concn))
  if (!is.null(calcurve) && "conc" %in% names(calcurve)) {
    # Only the calibration samples of the batch (a batch without its own holds all of them)
    batch_of <- function(se) as.character(SumExp::col_df(se)[["batch_id"]])
    calcurve <- calcurve[, batch_of(calcurve) %in% batch_of(concn)]
    if (ncol(calcurve) > 0) parts <- c(list(part(calcurve)), parts)
  }
  if (!is.null(blank) && "conc" %in% names(blank) && ncol(blank) > 0) {
    parts <- c(parts, list(part(blank)))
  }
  out <- do.call(cbind, parts)
  SumExp::metadata(out) <- SumExp::metadata(concn)
  out
}

#' Write one table for one SumExp object to a worksheet
#'
#' @param sumexp The quantified samples, for the summary of the chemicals
#' @param data_se The data to write (the same chemicals), with the `imputed` matrix
.write_one_sumexp_to_new_worksheet <- function(wb, sheetName, sumexp, is_closest_norm, data_se) {
  # Drop all features that are not to be exported per batch
  to_export <- SumExp::row_df(sumexp)$to_export
  se <- sumexp[to_export, ]
  data_se <- data_se[to_export, ]

  # Prepare the chemical summary table per batch
  chem_col <- .per_batch_summary_chemical_table(se, is_closest_norm) |>
    .append_slope()

  .write_data_to_new_worksheet(
    wb = wb,
    sheetName = sheetName,
    sumexp = data_se,
    mat_id = "conc",
    feat_tbl = chem_col
  )
  .add_imputed_styles(wb, sheetName, data_se, chem_col)
}

#' Export the concentration table
#'
#' @param sumexp_lst A list of [`SumExp::SumExp`] objects to be exported. The name of each element
#'   should be the batch ID.
#' @param file Path to the output file
#' @param is_closest_norm A logical value indicating whether to include the closest internal
#'   standard information in the exported table.
#' @param calcurve_lst `NULL` for the samples without the calibration samples, in the order QCs,
#'   study samples, blanks. Or a list of the calibration samples of each batch (as `sumexp_lst`),
#'   for all samples including the calibration samples, in the order of injection.
#' @param blank_lst `NULL`, or a list of the quantified blank samples of each batch (as
#'   `sumexp_lst`; `NULL` for a batch without blanks)
#' @export
export_concentration_xlsx <- function(sumexp_lst, file, is_closest_norm = FALSE, calcurve_lst = NULL,
                                      blank_lst = NULL) {

  stopifnot(is.list(sumexp_lst))
  for (ii in seq_along(sumexp_lst)) {
    se <- sumexp_lst[[ii]]
    if (! inherits(se, "SumExp")) {
      stop("Each element of `sumexp_lst` must be a `SumExp` object.",
           "The ", ii, "-th element is a ", class(se), ".")
    }
  }
  with_calibrants <- !is.null(calcurve_lst)
  reorder <- if (with_calibrants) .reorder_samples_by_injection else .reorder_samples_as_qc_first
  # Leave only the features to export in any batch
  in_any <- sapply(sumexp_lst, \(se) SumExp::row_df(se)$to_export)
  in_any <- rowSums(in_any) > 0L
  sumexp_lst <- lapply(sumexp_lst, \(se) se[in_any, ])
  # The data to write per batch, with the replaced values (`imputed`) for the colours
  data_lst <- lapply(stats::setNames(nm = names(sumexp_lst)), function(b) {
    se <- .combine_samples(sumexp_lst[[b]], calcurve_lst[[b]], blank_lst[[b]])
    se[["imputed"]] <- .imputed_codes(se)
    reorder(se)
  })

  wb <- openxlsx::createWorkbook()

  # If only 1 batch / all batches evaluated together
  if (length(sumexp_lst) == 1L) {
    .write_one_sumexp_to_new_worksheet(
      wb = wb,
      sheetName = paste("Batch", names(sumexp_lst)[1L]),
      sumexp = sumexp_lst[[1L]],
      is_closest_norm = is_closest_norm,
      data_se = data_lst[[1L]]
    )
  } else {  # Multiple batches
    # Summary table for all batches
    sum_all_batches <- .summary_chemical_table(sumexp_lst, is_closest_norm)
    # SumExp object with all batches
    merged_se <- .merge_sumexp_objs_with_identical_features(data_lst) |> reorder()
    .write_data_to_new_worksheet(
      wb = wb,
      sheetName = "Full",
      sumexp = merged_se,
      mat_id = "conc",
      feat_tbl = sum_all_batches
    )
    .add_imputed_styles(wb, "Full", merged_se, sum_all_batches)

    # Write per-batch tables
    for (ii in seq_along(sumexp_lst)) {
      .write_one_sumexp_to_new_worksheet(
        wb = wb,
        sheetName = paste("Batch", names(sumexp_lst)[ii]),
        sumexp = sumexp_lst[[ii]],
        is_closest_norm = is_closest_norm,
        data_se = data_lst[[ii]]
      )
    }
  }
  .add_legend_worksheet(wb, with_calibrants, imputed = .llox_imputed(sumexp_lst[[1L]]))
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
}
