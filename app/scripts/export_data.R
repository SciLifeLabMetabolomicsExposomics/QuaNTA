# ------------------------------------------------------------------------------------------- #
# Export the tables of one or more normalization methods, given by
# `--norm_method=<matrix ID>[,<matrix ID>...]` (e.g. `loess_norm`, `closest_norm`, `mvis_norm`,
# `raw`) or `--norm_method=auto` (default: the method with the lowest median QC RSD%, determined
# by `process.R`). Per method:
# 1. Normalized data of overall features including non-targets
# 2. Blank subtracted normalized data of overall features (the blanks, when kept, not subtracted)
# 3. Concentration data of targeted features (blank subtracted)
# 4. What happened to each feature and sample during the processing (from its event log)
#
# The injections in the tables (1-3) and their order (columns):
#   --keep_qc, --keep_blank, --keep_cal, --keep_samples   Keep the QCs / blanks / calibration
#                     standards / study samples (TRUE)
#   --group_by_type   Group the columns by injection type (TRUE), in the order of --group_order
#   --split_samples   With --group_by_type, the study samples as a group per class (TRUE), in the
#                     order of --group_order; otherwise all study samples together ("Samples")
#   --group_by_batch  Group the columns by batch first (FALSE), then by injection order
#   --group_by_class  Within each injection type, group the columns by class (FALSE; e.g. the QC
#                     groups), then by injection order
#   --group_order     The injection types in their order, as a JSON array (default
#                     ["QC","Samples","Blank","CalCurve"]): "QC", "Blank", "CalCurve" and the study
#                     sample groups as "grp:<Class>" ("Samples": all study groups not named)
#   --export_norm     With blank subtraction, also the normalized peak areas without it (FALSE)
#   --export_failed   Concentration tables: also the compounds with a failed calibration curve and
#                     those with no study sample at or above the LLOQ, with the signals of their
#                     calibration points only (no concentrations) (FALSE)
#
# Output file names are based on the input file name and normalization method.
# 1. (base name).(normalized method).norm.xlsx
# 2. (base name).(normalized method).norm_blk.xlsx
# 3. (base name).(normalized method).conc.xlsx
# 4. (base name).(normalized method).history.xlsx
# ------------------------------------------------------------------------------------------- #

# Load packages and project local libraries
options(box.path = Sys.getenv("R_BOX_PATH", "app/"))          # Path to project local libraries
box::use(
  SumExp,           # Light SummarizedExperiment, `[`
  projlib/msdial,     # Handle MS-Dial files
  util = projlib/msdial_utils,        # Utility functions for MS-Dial data
  io = projlib/check_io_exist,
  projlib/history,    # What happened to each feature and sample
)
# Get the input file name provided by the user
user_inputs <- msdial$get_user_input("input_file", "intermediate_dir", "table_dir")

# Input files
FILE <- list(
  i = rlang::list2(
    raw = user_inputs$input_file,     # Copy unsaved feature info into output files
    # Intermediate status of the data
    to_rep = msdial$get_raw_data_file_name(user_inputs, suffix = "to_report"),
  )
)
io$chq_all_files_exist(FILE)
# Load the normalized data
to_report <- readRDS(FILE$i$to_rep)
stopifnot("Processing NOT completed." = to_report[["Completed"]])
mat_ids_to_export <- to_report[["matrix ids to blk subt"]]     # IDs of matrices to export
IS_TARGET_MODE <- !to_report[["is_non_target_mode"]]

if (IS_TARGET_MODE) {
  # Processed data
  FILE$i$proc <- msdial$get_raw_data_file_name(user_inputs, suffix = "proc")
  io$chq_all_files_exist(FILE$i$proc)
}

# Output files
FILE$o <- local({
  # Copy the basename of the input file to the output file names
  # "(base name).(normalized method).conc.xlsx", "().norm.xlsx" and "().norm_blk.xlsx"
  b <- basename(user_inputs$input_file) |>
    tools::file_path_sans_ext()    # Without extension
  ns <- stringr::str_remove(mat_ids_to_export, "_norm")
  paste_file_name <- function(x, p) {
    file.path(user_inputs$table_dir, paste0(b, ".", x, ".", p, ".xlsx"))
  }
  # One table of peak areas: the blank-subtracted one when the data was blank-subtracted (processing
  # option `--blank_subtract`), otherwise the normalized one. With blank subtraction,
  # `--export_norm=TRUE` adds the normalized one (not blank-subtracted)
  export_norm <- any(toupper(sub("^--export_norm=", "", grep("^--export_norm=", commandArgs(TRUE), value = TRUE))) == "TRUE")
  peak_tbl <- if (isFALSE(to_report$params$blank_subtract)) "norm" else c(if (export_norm) "norm", "norm_blk")
  out_data <- if (IS_TARGET_MODE) {
    c(peak_tbl, "conc", "history")
  } else {
    c(peak_tbl, "history")
  }
  lapply(setNames(nm = out_data), \(.x) {
    lapply(ns, paste_file_name, p = .x)
  })
})
io$mkdir_if_not_exist(dirname(unlist(FILE$o)))

# Prepare the original feature table to copy unsaved feature information
i_sec <- msdial$get_three_section_indices(FILE$i$raw)
org_feature_table <- msdial$fetch_data_of_columns(FILE$i$raw, i_sec[[1]])

# Script arguments ----------
args <- commandArgs(trailingOnly = TRUE)
arg <- function(name, default) {
  v <- sub(paste0("^--", name, "="), "", grep(paste0("^--", name, "="), args, value = TRUE))
  if (length(v) == 0 || !nzchar(v[1])) default else v[1]
}
arg_lgl <- function(name, default) {
  v <- toupper(arg(name, if (default) "TRUE" else "FALSE"))
  if (!v %in% c("TRUE", "FALSE")) stop("`--", name, "` must be TRUE or FALSE.", call. = FALSE)
  v == "TRUE"
}

# Normalization methods to export: `--norm_method=<matrix ID>[,...]` or `auto` (default), the
# method with the lowest median QC RSD% determined by `process.R`
norm_methods <- unique(vapply(trimws(strsplit(arg("norm_method", "auto"), ",", fixed = TRUE)[[1]]), function(m) {
  if (m != "auto") return(m)
  best <- to_report[["best normalization"]]
  if (is.null(best)) {   # Processed with an older version; re-run processing to update
    cat("No best normalization method found in the processed data. Using `loess_norm`.\n")
    return("loess_norm")
  }
  cat("Best normalization method (lowest median RSD% of replicates):", best, "\n")
  best
}, character(1), USE.NAMES = FALSE))
not_available <- setdiff(norm_methods, mat_ids_to_export)
if (length(not_available) > 0) {
  stop("Normalization method(s) not available: ", paste0("`", not_available, "`", collapse = ", "),
       ". Available: ", paste(mat_ids_to_export, collapse = ", "), call. = FALSE)
}
# `--max_xlsx_cells=<number>` (given by the app on SciLifeLab Serve): larger tables as CSV only
max_xlsx_cells <- as.numeric(arg("max_xlsx_cells", "Inf"))
# `--formats=xlsx,csv` (default both): the .xlsx files and / or their CSV copies (one per sheet).
# A table too large for Excel is always written as CSV.
formats <- intersect(trimws(strsplit(arg("formats", "xlsx,csv"), ",", fixed = TRUE)[[1]]), c("xlsx", "csv"))
if (length(formats) == 0) formats <- c("xlsx", "csv")
cat("File formats:", paste(formats, collapse = ", "), "\n")
# The injections of the tables and their order
layout <- msdial$export_layout(
  by_type = arg_lgl("group_by_type", TRUE),
  by_batch = arg_lgl("group_by_batch", FALSE),
  order = jsonlite::fromJSON(arg("group_order", '["QC","Samples","Blank","CalCurve"]')),
  keep = c(QC = arg_lgl("keep_qc", TRUE), Blank = arg_lgl("keep_blank", TRUE), CalCurve = arg_lgl("keep_cal", TRUE),
           Samples = arg_lgl("keep_samples", TRUE)),
  split_samples = arg_lgl("split_samples", TRUE),
  by_class = arg_lgl("group_by_class", FALSE)
)
kept <- c(Samples = "study samples", QC = "QCs", Blank = "blanks (in the blank-subtracted tables: not blank-subtracted)",
          CalCurve = "calibration standards")
kept <- kept[names(kept) %in% names(layout$keep)[layout$keep]]
# `--export_failed`: the compounds with a failed curve or without a quantified study sample, with
# the signals of their calibration points only
EXPORT_FAILED <- arg_lgl("export_failed", FALSE)
if (EXPORT_FAILED && IS_TARGET_MODE) {
  cat("Concentration tables: also the compounds with a failed calibration curve or no quantified study sample",
      if (layout$keep[["CalCurve"]]) "(the signals of their calibration points, no concentrations).\n" else
        "(no concentrations; their calibration points need the calibration standards included).\n")
}
cat("Injections in the tables: ", if (length(kept)) paste(kept, collapse = ", ") else "none", ".\n", sep = "")
cat("Column order: ",
    if (layout$by_batch) "by batch, then ",
    if (layout$by_type) paste0("by injection type", if (layout$split_samples) " (study samples per class)", ", then "),
    if (layout$by_class) "by class, then ",
    "by injection order.\n", sep = "")

# Internal standards (IS) and volumetric internal standards (vIS) are not exported
not_std <- \(rd) !(util$is_internal_std(rd$.std_type) | rd$.std_type %in% "vIS")

# Export the tables of one method ----------
export_method <- function(mat_id) {
  ii <- which(mat_ids_to_export == mat_id)
  blk_mat_id <- util$mat_id_of_blank_subtracted(mat_id)
  is_closest_norm <- mat_id == "closest_norm"
  cat("\nExporting", mat_id, "data.\n")

  # Normalized data (without blank subtraction; with it only with `--export_norm=TRUE`)
  if (!is.null(FILE$o[["norm"]])) {
  # Per batch (a sheet each, after the "Full" sheet of all batches)
  se <- msdial$split_by_batch(msdial$read_full_normalized(user_inputs, to_report, mat_id, keep = not_std))
  msdial$export_data_with_feature_table_xlsx(
    sumexp_lst = se,
    mat_id = mat_id,
    org_feature_table = org_feature_table,
    out_file = FILE$o[["norm"]][[ii]],
    is_closest_norm = is_closest_norm,
    max_xlsx_cells = max_xlsx_cells,
    layout = layout
  )
  cat(glue::glue("{mat_id} data has been saved."), "\n")
  rm(se)
  invisible(gc(FALSE))
  }

  # Blank subtracted normalized data; only with blank subtraction. The blanks (when kept) with their
  # normalized values, not blank-subtracted
  if (!is.null(FILE$o[["norm_blk"]])) msdial$export_data_with_feature_table_xlsx(
    sumexp_lst = msdial$split_by_batch(msdial$read_full_blank_subtracted(
      user_inputs, to_report, blk_mat_id, keep = not_std, with_blanks = layout$keep[["Blank"]])),
    mat_id = blk_mat_id,
    org_feature_table = org_feature_table,
    out_file = FILE$o[["norm_blk"]][[ii]],
    is_closest_norm = is_closest_norm,
    max_xlsx_cells = max_xlsx_cells,
    layout = layout
  )
  if (!is.null(FILE$o[["norm_blk"]])) cat(glue::glue("{blk_mat_id} data has been saved."), "\n")
  invisible(gc(FALSE))

  # Concentration values
  with_conc <- IS_TARGET_MODE
  if (with_conc) {
    # The calibrated data of this method only (read after the large tables, as before: less memory
    # at a time than keeping all methods)
    proc_blk <- readRDS(FILE$i$proc)[[blk_mat_id]]
    invisible(gc(FALSE))
    # Concentration has been computed on the blank subtracted data
    lst_proc <- lapply(proc_blk, \(ea) ea[["concn"]])
    # No chemical exported in any batch (e.g. no study sample above any LLOQ): no concentration table
    # (with `--export_failed`, the table of their calibration points is written)
    if (!any(unlist(lapply(lst_proc, \(se) SumExp::row_df(se)$to_export)) %in% TRUE) && !EXPORT_FAILED) {
      cat("No chemical is exported (none has a study sample above its LLOQ; see the processing history):",
          "the concentration table is not written.\n")
      with_conc <- FALSE
    }
  }
  if (with_conc) {
    msdial$export_concentration_xlsx(
      sumexp_lst = lst_proc,
      file = FILE$o[["conc"]][[ii]],
      is_closest_norm = is_closest_norm,
      calcurve_lst = lapply(proc_blk, \(ea) ea[["calcurve"]]),
      # Blank samples, quantified without blank subtraction (NULL for a batch without blanks, and
      # for data processed by older versions)
      blank_lst = lapply(proc_blk, \(ea) ea[["blank"]]),
      layout = layout,
      # The compounds with a failed curve or no quantified study sample: their calibration points
      include_failed = EXPORT_FAILED,
      signal_mat = blk_mat_id
    )
    cat(glue::glue("{mat_id} concentration data has been saved to {basename(FILE$o[['conc']][[ii]])}."), "\n")
  }
  rm(list = intersect(c("proc_blk", "lst_proc"), ls()))
  invisible(gc(FALSE))

  # What happened to each feature and sample
  if (is.null(to_report[["feature history"]])) {   # Processed with an older version
    cat("No processing history in the processed data (re-run the processing to add it).\n")
  } else {
    history$write_history_xlsx(to_report, mat_id, FILE$o[["history"]][[ii]])
    cat(glue::glue("The processing history has been saved to {basename(FILE$o[['history']][[ii]])}."), "\n")
  }

  # CSV copies: one CSV per worksheet, with the same layout as the sheet:
  # "(base name).(method).(norm|norm_blk|conc|history).(sheet).csv" (not the "Legend" sheet)
  xlsx_files <- unlist(lapply(FILE$o, \(x) x[[ii]]))
  for (xlsx in if ("csv" %in% formats) xlsx_files[file.exists(xlsx_files)]) {
    for (sheet in setdiff(openxlsx::getSheetNames(xlsx), "Legend")) {
      tbl <- openxlsx::read.xlsx(xlsx, sheet = sheet, colNames = FALSE,
                                 skipEmptyRows = FALSE, skipEmptyCols = FALSE)
      # R2 always with 3 decimals (as the "0.000" format of the xlsx), below its header cell
      for (hit in which(as.matrix(tbl) %in% c("R2", "Average R2"))) {
        r <- (hit - 1) %% nrow(tbl) + 1
        k <- (hit - 1) %/% nrow(tbl) + 1
        below <- seq_len(nrow(tbl)) > r
        v <- suppressWarnings(as.numeric(tbl[below, k]))
        tbl[below, k] <- ifelse(is.na(v), tbl[below, k], sprintf("%.3f", v))
      }
      csv <- paste0(tools::file_path_sans_ext(xlsx), ".", gsub("[^A-Za-z0-9_-]+", "_", sheet), ".csv")
      utils::write.table(tbl, csv, sep = ",", na = "", row.names = FALSE, col.names = FALSE,
                         qmethod = "double", fileEncoding = "UTF-8")
    }
  }
  if ("csv" %in% formats) cat("CSV copies (one per worksheet) have been saved.\n")
  # CSV only: the .xlsx files are removed (after their CSV copies)
  if (!"xlsx" %in% formats) {
    unlink(xlsx_files[file.exists(xlsx_files)])
    cat("Only CSV files are kept (option).\n")
  }
  invisible(gc(FALSE))
}

# One method after the other (the memory of one at a time)
for (m in norm_methods) export_method(m)

# The last line of the log: the export is done (only reached when every method was exported)
n_files <- length(list.files(user_inputs$table_dir, pattern = "\\.(xlsx|csv)$"))
cat("\nExport finished: ", length(norm_methods), " normalization method(s), ", n_files,
    " file(s). Download them with \"Download exported tables (.zip)\".\n", sep = "")
