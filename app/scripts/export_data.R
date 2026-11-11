# ------------------------------------------------------------------------------------------- #
# Export the tables of one or more normalization methods, given by
# `--norm_method=<matrix ID>[,<matrix ID>...]` (e.g. `loess_norm`, `closest_norm`, `mvis_norm`,
# `raw`) or `--norm_method=auto` (default: the method with the lowest median QC RSD%, determined
# by `process.R`). Per method:
# 1. Normalized data of overall features including non-targets
# 2. Blank subtracted normalized data of overall features (never with the blanks)
# 3. Concentration data of targeted features (blank subtracted)
# 4. What happened to each feature and sample during the processing (from its event log)
#
# The injections in the tables (1-3) and their order (columns):
#   --keep_qc, --keep_blank, --keep_cal   Keep the QCs / blanks / calibration standards (TRUE);
#                                         the study samples are always kept
#   --group_by_type   Group the columns by injection type (TRUE), in the order of --group_order
#   --group_by_batch  Group the columns by batch first (FALSE), then by injection order
#   --group_order     The injection types in their order, as a JSON array (default
#                     ["QC","Samples","Blank","CalCurve"]): "QC", "Blank", "CalCurve" and the study
#                     sample groups as "grp:<Class>" ("Samples": all study groups not named)
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
  out_data <- if (IS_TARGET_MODE) {
    c("norm", "norm_blk", "conc", "history")
  } else {
    c("norm", "norm_blk", "history")
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
# The injections of the tables and their order
layout <- msdial$export_layout(
  by_type = arg_lgl("group_by_type", TRUE),
  by_batch = arg_lgl("group_by_batch", FALSE),
  order = jsonlite::fromJSON(arg("group_order", '["QC","Samples","Blank","CalCurve"]')),
  keep = c(QC = arg_lgl("keep_qc", TRUE), Blank = arg_lgl("keep_blank", TRUE), CalCurve = arg_lgl("keep_cal", TRUE))
)
cat("Injections in the tables: study samples",
    if (layout$keep[["QC"]]) ", QCs", if (layout$keep[["Blank"]]) ", blanks (not in the blank-subtracted tables)",
    if (layout$keep[["CalCurve"]]) ", calibration standards", ".\n", sep = "")
cat("Column order: ",
    if (layout$by_batch) "by batch, then ",
    if (layout$by_type) "by injection type, then ",
    "by injection order.\n", sep = "")

# Internal standards (IS) and volumetric internal standards (vIS) are not exported
not_std <- \(rd) !(util$is_internal_std(rd$.std_type) | rd$.std_type %in% "vIS")

# Export the tables of one method ----------
export_method <- function(mat_id) {
  ii <- which(mat_ids_to_export == mat_id)
  blk_mat_id <- util$mat_id_of_blank_subtracted(mat_id)
  is_closest_norm <- mat_id == "closest_norm"
  cat("\nExporting", mat_id, "data.\n")

  # Normalized data
  se <- msdial$read_full_normalized(user_inputs, to_report, mat_id, keep = not_std)
  msdial$export_data_with_feature_table_xlsx(
    sumexp_lst = se,
    mat_id = mat_id,
    org_feature_table = org_feature_table,
    out_file = FILE$o$norm[[ii]],
    is_closest_norm = is_closest_norm,
    max_xlsx_cells = max_xlsx_cells,
    layout = layout
  )
  cat(glue::glue("{mat_id} data has been saved."), "\n")
  rm(se)
  invisible(gc(FALSE))

  # Blank subtracted normalized data (the blanks are left out)
  msdial$export_data_with_feature_table_xlsx(
    sumexp_lst = msdial$split_by_batch(msdial$read_full_blank_subtracted(user_inputs, to_report, blk_mat_id, keep = not_std)),
    mat_id = blk_mat_id,
    org_feature_table = org_feature_table,
    out_file = FILE$o$norm_blk[[ii]],
    is_closest_norm = is_closest_norm,
    max_xlsx_cells = max_xlsx_cells,
    layout = layout
  )
  cat(glue::glue("{blk_mat_id} data has been saved."), "\n")
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
    if (!any(unlist(lapply(lst_proc, \(se) SumExp::row_df(se)$to_export)) %in% TRUE)) {
      cat("No chemical is exported (none has a study sample above its LLOQ; see the processing history):",
          "the concentration table is not written.\n")
      with_conc <- FALSE
    }
  }
  if (with_conc) {
    msdial$export_concentration_xlsx(
      sumexp_lst = lst_proc,
      file = FILE$o$conc[[ii]],
      is_closest_norm = is_closest_norm,
      calcurve_lst = lapply(proc_blk, \(ea) ea[["calcurve"]]),
      # Blank samples, quantified without blank subtraction (NULL for a batch without blanks, and
      # for data processed by older versions)
      blank_lst = lapply(proc_blk, \(ea) ea[["blank"]]),
      layout = layout
    )
    cat(glue::glue("{mat_id} concentration data has been saved to {basename(FILE$o$conc[[ii]])}."), "\n")
  }
  rm(list = intersect(c("proc_blk", "lst_proc"), ls()))
  invisible(gc(FALSE))

  # What happened to each feature and sample
  if (is.null(to_report[["feature history"]])) {   # Processed with an older version
    cat("No processing history in the processed data (re-run the processing to add it).\n")
  } else {
    history$write_history_xlsx(to_report, mat_id, FILE$o$history[[ii]])
    cat(glue::glue("The processing history has been saved to {basename(FILE$o$history[[ii]])}."), "\n")
  }

  # CSV copies: one CSV per worksheet, with the same layout as the sheet:
  # "(base name).(method).(norm|norm_blk|conc|history).(sheet).csv" (not the "Legend" sheet)
  xlsx_files <- unlist(lapply(FILE$o, \(x) x[[ii]]))
  for (xlsx in xlsx_files[file.exists(xlsx_files)]) {
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
  cat("CSV copies (one per worksheet) have been saved.\n")
  invisible(gc(FALSE))
}

# One method after the other (the memory of one at a time)
for (m in norm_methods) export_method(m)
