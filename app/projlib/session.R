# ------------------------------------------------------------------------------------------- #
# Session data: one .rds file with everything needed to continue without re-running the
# reading and processing steps (used by scripts/session_data.R for the app's save / load)
#   - the original MS-DIAL file (the exports and reports read its feature table)
#   - the parsed data (output of read-msdial.R)
#   - the processed data and the intermediate QC data (output of process.R), if processed
#   - the project information and the processing parameters
# ------------------------------------------------------------------------------------------- #
box::use(./msdial)

#' Identification of a session data file
#' @export
format_name <- "QuaNTA session"
#' Version of the session data format. Increase it when the content changes incompatibly.
#' @export
format_version <- 1L

# Path of a data file in `intermediate_dir` for the MS-DIAL file `input_file`
.data_file <- function(input_file, intermediate_dir, suffix) {
  msdial$get_raw_data_file_name(
    list(input_file = input_file, intermediate_dir = intermediate_dir),
    suffix = suffix
  )
}

#' Collect the session data
#'
#' @param input_file Path of the uploaded MS-DIAL file in the session workspace.
#' @param input_file_name The original name of the MS-DIAL file.
#' @param intermediate_dir The directory with the parsed / processed data files.
#' @param user_params A named list of the project information.
#' @param processing A named list of the processing parameters.
#' @returns A list (the session data)
#' @export
build_bundle <- function(input_file, input_file_name, intermediate_dir, user_params, processing) {
  parsed_file <- .data_file(input_file, intermediate_dir, "")
  if (!file.exists(parsed_file)) stop("Nothing to save yet: read an MS-DIAL file first.")
  read_if_exists <- function(suffix) {
    path <- .data_file(input_file, intermediate_dir, suffix)
    if (file.exists(path)) readRDS(path) else NULL
  }
  list(
    format = format_name,
    format_version = format_version,
    created = Sys.time(),
    input_file_name = input_file_name,
    input_file_basename = basename(input_file),
    input_file_raw = readBin(input_file, "raw", n = file.size(input_file)),
    user_params = user_params,
    processing = processing,
    parsed = readRDS(parsed_file),
    to_report = read_if_exists("to_report"),
    proc = read_if_exists("proc")
  )
}

#' Write the session data into a session workspace
#'
#' Removes the earlier data of the workspace, as when a new MS-DIAL file is uploaded.
#'
#' @param b Valid session data, see [validate_bundle()].
#' @param input_dir,intermediate_dir,tables_dir Directories of the session workspace.
#' @returns The path of the restored MS-DIAL file
#' @export
restore_bundle <- function(b, input_dir, intermediate_dir, tables_dir) {
  for (d in c(input_dir, intermediate_dir, tables_dir)) {
    unlink(list.files(d, full.names = TRUE), recursive = TRUE)
  }
  input_file <- file.path(input_dir, b$input_file_basename)
  writeBin(b$input_file_raw, input_file)
  input_file <- normalizePath(input_file, winslash = "/", mustWork = TRUE)
  saveRDS(b$parsed, .data_file(input_file, intermediate_dir, ""))
  if (!is.null(b$to_report)) saveRDS(b$to_report, .data_file(input_file, intermediate_dir, "to_report"))
  # `[[` (exact match): `$proc` would partially match `processing` when `proc` is absent
  if (!is.null(b[["proc"]])) saveRDS(b[["proc"]], .data_file(input_file, intermediate_dir, "proc"))
  input_file
}

#' Check that an object is valid session data
#'
#' @param b The object read from an uploaded .rds file
#' @returns A character vector of problems; empty if the object can be loaded
#' @export
validate_bundle <- function(b) {
  if (!is.list(b) || !identical(b$format, format_name)) {
    return("The file is not a QuaNTA session file (saved with \"Save session data\").")
  }
  p <- character(0)
  if (!isTRUE(b$format_version <= format_version)) {
    p <- c(p, paste0("It was saved by a newer version of QuaNTA (format version ",
                     b$format_version, ")."))
  }
  if (!is.raw(b$input_file_raw) || length(b$input_file_raw) == 0 ||
      !is.character(b$input_file_basename) || !nzchar(b$input_file_basename) ||
      grepl("[/\\\\]", b$input_file_basename)) {
    p <- c(p, "The original MS-DIAL file is missing.")
  }

  # Parsed data (read-msdial.R)
  se <- b$parsed
  if (!inherits(se, "SumExp")) {
    p <- c(p, "The read MS-DIAL data is missing or not a SumExp object.")
  } else {
    if (!"raw" %in% names(se)) p <- c(p, "The read MS-DIAL data has no \"raw\" values.")
    if (!all(c("batch_id", ".ctrl_cat", "sample_name") %in% names(SumExp::col_df(se)))) {
      p <- c(p, "The read MS-DIAL data lacks sample information (batch, category or name).")
    }
    if (is.null(SumExp::metadata(se)$is_non_target_mode)) {
      p <- c(p, "The read MS-DIAL data lacks the target mode information.")
    }
  }

  # Processed data (process.R), optional
  tr <- b$to_report
  proc <- b[["proc"]]   # Exact match (`$proc` would partially match `processing`)
  if (!is.null(tr)) {
    if (!is.list(tr) || !isTRUE(tr[["Completed"]])) {
      p <- c(p, "The processed data is incomplete (processing did not finish).")
    } else {
      if (!inherits(tr[["normalized"]], "SumExp")) {
        p <- c(p, "The processed data lacks the normalized data.")
      }
      mat_ids <- tr[["matrix ids to blk subt"]]
      if (!is.character(mat_ids)) {
        p <- c(p, "The processed data lacks the list of normalization methods.")
      }
      # Calibrated data, except in non-target mode
      if (!isTRUE(tr[["is_non_target_mode"]]) && is.character(mat_ids)) {
        blk_ids <- paste0(mat_ids, "_blk")
        if (!is.list(proc) || !all(blk_ids %in% names(proc))) {
          p <- c(p, "The calibrated data is missing for some normalization methods.")
        } else {
          ok <- all(vapply(proc[blk_ids], function(per_batch) {
            is.list(per_batch) && length(per_batch) > 0 &&
              all(vapply(per_batch, function(x) {
                inherits(x$calcurve, "SumExp") && inherits(x$concn, "SumExp")
              }, logical(1)))
          }, logical(1)))
          if (!ok) p <- c(p, "The calibrated data is malformed (calibration curve / concentrations).")
        }
      }
    }
  } else if (!is.null(proc)) {
    p <- c(p, "The calibrated data is present without the processed (QC) data.")
  }
  p
}
