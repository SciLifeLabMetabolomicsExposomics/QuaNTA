# ------------------------------------------------------------------------------------------- #
# A collection of functions for `msdialInfo.R`
# Parameters live in Shiny inputs. A YAML file is only used
#   (a) as an optional upload to pre-fill the inputs,
#   (b) as an optional download of the current inputs, and
#   (c) as a private, per-session hand-over file to the Rscript / Quarto child processes.
# ------------------------------------------------------------------------------------------- #

# IDs stored in the parameter file (and expected by `msdial$get_user_input()`)
.user_param_ids <- c(
  "project_title", "user", "university", "free_text",
  "input_file", "concentration_unit",
  "intermediate_dir", "table_dir", "report_dir"
)
# Those the user can type in / load from a file
.text_param_ids <- c("project_title", "user", "university", "concentration_unit")
.textarea_param_ids <- "free_text"

# Processing options that live in the other modules' inputs. `module` must match the IDs
# used in app.R (`runReadMsdialUI("read_msdial")` and `runProcessUI("proc")`).
.processing_inputs <- tibble::tribble(
  ~id,                   ~module,       ~type,
  "per_batch",           "read_msdial", "checkbox",
  "optimize_cal_points", "proc",        "checkbox",
  "set_minimum_number",  "proc",        "checkbox",
  "min_cal_points",      "proc",        "numeric",
  "keep_all_calcurves",  "proc",        "checkbox",
  "r2_filter",           "proc",        "checkbox",
  "min_r2",              "proc",        "numeric",
  "linear_only",         "proc",        "checkbox",
  "rm_outlier",         "proc",        "checkbox",
  "log_calibration",     "proc",        "checkbox",
  "blank_filter",        "proc",        "checkbox",
  "blank_filter_factor", "proc",        "numeric",
  "weight",              "proc",        "radio",
  "llox_method",         "proc",        "radio",
  "use_rsd20",           "proc",        "checkbox",
  "rsd_filter",          "proc",        "checkbox",
  "rsd_filter_scope",    "proc",        "radio",
  "rsd_max_targeted",    "proc",        "numeric",
  "rsd_max_nontargeted", "proc",        "numeric"
)

#' Current processing options as a named list (for the parameter download)
#'
#' @param input The root `input` of `server()`
#' @export
collect_processing_params <- function(input) {
  vals <- lapply(seq_len(nrow(.processing_inputs)), function(i) {
    input[[paste0(.processing_inputs$module[i], "-", .processing_inputs$id[i])]]
  })
  names(vals) <- .processing_inputs$id
  vals[!vapply(vals, is.null, logical(1))]
}

#' Push loaded processing options into the inputs of the other modules
#'
#' @param session The root `session` of `server()`
#' @param p A named list, typically the `processing:` section of an uploaded parameter file
#' @export
apply_processing_params <- function(session, p) {
  for (i in seq_len(nrow(.processing_inputs))) {
    id <- .processing_inputs$id[i]
    if (is.null(p[[id]])) next
    full_id <- paste0(.processing_inputs$module[i], "-", id)
    switch(
      .processing_inputs$type[i],
      checkbox = shiny::updateCheckboxInput(session, full_id, value = isTRUE(as.logical(p[[id]]))),
      numeric  = shiny::updateNumericInput(session, full_id, value = as.numeric(p[[id]])),
      radio    = shiny::updateRadioButtons(session, full_id, selected = as.character(p[[id]]))
    )
  }
  invisible(NULL)
}

#' Read an uploaded parameter file
#'
#' @param path Path to a `.yml` / `.yaml` file
#' @returns A named list
#' @export
read_params_file <- function(path) {
  if (!tolower(tools::file_ext(path)) %in% c("yml", "yaml")) {
    stop("The parameter file must have a .yml or .yaml extension.", call. = FALSE)
  }
  p <- yaml::read_yaml(path)
  if (!is.list(p) || is.null(names(p))) stop("The parameter file is empty or malformed.", call. = FALSE)
  p
}

#' Write the private, per-session parameter file read by the child processes
#'
#' The directories are always taken from the session workspace `ws`, never from user input.
#'
#' @param info A plain named list, e.g. `shiny::reactiveValuesToList(data_info)`
#' @param ws The list returned by [create_session_workspace()]
#' @returns The path of the written YAML file
#' @export
write_session_params <- function(info, ws) {
  stopifnot(is.list(info), !shiny::is.reactive(info))
  p <- lapply(rlang::set_names(.user_param_ids), function(id) {
    v <- info[[id]]
    if (is.null(v)) "" else as.character(v)
  })
  p$intermediate_dir <- ws$intermediate
  p$table_dir <- ws$tables
  p$report_dir <- ws$report
  if (!nzchar(p$input_file) || !file.exists(p$input_file)) {
    stop("Please upload an MS-DIAL output file first.", call. = FALSE)
  }
  path <- file.path(ws$base, "params.yml")
  yaml::write_yaml(p, path)
  path
}
