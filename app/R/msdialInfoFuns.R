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

# Processing options that live in the other modules' inputs, with their defaults, allowed values
# and ranges: shared with the scripts (projlib/settings.R). `module` must match the IDs used in
# app.R (`runReadMsdialUI("read_msdial")` and `runProcessUI("proc")`).
box::use(settings = projlib/settings)
.processing_inputs <- settings$processing_options

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
#' A value that is not valid for its option (e.g. a misspelled choice, "maybe" for a tick box, or
#' text for a number) is replaced by the option's default; numbers outside their range are kept
#' here and corrected when "Process data" is clicked (see [processing_value_corrections()]).
#'
#' @param session The root `session` of `server()`
#' @param p A named list, typically the `processing:` section of an uploaded parameter file
#' @returns A character vector: one note per value that was replaced by the default (shown when
#'   "Process data" is clicked), invisibly
#' @export
apply_processing_params <- function(session, p) {
  notes <- character()
  shown <- function(v) paste(format(v), collapse = ", ")
  for (i in seq_len(nrow(.processing_inputs))) {
    id <- .processing_inputs$id[i]
    v <- p[[id]]
    if (is.null(v)) next
    full_id <- paste0(.processing_inputs$module[i], "-", id)
    def <- .processing_inputs$default[[i]][[1]]
    type <- .processing_inputs$type[i]
    ok <- length(v) == 1 && switch(
      type,
      checkbox = !is.na(suppressWarnings(as.logical(v))),
      numeric  = is.finite(suppressWarnings(as.numeric(v))),
      radio    = as.character(v) %in% .processing_inputs$choices[[i]][[1]]
    )
    if (!ok) {
      notes <- c(notes, sprintf("\"%s\": the value \"%s\" in the parameters file is not valid; set to the default (%s).",
                                id, shown(v), shown(def)))
      v <- def
    }
    switch(
      type,
      checkbox = shiny::updateCheckboxInput(session, full_id, value = isTRUE(as.logical(v))),
      numeric  = shiny::updateNumericInput(session, full_id, value = as.numeric(v)),
      radio    = shiny::updateRadioButtons(session, full_id, selected = as.character(v))
    )
  }
  invisible(notes)
}

#' Numbers of the processing options outside their allowed range (or missing), with the value
#' they are set to: the closest allowed value (whole numbers rounded), or the default when missing
#'
#' @param input The `input` of the processing module (`runProcessServer()`)
#' @returns A tibble of `id`, `from` (as text) and `to` (one row per value to correct)
#' @export
processing_value_corrections <- function(input) {
  idx <- which(.processing_inputs$module == "proc" & .processing_inputs$type == "numeric")
  rows <- lapply(idx, function(i) {
    v <- suppressWarnings(as.numeric(input[[.processing_inputs$id[i]]]))
    to <- settings$clamp_value(i, v)
    if (length(v) == 1 && is.finite(v) && isTRUE(all.equal(v, to))) return(NULL)
    tibble::tibble(id = .processing_inputs$id[i], from = if (length(v) == 1 && !is.na(v)) format(v) else "empty",
                   to = to, range = settings$range_text(i))
  })
  dplyr::bind_rows(rows)
}#' Read an uploaded parameter file
#'
#' @param path Path to a `.yml` / `.yaml` file
#' @returns A named list
#' @export
read_params_file <- function(path) {
  if (!tolower(tools::file_ext(path)) %in% c("yml", "yaml")) {
    stop("The parameter file must have a .yml or .yaml extension.", call. = FALSE)
  }
  p <- settings$read_parameters_file(path)   # Also with a UTF-8 byte order mark (e.g. Notepad)
  if (!is.list(p) || is.null(names(p))) stop("The parameter file is empty or malformed.", call. = FALSE)
  p
}

#' Why an uploaded data file has a type that cannot be used
#'
#' Accepted: `.txt`, `.csv`, `.xlsx`, `.txt.gz` / `.csv.gz` and `.zip` (whose single file must be
#' one of these; checked by `scripts/prepare_input.R`). See `projlib/input_file.R`.
#'
#' @param name The original file name
#' @returns A message, or `NULL` when the type is accepted
#' @export
upload_type_problem <- function(name) {
  box::use(input_file = projlib/input_file)
  input_file$type_problem(name)
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
    stop("Please upload a data file first.", call. = FALSE)
  }
  path <- file.path(ws$base, "params.yml")
  yaml::write_yaml(p, path)
  path
}
