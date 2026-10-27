# ------------------------------------------------------------------------------------------- #
# The processing options: their defaults, allowed values and ranges, and reading them from a
# parameters file. Shared by the app (R/msdialInfoFuns.R, R/runProcess.R) and the scripts
# (scripts/read-msdial.R, scripts/process.R), so that a parameters file saved in the app gives
# the same processing on the command line.
# ------------------------------------------------------------------------------------------- #

#' The processing options
#'
#' `module`: the app module of the input (`runReadMsdialUI("read_msdial")`, `runProcessUI("proc")`;
#' the first also tells the script: "read_msdial" = read-msdial.R, "proc" = process.R). `default`:
#' the value of the app; `choices`: the allowed values of a choice (each in a list: use
#' `default[[i]][[1]]`); `min` / `max`: the allowed range of a number (`NA`: none); `integer`:
#' whole numbers only.
#' @export
processing_options <- tibble::tribble(
  ~id,                   ~module,       ~type,      ~default,                        ~choices,                                             ~min, ~max, ~integer,
  "per_batch",           "read_msdial", "checkbox", list(TRUE),                      list(NULL),                                           NA,   NA,   FALSE,
  "optimize_cal_points", "proc",        "checkbox", list(FALSE),                     list(NULL),                                           NA,   NA,   FALSE,
  "set_minimum_number",  "proc",        "checkbox", list(FALSE),                     list(NULL),                                           NA,   NA,   FALSE,
  "min_cal_points",      "proc",        "numeric",  list(3),                         list(NULL),                                           3,    NA,   TRUE,
  "average_cal_reps",    "proc",        "checkbox", list(FALSE),                     list(NULL),                                           NA,   NA,   FALSE,
  "keep_all_calcurves",  "proc",        "checkbox", list(FALSE),                     list(NULL),                                           NA,   NA,   FALSE,
  "r2_filter",           "proc",        "checkbox", list(FALSE),                     list(NULL),                                           NA,   NA,   FALSE,
  "min_r2",              "proc",        "numeric",  list(0.99),                      list(NULL),                                           0,    1,    FALSE,
  "linear_only",         "proc",        "checkbox", list(FALSE),                     list(NULL),                                           NA,   NA,   FALSE,
  "enforce_linear_range","proc",        "checkbox", list(FALSE),                     list(NULL),                                           NA,   NA,   FALSE,
  "linear_range_tol",    "proc",        "numeric",  list(15),                        list(NULL),                                           1,    50,   FALSE,
  "vis_prenorm",         "proc",        "checkbox", list(FALSE),                     list(NULL),                                           NA,   NA,   FALSE,
  "rm_outlier",          "proc",        "checkbox", list(TRUE),                      list(NULL),                                           NA,   NA,   FALSE,
  "rm_outlier_scope",    "proc",        "radio",    list("all"),                     list(c("all", "no_calib")),                           NA,   NA,   FALSE,
  "log_calibration",     "proc",        "checkbox", list(FALSE),                     list(NULL),                                           NA,   NA,   FALSE,
  "blank_filter",        "proc",        "checkbox", list(TRUE),                      list(NULL),                                           NA,   NA,   FALSE,
  "blank_filter_factor", "proc",        "numeric",  list(5),                         list(NULL),                                           0,    NA,   FALSE,
  "weight",              "proc",        "radio",    list("largestR2"),               list(c("largestR2", "1", "1_div_x", "1_div_x2")),     NA,   NA,   FALSE,
  "llox_method",         "proc",        "radio",    list("pt_signal_mean_plus_sd"),  list(c("pt_signal_mean", "pt_signal_mean_plus_sd")), NA,   NA,   FALSE,
  "use_rsd20",           "proc",        "checkbox", list(TRUE),                      list(NULL),                                           NA,   NA,   FALSE,
  "impute_llox",         "proc",        "checkbox", list(TRUE),                      list(NULL),                                           NA,   NA,   FALSE,
  "rsd_filter",          "proc",        "checkbox", list(FALSE),                     list(NULL),                                           NA,   NA,   FALSE,
  "rsd_filter_scope",    "proc",        "radio",    list("both"),                    list(c("both", "targeted", "nontargeted")),           NA,   NA,   FALSE,
  "rsd_max_targeted",    "proc",        "numeric",  list(30),                        list(NULL),                                           1,    80,   FALSE,
  "rsd_max_nontargeted", "proc",        "numeric",  list(30),                        list(NULL),                                           1,    80,   FALSE
)

#' Read a parameters file (YAML), also one saved with a UTF-8 byte order mark (e.g. by Windows
#' Notepad), which would otherwise hide its first key
#'
#' @param path Path to the file
#' @returns A named list
#' @export
read_parameters_file <- function(path) {
  txt <- readLines(path, warn = FALSE, encoding = "UTF-8")
  if (length(txt) > 0) txt[1] <- sub("^﻿", "", txt[1])
  p <- yaml::yaml.load(paste(txt, collapse = "\n"))
  if (!is.list(p)) p <- list()
  p
}

#' Whether a value is valid for option `i` of [processing_options]
#' @export
is_valid_value <- function(i, v) {
  length(v) == 1 && !is.null(v) && switch(
    processing_options$type[i],
    checkbox = !is.na(suppressWarnings(as.logical(v))),
    numeric  = is.finite(suppressWarnings(as.numeric(v))),
    radio    = as.character(v) %in% processing_options$choices[[i]][[1]]
  )
}

#' A valid value in the type of option `i` (`v` already checked with [is_valid_value()])
#' @export
as_option_value <- function(i, v) {
  switch(processing_options$type[i],
         checkbox = as.logical(v), numeric = as.numeric(v), radio = as.character(v))
}

#' A number of option `i` within its allowed range: the closest allowed value (whole numbers
#' rounded), or the default when missing
#' @export
clamp_value <- function(i, v) {
  v <- suppressWarnings(as.numeric(v))
  if (length(v) != 1 || !is.finite(v)) return(processing_options$default[[i]][[1]])
  if (processing_options$integer[i]) v <- round(v)
  if (!is.na(processing_options$min[i])) v <- max(v, processing_options$min[i])
  if (!is.na(processing_options$max[i])) v <- min(v, processing_options$max[i])
  v
}

#' The allowed range of number option `i`, as text (e.g. "at least 0 and at most 1")
#' @export
range_text <- function(i) {
  mn <- processing_options$min[i]; mx <- processing_options$max[i]
  paste0(if (is.na(mn)) "" else paste("at least", mn), if (!is.na(mn) && !is.na(mx)) " and " else "",
         if (is.na(mx)) "" else paste("at most", mx), if (processing_options$integer[i]) ", a whole number" else "")
}

#' The options of a script from the parameters file and its command line
#'
#' The values of the `processing:` section of the parameters file are used for the options that
#' are not given on the command line (the command line wins). As in the app: a value that is not
#' valid is replaced by the default, a number outside its range by the closest allowed value, and
#' without `set_minimum_number` the minimum number of calibration points is 3. Each change is
#' printed as a note.
#'
#' @param params The options parsed from the command line (`optparse::parse_args()`), with their
#'   defaults
#' @param module "read_msdial" (read-msdial.R) or "proc" (process.R): the options of that script
#' @param file The parameters file (`QUANTA_PARAMS_FILE`, as `msdial$get_user_input()`)
#' @param args The command line arguments (to know which options were given there)
#' @returns `params` with the values of the file, checked
#' @export
options_with_parameters_file <- function(params, module, file = Sys.getenv("QUANTA_PARAMS_FILE", unset = "params.yml"),
                                         args = commandArgs(trailingOnly = TRUE)) {
  given <- unique(sub("^--([^=]+).*$", "\\1", grep("^--", args, value = TRUE)))
  p <- if (file.exists(file)) read_parameters_file(file)[["processing"]] else NULL
  if (!is.list(p)) p <- list()
  notes <- character()
  ids <- processing_options$id[processing_options$module == module]
  for (id in ids) {
    i <- match(id, processing_options$id)
    if (id %in% given || is.null(p[[id]])) next
    v <- p[[id]]
    if (!is_valid_value(i, v)) {
      notes <- c(notes, sprintf("\"%s\": the value \"%s\" in the parameters file is not valid; the default (%s) is used.",
                                id, paste(format(v), collapse = ", "), format(processing_options$default[[i]][[1]])))
      v <- processing_options$default[[i]][[1]]
    }
    params[[id]] <- as_option_value(i, v)
  }
  # Without "set_minimum_number" (in the file), the minimum number of calibration points is 3
  if ("min_cal_points" %in% ids && !"min_cal_points" %in% given && isFALSE(as.logical(p[["set_minimum_number"]]))) {
    params$min_cal_points <- 3
  }
  # Numbers within their allowed range
  for (id in intersect(ids, processing_options$id[processing_options$type == "numeric"])) {
    i <- match(id, processing_options$id)
    if (is.null(params[[id]])) next
    to <- clamp_value(i, params[[id]])
    if (!isTRUE(all.equal(suppressWarnings(as.numeric(params[[id]])), to))) {
      notes <- c(notes, sprintf("\"%s\": %s is outside the allowed range (%s); %s is used.",
                                id, format(params[[id]]), range_text(i), format(to)))
      params[[id]] <- to
    }
  }
  if (length(params$min_cal_points) == 1) params$min_cal_points <- as.integer(params$min_cal_points)
  if (length(p) > 0) cat("Processing options from the parameters file", basename(file), "(command line options take precedence).\n")
  for (n in notes) cat("Note:", n, "\n")
  params
}
