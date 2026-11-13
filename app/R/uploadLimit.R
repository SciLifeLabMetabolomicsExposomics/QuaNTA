# ------------------------------------------------------------------------------------------- #
# Upload size limit on SciLifeLab Serve
#
# Serve's web proxy rejects uploads above its limit ("413 Request Entity Too Large") before they
# reach the app, so R cannot change that limit. Serve has allowed QuaNTA uploads of up to 1000 MB.
# Locally (RStudio, local Docker) there is no such proxy and only `shiny.maxRequestSize` (app.R)
# applies. Whether the app runs on Serve is told by the address the user has opened, since the
# Docker image is the same in both places.
# ------------------------------------------------------------------------------------------- #

.serve_host_regex <- "(^|\\.)serve\\.scilifelab\\.se$"   # Also used in the JavaScript below
.serve_upload_limit_mb <- 1000
.serve_xlsx_max_cells <- 5e6     # On Serve, larger tables are exported as CSV only (memory)
.serve_warn_values <- 4.8e6      # On Serve, a popup after uploading a file with more values

#' Whether the app is opened on SciLifeLab Serve
#'
#' @param session The Shiny session.
#' @export
is_on_serve <- function(session = shiny::getDefaultReactiveDomain()) {
  host <- session$clientData$url_hostname
  !is.null(host) && grepl(.serve_host_regex, host, ignore.case = TRUE)
}

#' Argument of `export_data.R` that limits the size of the Excel files, only on Serve
#'
#' @param session The Shiny session.
#' @returns A character vector (empty outside Serve)
#' @export
serve_export_args <- function(session = shiny::getDefaultReactiveDomain()) {
  if (shiny::isolate(is_on_serve(session))) sprintf("--max_xlsx_cells=%.0f", .serve_xlsx_max_cells) else character()
}

#' Note in the export box, only on Serve, on the tables that will be exported as CSV only
#'
#' @param n_values The numbers of values of the tables (`msdial$export_table_values()`), or `NULL`
#' @param session The Shiny session.
#' @returns A help text, or `NULL` if no table is above the limit
#' @export
export_size_note <- function(n_values, session = shiny::getDefaultReactiveDomain()) {
  if (is.null(n_values) || !is_on_serve(session)) return(NULL)
  over <- n_values[n_values > .serve_xlsx_max_cells]
  if (length(over) == 0) return(NULL)
  labels <- c(norm = "normalized", norm_blk = "blank-subtracted")[names(over)]
  shiny::helpText(sprintf(
    "On SciLifeLab Serve, tables above %.0f million values are exported as CSV only: here the %s. Run QuaNTA locally for Excel files.",
    .serve_xlsx_max_cells / 1e6,
    paste(sprintf("%s table (%.1f million)", labels, over / 1e6), collapse = " and the ")
  ))
}

#' Number of lines of a file, read a part at a time
.count_file_lines <- function(path) {
  con <- file(path, "rb")
  on.exit(close(con))
  n <- 0
  last <- as.raw(10L)
  repeat {
    b <- readBin(con, "raw", 16e6)
    if (length(b) == 0) break
    n <- n + sum(b == as.raw(10L))
    last <- b[length(b)]
  }
  n + (last != as.raw(10L))
}

#' Number of values (features x injections) of an MS-DIAL file, without reading its data
#'
#' @param path Path to the MS-DIAL file
#' @returns A number, or `NULL` for a file that cannot be understood (the reading step explains why)
.count_msdial_values <- function(path) {
  box::use(msdial = projlib/msdial)
  tryCatch({
    n_injections <- length(msdial$get_three_section_indices(path)[["2nd"]])
    max(0, .count_file_lines(path) - 5) * n_injections      # 5 header lines
  }, error = function(e) NULL)
}

#' Popup on SciLifeLab Serve when an uploaded data file is large, right after the upload
#'
#' @param path Path to the uploaded (decompressed) MS-DIAL file
#' @param session The Shiny session.
#' @export
warn_if_large_upload <- function(path, session = shiny::getDefaultReactiveDomain()) {
  if (!shiny::isolate(is_on_serve(session))) return(invisible(NULL))
  n <- .count_msdial_values(path)
  if (is.null(n) || n <= .serve_warn_values) return(invisible(NULL))
  shiny::showModal(session = session, shiny::modalDialog(
    title = "Large dataset",
    shiny::p(sprintf("This dataset has %.1f million values (features × injections).", n / 1e6),
             "On SciLifeLab Serve this limits what you can do with it:"),
    shiny::tags$ul(
      shiny::tags$li(sprintf(paste("The normalized and blank-subtracted tables are exported as CSV only",
                                   "if they have more than %.0f million values, not as Excel files."),
                             .serve_xlsx_max_cells / 1e6)),
      shiny::tags$li("Processing and the reports take long, and the server may run out of memory.")
    ),
    shiny::p("Run QuaNTA on your own computer for the full Excel export."),
    easyClose = TRUE,
    footer = shiny::modalButton("OK")
  ))
  invisible(NULL)
}

#' JavaScript that stops uploads above the Serve limit (only on Serve) before they start
#'
#' Applies to all file inputs. A capture listener on the document runs before Shiny's own
#' `change` handler of the file input, so a too large file is never sent. The server is told via
#' the input `quanta_upload_too_large` to show a message ([uploadLimitServer()]).
#' @export
upload_limit_script <- function() {
  shiny::tags$script(shiny::HTML(sprintf("
    (function() {
      var onServe = new RegExp(%s, 'i').test(window.location.hostname);
      if (!onServe) return;
      var maxBytes = %d * 1e6;
      document.addEventListener('change', function(e) {
        var el = e.target;
        if (!el || el.type !== 'file' || !el.files) return;
        var big = Array.prototype.filter.call(el.files, function(f) { return f.size > maxBytes; });
        if (big.length === 0) return;
        e.stopImmediatePropagation();   // Shiny's upload handler is not reached
        el.value = '';
        Shiny.setInputValue('quanta_upload_too_large',
          { name: big[0].name, mb: big[0].size / 1e6 }, { priority: 'event' });
      }, true);
    })();
  ", jsonlite::toJSON(.serve_host_regex, auto_unbox = TRUE), .serve_upload_limit_mb)))
}

#' Message for a file that is too large for Serve (see [upload_limit_script()])
#'
#' @param input,session The root `input` and `session` of `server()`.
#' @export
uploadLimitServer <- function(input, session) {
  shiny::observeEvent(input$quanta_upload_too_large, {
    f <- input$quanta_upload_too_large
    shiny::showNotification(
      sprintf(paste(
        "%s is %.0f MB, but SciLifeLab Serve accepts uploads of at most %d MB.",
        "Compress the data file as .zip or .gz (typically 5-10 times smaller) and upload that,",
        "or run QuaNTA locally, where there is no such limit."
      ), f$name, f$mb, .serve_upload_limit_mb),
      type = "error", duration = NULL, session = session
    )
  })
}

#' Hint under the MS-DIAL file upload: the Serve limit is only mentioned on Serve
#'
#' @param session The Shiny session.
#' @export
upload_hint <- function(session = shiny::getDefaultReactiveDomain()) {
  if (is_on_serve(session)) {
    shiny::helpText(sprintf(paste(
      "Text (.txt, .csv) or Excel (.xlsx), or compressed as .gz or .zip (with one file). SciLifeLab",
      "Serve accepts files of at most %d MB: compress larger files (typically 5-10 times smaller)."
    ), .serve_upload_limit_mb))
  } else {
    shiny::helpText("Text (.txt, .csv) or Excel (.xlsx), or compressed as .gz or .zip (with one file).")
  }
}
