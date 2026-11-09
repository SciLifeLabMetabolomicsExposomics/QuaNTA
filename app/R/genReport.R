# ------------------------------------------------------------------------------------------- #
# A Shiny module to generate the reports (rendered into the per-session report directory)
#
# Rendering runs in a background process (scripts/render_report.R via [run_bg_rscript()]) in a
# private copy of the report sources, so it neither freezes the app for other users nor shares
# Quarto's intermediate files with them.
# ------------------------------------------------------------------------------------------- #

#' A Shiny UI module to generate a report
#'
#' @param uiId A string that identifies the UI module.
#' @export
genReportUI <- function(uiId) {
  ns <- shiny::NS(uiId)
  # One group per report (button, progress bar, message): the card spaces the groups only
  shiny::tagList(
    # External report (PDF): shown in the app when generated ("Preview report", app.R, with its
    # download button)
    shiny::div(
      class = "q-gen-group",
      shiny::actionButton(inputId = ns("gen_external"), label = "Summary report (pdf)", class = "q-gen-btn"),
      shiny::uiOutput(ns("progress_external")),   # Progress bar while rendering
      shiny::textOutput(ns("gen_external_out"))
    ),
    shiny::div(
      class = "q-gen-group",
      shiny::actionButton(inputId = ns("gen_internal"), label = "QC/QA report (html)", class = "q-gen-btn"),
      shiny::uiOutput(ns("progress_internal")),
      shiny::textOutput(ns("gen_internal_out"))
    )
  )
}

#' Progress of a report rendering, from the output of Quarto so far
#'
#' knitr writes a line "k/N [chunk label]" for each of the N parts (text and code chunks) of the
#' report, then "output file: ...", after which pandoc (and for a PDF, LaTeX) runs.
#'
#' @param log The output lines of the rendering process so far
#' @param pdf `TRUE` for the PDF report (LaTeX after pandoc)
#' @returns A list with `value` (0 to 1) and `text`
#' @export
report_progress <- function(log, pdf = FALSE) {
  log <- trimws(unlist(strsplit(gsub("\033\\[[0-9;]*m", "", log), "\r", fixed = TRUE)))
  if (any(grepl("^output file:", log))) {
    if (pdf && any(grepl("latex", log, ignore.case = TRUE))) {
      return(list(value = 0.95, text = "Typesetting the PDF (LaTeX) ..."))
    }
    return(list(value = 0.92, text = if (pdf) "Converting to PDF (pandoc) ..." else "Building the HTML page (pandoc) ..."))
  }
  steps <- regmatches(log, regexec("^(\\d+)/(\\d+)\\s*(\\[(.*)\\])?", log))
  steps <- Filter(length, steps)
  if (length(steps) == 0) return(list(value = 0.02, text = "Starting Quarto ..."))
  last <- steps[[length(steps)]]
  k <- as.numeric(last[2]); n <- as.numeric(last[3])
  label <- if (nzchar(last[5]) && !grepl("^unnamed-chunk", last[5])) paste0(" (", last[5], ")") else ""
  list(value = 0.03 + 0.87 * k / n,
       text = sprintf("Running the report code: part %d of %d%s ...", as.integer(k), as.integer(n), label))
}

# A Bootstrap progress bar with a line of text under it
progress_bar_ui <- function(p) {
  if (is.null(p)) return(NULL)
  pct <- round(100 * min(max(p$value, 0), 1))
  shiny::div(
    style = "margin: 8px 0 6px; max-width: 460px;",
    shiny::div(
      class = "progress", style = "height: 22px;",
      shiny::div(class = "progress-bar progress-bar-striped progress-bar-animated", role = "progressbar",
                 style = sprintf("width: %d%%;", pct), `aria-valuenow` = pct, `aria-valuemin` = 0,
                 `aria-valuemax` = 100, sprintf("%d%%", pct))
    ),
    shiny::div(class = "small text-muted", style = "margin-top: 3px;", p$text)
  )
}

#' A Shiny server module to generate a report
#'
#' @param serverId A string that identifies the server module. It should match the UI module's ID.
#' @param data_info The `reactiveValues` returned by [msdialInfoServer()].
#' @param ws The session workspace from [create_session_workspace()].
#' @param norm_method A reactive returning the normalization method (e.g. `"loess_norm"`).
#' @param no_external_reason `NULL`, or a reactive returning why the external (PDF) report cannot be
#'   generated (e.g. no calibration in non-target mode), `NULL` when it can.
#'
#' @returns A list with the reactives `report_path` (absolute path of the latest internal report,
#'   `""` before the first one; its file name includes the time of generation), `report_ready`
#'   (changes every time an internal report was rendered), `external_path` and `external_ready`
#'   (the same for the external PDF report; `NULL` before the first one), and `reset()`.
#' @export
genReportServer <- function(serverId, data_info, ws, norm_method, no_external_reason = NULL) {
  shiny::moduleServer(serverId, function(input, output, session) {
    msg <- shiny::reactiveVal("")
    report_ready <- shiny::reactiveVal(NULL)
    # One step at a time per session: a report reads the files that reading and processing write
    rendering <- session_busy(session)
    # Progress of the rendering (bar under its button), `NULL` when not rendering
    progress <- list(internal = shiny::reactiveVal(NULL), external = shiny::reactiveVal(NULL))
    output$progress_internal <- shiny::renderUI(progress_bar_ui(progress$internal()))
    output$progress_external <- shiny::renderUI(progress_bar_ui(progress$external()))

    # Report file name with the time of generation, e.g.
    # "Dummy_Test-loess_norm-internal_20261010_143205.html" (`type`: "internal.html" / "external.pdf")
    report_name <- function(type, time) {
      shiny::req(data_info[["input_file"]], nzchar(data_info[["input_file"]]), norm_method())
      base <- tools::file_path_sans_ext(basename(data_info[["input_file"]]), compression = TRUE)
      paste0(base, "-", norm_method(), "-", sub("\\.", paste0("_", format(time, "%Y%m%d_%H%M%S"), "."), type))
    }
    # The latest internal report of this session (shown in the app and downloaded)
    report_path <- shiny::reactiveVal("")

    # Start rendering a report in the background; `on_done(path)` / `on_error(message)` follow
    render_report <- function(qmd, out_name, output_format, what, on_done, on_error, time) {
      if (shiny::isolate(rendering())) {
        on_error("Another step (reading, processing, export or a report) is running. Please wait until it has finished.")
        return(invisible())
      }
      params_file <- tryCatch(
        write_session_params(shiny::reactiveValuesToList(data_info), ws),
        error = function(e) {
          on_error(conditionMessage(e))
          NULL
        }
      )
      if (is.null(params_file)) return(invisible())
      rendering(TRUE)
      key <- if (output_format == "pdf") "external" else "internal"
      progress[[key]](list(value = 0.02, text = "Starting Quarto ..."))
      job <- list(
        reports_dir = normalizePath("reports", winslash = "/", mustWork = TRUE),
        work_dir = file.path(ws$base, "render"),
        qmd = qmd,
        output_format = output_format,
        out_name = out_name,
        out_dir = ws$report,
        execute_params = list(
          norm_method = shiny::isolate(norm_method()),
          interactive_plot = FALSE,
          project_title = shiny::isolate(data_info[["project_title"]]),
          # The same time as in the file name, shown at the top of the report
          generated_at = format(time, "%Y-%m-%d %H:%M:%S %Z"),
          # Generated on SciLifeLab Serve: the reports acknowledge the hosting
          on_serve = shiny::isolate(is_on_serve(session))
        )
      )
      run_bg_rscript(
        session, ws,
        script = "scripts/render_report.R",
        job = job,
        env = c(QUANTA_PARAMS_FILE = params_file),
        progress_message = paste0("Rendering ", what, " (this can take a minute)"),
        on_progress = function(log) progress[[key]](report_progress(log, pdf = output_format == "pdf")),
        on_done = function(ok, log) {
          rendering(FALSE)
          progress[[key]](NULL)
          path <- file.path(ws$report, out_name)
          if (ok && file.exists(path)) on_done(path) else on_error(bg_error_message(log))
        }
      )
    }

    shiny::observeEvent(input$gen_internal, {
      shiny::req(norm_method())
      msg("")
      time <- Sys.time()
      out_name <- report_name("internal.html", time)
      render_report(
        "report-internal.qmd", out_name, "html", "internal report",
        on_done = function(path) {
          # Keep only the latest internal report of the session
          old <- shiny::isolate(report_path())
          if (nzchar(old) && !identical(old, path)) unlink(old)
          report_path(path)
          report_ready(Sys.time())
          msg(paste0("Internal report `", out_name, "` generated."))
        },
        on_error = function(e) msg(paste("Error:", e)),
        time = time
      )
    })
    output$gen_internal_out <- shiny::renderText(msg())

    # ---- External report (PDF), shown in the app when generated ----
    msg_external <- shiny::reactiveVal("")
    external_path <- shiny::reactiveVal(NULL)
    external_ready <- shiny::reactiveVal(NULL)   # Changes every time a PDF report was rendered
    shiny::observeEvent(input$gen_external, {
      shiny::req(norm_method())
      msg_external("")
      # The PDF report is about the quantified compounds: say why it cannot be made instead of failing
      reason <- if (is.null(no_external_reason)) NULL else tryCatch(no_external_reason(), error = function(e) NULL)
      if (!is.null(reason)) {
        msg_external(paste("The external (PDF) report shows the quantified compounds, so it needs calibration.",
                           reason, "The internal report covers this dataset."))
        return(invisible())
      }
      time <- Sys.time()
      out_name <- report_name("external.pdf", time)
      render_report(
        "report-external.qmd", out_name, "pdf", "external report",
        time = time,
        on_done = function(path) {
          old <- shiny::isolate(external_path())
          if (!is.null(old) && !identical(old, path)) unlink(old)   # Keep only the latest
          external_path(path)
          external_ready(Sys.time())   # Shown in the "External report" tab of the app
          msg_external(paste0("External report `", out_name, "` generated."))
        },
        on_error = function(e) msg_external(paste("Error:", e))
      )
    })
    output$gen_external_out <- shiny::renderText(msg_external())

    # Forget the reports (their files are removed with all results, e.g. when a parameters file is
    # loaded): no report shown or downloaded until a new one is generated
    reset <- function() {
      report_path("")
      report_ready(NULL)
      external_path(NULL)
      external_ready(NULL)
      msg("")
      msg_external("")
    }

    list(report_path = report_path, report_ready = report_ready, external_path = external_path,
         external_ready = external_ready, reset = reset)
  })
}
