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
  shiny::tagList(
    # External report (PDF): downloaded directly once generated, not shown in the app
    shiny::actionButton(inputId = ns("gen_external"), label = "Generate external report"),
    shiny::textOutput(ns("gen_external_out")),
    # Hidden link that the server "clicks" (message `quantaClickDownload`, defined in app.R) to
    # start the download when the PDF is ready
    shiny::downloadLink(ns("download_external"), label = "", style = "display: none;"),
    shiny::br(),
    shiny::actionButton(inputId = ns("gen_internal"), label = "Generate internal report"),
    shiny::textOutput(ns("gen_internal_out"))
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
#'   `""` before the first one; its file name includes the time of generation) and `report_ready`
#'   (changes every time a report was rendered).
#' @export
genReportServer <- function(serverId, data_info, ws, norm_method, no_external_reason = NULL) {
  shiny::moduleServer(serverId, function(input, output, session) {
    msg <- shiny::reactiveVal("")
    report_ready <- shiny::reactiveVal(NULL)
    rendering <- shiny::reactiveVal(FALSE)   # One render at a time per session

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
        on_error("A report is already being generated. Please wait until it has finished.")
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
        on_done = function(ok, log) {
          rendering(FALSE)
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

    # ---- External report (PDF), downloaded directly when generated ----
    msg_external <- shiny::reactiveVal("")
    external_path <- shiny::reactiveVal(NULL)
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
          # Start the download in the browser
          session$sendCustomMessage("quantaClickDownload", session$ns("download_external"))
          msg_external(paste0("External report `", out_name, "` generated and downloaded."))
        },
        on_error = function(e) msg_external(paste("Error:", e))
      )
    })
    output$gen_external_out <- shiny::renderText(msg_external())
    output$download_external <- shiny::downloadHandler(
      filename = function() basename(shiny::req(external_path())),
      content = function(file) file.copy(shiny::req(external_path()), file, overwrite = TRUE),
      contentType = "application/pdf"
    )
    # The link is hidden; without this Shiny would not give it a download URL
    shiny::outputOptions(output, "download_external", suspendWhenHidden = FALSE)

    list(report_path = report_path, report_ready = report_ready)
  })
}
