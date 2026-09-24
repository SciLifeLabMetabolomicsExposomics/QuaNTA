# ------------------------------------------------------------------------------------------- #
# A Shiny module to generate a report (rendered into the per-session report directory)
# ------------------------------------------------------------------------------------------- #

#' A Shiny UI module to generate a report
#'
#' @param uiId A string that identifies the UI module.
#' @export
genReportUI <- function(uiId) {
  ns <- shiny::NS(uiId)
  shiny::tagList(
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
#'
#' @returns A list with the reactives `report_path` (absolute path of the report, which may not
#'   exist yet) and `report_ready` (changes every time a report was rendered).
#' @export
genReportServer <- function(serverId, data_info, ws, norm_method) {
  shiny::moduleServer(serverId, function(input, output, session) {
    msg <- shiny::reactiveVal("")
    report_ready <- shiny::reactiveVal(NULL)

    # e.g. "Dummy_Test-loess_norm-internal.html"
    report_name <- shiny::reactive({
      shiny::req(data_info[["input_file"]], nzchar(data_info[["input_file"]]), norm_method())
      base <- tools::file_path_sans_ext(basename(data_info[["input_file"]]), compression = TRUE)
      paste0(base, "-", norm_method(), "-internal.html")
    })
    report_path <- shiny::reactive(file.path(ws$report, report_name()))

    shiny::observeEvent(input$gen_internal, {
      shiny::req(norm_method())
      msg("")
      out_name <- report_name()
      msg(tryCatch({
        # Same hand-over as for the processing scripts. The env var is inherited by the quarto
        # child process. `quarto_render()` blocks, so no other session can interleave.
        params_file <- write_session_params(shiny::reactiveValuesToList(data_info), ws)
        reports_dir <- normalizePath("reports", mustWork = TRUE)
        rel_out <- as.character(fs::path_rel(ws$report, start = reports_dir))

        shiny::withProgress(message = "Rendering report (this can take a minute) ...", value = 0.3, {
          withr::with_envvar(
            c(QUANTA_PARAMS_FILE = params_file),
            withr::with_dir(
              reports_dir,
              quarto::quarto_render(
                "report-internal.qmd",
                output_format = "html",
                output_file = out_name,
                execute_params = rlang::list2(
                  norm_method = norm_method(),
                  interactive_plot = FALSE,
                  project_title = data_info[["project_title"]]
                ),
                quarto_args = c("--output-dir", rel_out)
              )
            )
          )
        })
        report_ready(Sys.time())
        paste0("Internal report `", out_name, "` generated.")
      }, error = function(e) paste("Error:", conditionMessage(e))))
    })
    output$gen_internal_out <- shiny::renderText(msg())

    list(report_path = report_path, report_ready = report_ready)
  })
}
