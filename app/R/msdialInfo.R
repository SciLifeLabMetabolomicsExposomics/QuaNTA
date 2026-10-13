# ------------------------------------------------------------------------------------------- #
# A Shiny module to collect the user's project / MS-DIAL information
#   - values are typed into inputs (or pre-filled from an uploaded parameter file)
#   - the MS-DIAL file is uploaded (not browsed on the server)
#   - directories are per-session and not chosen by the user
# ------------------------------------------------------------------------------------------- #

.hr <- function() shiny::tags$hr(class = "q-divider")

#' MS-DIAL User Information UI for Shiny
#'
#' @param uiId The ID of the UI module. It should match the ID of [msdialInfoServer()].
#' @returns A Shiny UI module
#' @export
msdialInfoUI <- function(uiId) {
  ns <- shiny::NS(uiId)
  shiny::tagList(
    shiny::strong("Load a saved parameters file (optional)"),
    shiny::fileInput(
      ns("load_params"),
      label = NULL,
      buttonLabel = "Load parameters file",
      placeholder = "No file loaded",
      accept = c(".yml", ".yaml"),
      width = "100%"
    ),
    .hr(),
    shiny::textInput(ns("project_title"), "Project title", value = "", width = "100%"),
    shiny::textInput(ns("user"), "User", value = "", width = "100%"),
    shiny::textInput(ns("university"), "University", value = "", width = "100%"),
    shiny::textAreaInput(ns("free_text"), "Free text to the external report",
                         value = "", width = "100%"),
    .hr(),
    shiny::textInput(ns("concentration_unit"), "Concentration unit", value = "", width = "100%"),
    shiny::strong("MS-DIAL output file"),
    shiny::uiOutput(ns("upload_hint")),   # Mentions the upload limit only on SciLifeLab Serve
    shiny::fileInput(
      ns("input_file_upload"),
      label = NULL,
      buttonLabel = "Browse",
      placeholder = "No file selected",
      width = "100%"
    ),
    shiny::verbatimTextOutput(ns("input_file_out"), placeholder = TRUE)
    # The download of the parameters is at the end of the processing settings (app.R):
    # msdialInfoDownloadUI()
  )
}

#' Button to download the parameters and processing settings (a `.yml` file that "Load parameters
#' file" reads), for placement outside the module's UI
#'
#' @inheritParams msdialInfoUI
#' @export
msdialInfoDownloadUI <- function(uiId) {
  shiny::downloadButton(shiny::NS(uiId)("download_params"),
                        "Download parameters and processing settings (.yml)")
}

#' MS-DIAL User Information Server for Shiny
#'
#' @param serverId The ID of the server module. It should match the UI ID.
#' @param ws The session workspace from [create_session_workspace()].
#' @param processing_params A reactive returning the current processing options as a list. Only
#'   used to include them in the downloaded parameter file.
#'
#' @returns A `reactiveValues` object with the parameters (`project_title`, `user`, `university`,
#'   `free_text`, `input_file`, `concentration_unit`, `intermediate_dir`, `table_dir`,
#'   `report_dir`). `data_info$loaded_processing` is set when an uploaded parameter file contains
#'   a `processing:` section.
#' @export
msdialInfoServer <- function(serverId, ws, processing_params = shiny::reactive(list())) {
  shiny::moduleServer(serverId, function(input, output, session) {
    init <- lapply(rlang::set_names(.user_param_ids), function(x) "")
    init$intermediate_dir <- ws$intermediate
    init$table_dir <- ws$tables
    init$report_dir <- ws$report
    data_info <- do.call(shiny::reactiveValues, init)

    # Text inputs -> data_info
    for (id in c(.text_param_ids, .textarea_param_ids)) {
      local({
        id <- id
        shiny::observeEvent(input[[id]], {
          data_info[[id]] <- input[[id]]
        }, ignoreNULL = FALSE)
      })
    }

    # MS-DIAL file upload: keep the original file name (used to name reports and tables).
    # Compressed uploads (.gz, .zip) are decompressed in a background process.
    shiny::observeEvent(input$input_file_upload, {
      up <- input$input_file_upload
      # A new file makes all earlier results stale (data, exported tables, reports)
      unlink(list.files(ws$input, full.names = TRUE))
      unlink(list.files(ws$intermediate, full.names = TRUE))
      unlink(list.files(ws$tables, full.names = TRUE), recursive = TRUE)
      unlink(list.files(ws$report, full.names = TRUE), recursive = TRUE)

      if (tolower(tools::file_ext(up$name)) %in% c("gz", "zip")) {
        data_info$input_file <- ""   # Not usable until decompressed ("Read" stays disabled)
        data_info$input_file_name <- paste(up$name, "(decompressing ...)")
        result_file <- file.path(ws$base, "decompress_result.rds")
        unlink(result_file)
        job <- list(src = up$datapath, name = up$name, out_dir = ws$input, result_file = result_file)
        run_bg_rscript(
          session, ws, "scripts/decompress_input.R", job,
          progress_message = paste("Decompressing", up$name),
          on_done = function(ok, log) {
            res <- if (ok && file.exists(result_file)) readRDS(result_file)
            unlink(result_file)
            if (is.null(res)) {
              data_info$input_file_name <- paste(up$name, "(could not be decompressed)")
              shiny::showNotification(paste("The uploaded file could not be decompressed:",
                                            bg_error_message(log)), type = "error", duration = NULL)
              return()
            }
            data_info$input_file <- res$path
            data_info$input_file_name <- res$name
            shiny::showNotification(paste("Decompressed", up$name, "to", res$name), type = "message")
          }
        )
        return()
      }

      safe_name <- gsub("[^A-Za-z0-9._-]", "_", basename(up$name))
      dest <- file.path(ws$input, safe_name)
      file.copy(up$datapath, dest, overwrite = TRUE)
      data_info$input_file <- normalizePath(dest, winslash = "/", mustWork = TRUE)
      data_info$input_file_name <- up$name
    }, priority = 100)   # Before the outputs re-read the (deleted) data files
    output$upload_hint <- shiny::renderUI(upload_hint(session))
    output$input_file_out <- shiny::renderText({
      nm <- data_info$input_file_name
      if (is.null(nm) || !nzchar(nm)) "No file uploaded" else nm
    })

    # Load parameters file -> update the inputs
    shiny::observeEvent(input$load_params, {
      p <- tryCatch(
        read_params_file(input$load_params$datapath),
        error = function(e) {
          shiny::showNotification(conditionMessage(e), type = "error")
          NULL
        }
      )
      shiny::req(p)
      for (id in .text_param_ids) {
        if (!is.null(p[[id]])) shiny::updateTextInput(session, id, value = as.character(p[[id]]))
      }
      if (!is.null(p[["free_text"]])) {
        shiny::updateTextAreaInput(session, "free_text", value = as.character(p[["free_text"]]))
      }
      if (is.list(p[["processing"]])) {
        # The nonce makes loading the same file twice trigger the observer again
        data_info$loaded_processing <- c(p[["processing"]], list(.nonce = as.numeric(Sys.time())))
      }
      shiny::showNotification(
        paste("Parameters loaded from", input$load_params$name,
              "- the MS-DIAL file must still be uploaded."),
        type = "message"
      )
    })

    # Download the current parameters
    output$download_params <- shiny::downloadHandler(
      filename = function() paste0("quanta_parameters_", format(Sys.Date(), "%Y%m%d"), ".yml"),
      content = function(file) {
        info <- shiny::reactiveValuesToList(data_info)
        p <- info[c(.text_param_ids, .textarea_param_ids)]
        yaml::write_yaml(c(p, list(processing = processing_params())), file)
      }
    )

    data_info
  })
}

#' MS-DIAL User Information App for modular testing
msdialInfoApp <- function() {
  ui <- shiny::fluidPage(msdialInfoUI("data_info"))
  server <- function(input, output, session) {
    ws <- create_session_workspace(session)
    msdialInfoServer("data_info", ws = ws)
  }
  shiny::shinyApp(ui, server)
}
