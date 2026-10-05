# ------------------------------------------------------------------------------------------- #
# Save / load the session data as one .rds file (UI and server; the data handling itself is in
# projlib/session.R and runs in a background process, scripts/session_data.R, so that large
# files do not freeze the app for other users)
# ------------------------------------------------------------------------------------------- #

#' A button to download the session data (place below "Process data")
#'
#' @param uiId The ID of the module. It should match [sessionDataServer()].
#' @export
sessionSaveUI <- function(uiId) {
  ns <- shiny::NS(uiId)
  shiny::tagList(
    shiny::br(),
    shiny::actionButton(ns("save"), "Save session data (.rds)", icon = shiny::icon("download")),
    # Hidden link that the server "clicks" (message `quantaClickDownload`, defined in app.R)
    # once the file has been prepared
    shiny::downloadLink(ns("download"), label = "", style = "display: none;")
  )
}

#' A file input to load saved session data (place above "Load parameters file")
#'
#' @inheritParams sessionSaveUI
#' @export
sessionLoadUI <- function(uiId) {
  ns <- shiny::NS(uiId)
  shiny::tagList(
    shiny::strong("Load saved session data (optional)"),
    shiny::fileInput(
      ns("load"),
      label = NULL,
      buttonLabel = "Load session data",
      placeholder = "No file loaded",
      accept = ".rds",
      width = "100%"
    )
  )
}

#' Server of the session data save / load
#'
#' @param serverId The ID of the module. It should match the UI IDs.
#' @param data_info The `reactiveValues` returned by [msdialInfoServer()].
#' @param ws The session workspace from [create_session_workspace()].
#' @param processing_params A reactive returning the current processing options as a list.
#'
#' @returns A reactive that changes every time session data has been loaded (use it to
#'   invalidate the reactives that read the data files).
#' @export
sessionDataServer <- function(serverId, data_info, ws, processing_params) {
  shiny::moduleServer(serverId, function(input, output, session) {
    loaded_at <- shiny::reactiveVal(NULL)
    busy <- shiny::reactiveVal(FALSE)   # One save / load at a time per session
    saved_file <- shiny::reactiveVal(NULL)

    start <- function(job, message, on_done) {
      if (shiny::isolate(busy())) {
        shiny::showNotification("Please wait until the session data has been saved / loaded.",
                                type = "warning")
        return(invisible())
      }
      busy(TRUE)
      run_bg_rscript(session, ws, "scripts/session_data.R", job, message,
                     on_done = function(ok, log) {
                       busy(FALSE)
                       on_done(ok, log)
                     })
    }

    # ---- Save: prepare the file in the background, then start the download ----
    shiny::observeEvent(input$save, {
      info <- shiny::reactiveValuesToList(data_info)
      input_file <- info[["input_file"]]
      if (is.null(input_file) || !nzchar(input_file) || !file.exists(input_file)) {
        shiny::showNotification("Nothing to save yet: upload and read an MS-DIAL file first.",
                                type = "error")
        return()
      }
      nm <- info[["input_file_name"]]
      base <- if (is.null(nm) || !nzchar(nm)) "QuaNTA" else tools::file_path_sans_ext(nm)
      out_name <- paste0(base, "_session_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
      out_file <- file.path(ws$base, "session_download", out_name)
      unlink(dirname(out_file), recursive = TRUE)
      dir.create(dirname(out_file), recursive = TRUE)
      job <- list(
        mode = "save",
        input_file = input_file,
        input_file_name = nm,
        intermediate_dir = ws$intermediate,
        user_params = info[c(.text_param_ids, .textarea_param_ids)],
        processing = processing_params(),
        out_file = out_file
      )
      start(job, "Preparing the session data file ...", function(ok, log) {
        if (ok && file.exists(out_file)) {
          saved_file(out_file)
          session$sendCustomMessage("quantaClickDownload", session$ns("download"))
        } else {
          shiny::showNotification(paste("Saving failed:", bg_error_message(log)), type = "error",
                                  duration = NULL)
        }
      })
    })
    output$download <- shiny::downloadHandler(
      filename = function() basename(shiny::req(saved_file())),
      content = function(file) file.copy(shiny::req(saved_file()), file, overwrite = TRUE)
    )
    # The link is hidden; without this Shiny would not give it a download URL
    shiny::outputOptions(output, "download", suspendWhenHidden = FALSE)

    # ---- Load: check and restore in the background, then update the inputs ----
    shiny::observeEvent(input$load, {
      result_file <- file.path(ws$base, "session_load_result.rds")
      unlink(result_file)
      job <- list(
        mode = "load",
        file = input$load$datapath,
        input_dir = ws$input,
        intermediate_dir = ws$intermediate,
        tables_dir = ws$tables,
        result_file = result_file
      )
      file_name <- input$load$name
      start(job, "Checking and loading the session data ...", function(ok, log) {
        res <- if (ok && file.exists(result_file)) readRDS(result_file)
        unlink(result_file)
        if (is.null(res)) {
          shiny::showNotification(paste("Loading failed:", bg_error_message(log)), type = "error",
                                  duration = NULL)
          return()
        }
        if (length(res$problems) > 0) {
          shiny::showModal(shiny::modalDialog(
            title = "This file cannot be loaded",
            shiny::p("It is not valid QuaNTA session data:"),
            shiny::tags$ul(lapply(res$problems, shiny::tags$li)),
            easyClose = TRUE
          ))
          return()
        }

        # Restore the inputs (the IDs of the msdialInfo module are "data_info-<id>")
        root <- session$rootScope()
        for (id in .text_param_ids) {
          v <- res$user_params[[id]]
          if (!is.null(v)) shiny::updateTextInput(root, paste0("data_info-", id), value = as.character(v))
        }
        v <- res$user_params[["free_text"]]
        if (!is.null(v)) shiny::updateTextAreaInput(root, "data_info-free_text", value = as.character(v))
        if (length(res$processing) > 0) {
          data_info$loaded_processing <- c(res$processing, list(.nonce = as.numeric(Sys.time())))
        }
        data_info$input_file <- res$input_file
        data_info$input_file_name <- res$input_file_name
        loaded_at(Sys.time())

        shiny::showNotification(
          paste0("Session data loaded from ", file_name,
                 if (isTRUE(res$processed)) " (read and processed)." else " (read, not yet processed)."),
          type = "message", duration = 8
        )
      })
    })

    loaded_at
  })
}
