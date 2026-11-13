# ------------------------------------------------------------------------------------------- #
# A Shiny module to collect the user's project / MS-DIAL information
#   - values are typed into inputs (or pre-filled from an uploaded parameter file)
#   - the MS-DIAL file is uploaded (not browsed on the server)
#   - directories are per-session and not chosen by the user
# ------------------------------------------------------------------------------------------- #

.hr <- function() shiny::tags$hr(class = "q-divider")

# "Read data file" and "Process data" greyed out in the browser as soon as a data file is chosen:
# the server learns of the new file only when its upload has finished, and until then the steps
# would run on the earlier file, which the finished upload then deletes. They are enabled again by
# app.R ("Step buttons are disabled ...") when the upload has arrived. A file stopped by the Serve
# upload limit (R/uploadLimit.R) never reaches this listener (it stops the event first).
.disable_steps_while_uploading <- function(input_id) {
  shiny::tags$script(shiny::HTML(sprintf("
    document.addEventListener('change', function(e) {
      var el = e.target;
      if (!el || el.id !== '%s' || !el.files || el.files.length === 0) return;
      ['read_msdial-run_button', 'proc-run_button'].forEach(function(id) {
        var b = document.getElementById(id);
        if (b) b.disabled = true;
      });
    });
  ", input_id)))
}

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
      buttonLabel = "Load parameters",
      placeholder = "No file loaded",
      accept = c(".yml", ".yaml"),
      width = "100%"
    ),
    .hr(),
    shiny::textInput(ns("project_title"), "Project title", value = "", width = "100%"),
    shiny::textInput(ns("user"), "User", value = "", width = "100%"),
    shiny::textInput(ns("university"), "University", value = "", width = "100%"),
    shiny::textAreaInput(ns("free_text"), "Comments (used for reports)",
                         value = "", width = "100%"),
    .hr(),
    shiny::textInput(ns("concentration_unit"), "Concentration unit", value = "", width = "100%"),
    shiny::strong("Data file"),
    shiny::uiOutput(ns("upload_hint")),   # Mentions the upload limit only on SciLifeLab Serve
    shiny::fileInput(
      ns("input_file_upload"),
      label = NULL,
      buttonLabel = "Browse",
      placeholder = "No file selected",
      # Only the file dialog's filter; the server checks the type too (`upload_type_problem()`)
      accept = c(".txt", ".csv", ".xlsx", ".gz", ".zip"),
      width = "100%"
    ),
    .disable_steps_while_uploading(ns("input_file_upload")),
    shiny::verbatimTextOutput(ns("input_file_out"), placeholder = TRUE)
    # The download of the parameters is at the end of the processing settings (app.R):
    # msdialInfoDownloadUI()
  )
}

#' Button to download the parameters and processing settings (a `.yml` file that "Load parameters
#' file" reads), for placement outside the module's UI
#'
#' @inheritParams msdialInfoUI
#' @param where "settings" (the processing settings, page 2) or "export" (the export box, page 5):
#'   the same file, one button in each place
#' @export
msdialInfoDownloadUI <- function(uiId, label = "Download parameters and processing settings (.yml)",
                                 class = NULL, where = c("settings", "export")) {
  where <- match.arg(where)
  id <- if (where == "settings") "download_params" else "download_params_export"
  shiny::downloadButton(shiny::NS(uiId)(id), label, class = class)
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
#'   a `processing:` section; `data_info$previous_dataset` is the name of the dataset saved in it
#'   (`""` if none).
#' @param on_params_loaded `NULL`, or a function called when a parameters file has been loaded
#'   (e.g. to remove the earlier results, which no longer match the settings)
#' @param export_params A reactive returning the export options as a list (the `export:` section
#'   of the file; their defaults until the user changes them). `data_info$loaded_export` is set
#'   when a loaded parameters file has an `export:` section.
#' @export
msdialInfoServer <- function(serverId, ws, processing_params = shiny::reactive(list()),
                             on_params_loaded = NULL, export_params = shiny::reactive(NULL)) {
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
    # A tab-delimited text file is used as it is. Others are made ready for reading in a background
    # process (scripts/prepare_input.R, projlib/input_file.R): .gz / .zip unpacked, an .xlsx sheet
    # converted to text (the user chooses the sheet when there are several), text separated by
    # semicolons or commas, in UTF-16 or with decimal commas rewritten as tab-delimited text.
    box::use(input_file = projlib/input_file)
    n_uploads <- 0   # A preparation that ends after a newer upload is ignored
    pending_sheet <- shiny::reactiveVal(NULL)   # An .xlsx file waiting for the choice of its sheet
    file_problem_modal <- function(title, ...) {
      shiny::showModal(shiny::modalDialog(
        title = title, ...,
        shiny::p(paste0("Upload ", input_file$accepted_types_text, ". A parameters file (.yml) is loaded ",
                        "with \"Load parameters\" above.")),
        easyClose = TRUE
      ))
    }
    prepare_upload <- function(src, name, sheet, this_upload, out_dir, label, remove_src = FALSE) {
      data_info$input_file <- ""   # Not usable until ready ("Read" stays disabled)
      data_info$input_file_name <- paste(label, "(preparing ...)")
      result_file <- file.path(ws$base, paste0("prepare_result_", this_upload, ".rds"))
      unlink(result_file)
      job <- list(src = src, name = name, out_dir = out_dir, sheet = sheet, result_file = result_file)
      run_bg_rscript(
        session, ws, "scripts/prepare_input.R", job,
        progress_message = paste("Preparing", label),
        on_done = function(ok, log) {
          res <- if (ok && file.exists(result_file)) readRDS(result_file)
          unlink(result_file)
          # Another file was uploaded meanwhile: that one is the data file
          if (this_upload != n_uploads) {
            unlink(out_dir, recursive = TRUE)
            return()
          }
          if (is.null(res)) {
            data_info$input_file_name <- paste(label, "(could not be used)")
            file_problem_modal("The file could not be used", shiny::p(bg_error_message(log)))
            return()
          }
          if (!is.null(res$sheets)) {
            pending_sheet(list(path = res$path, name = res$name, upload = this_upload, out_dir = out_dir,
                               label = label))
            data_info$input_file_name <- paste(label, "(choose a sheet)")
            shiny::showModal(shiny::modalDialog(
              title = "Which sheet has the data?",
              shiny::p(sprintf("The workbook %s has %d sheets. Choose the sheet with the MS-DIAL data to analyse.",
                               res$name, length(res$sheets))),
              shiny::selectInput(session$ns("xlsx_sheet"), "Sheet", choices = res$sheets, width = "100%"),
              easyClose = FALSE,
              footer = shiny::tagList(
                shiny::modalButton("Cancel"),
                shiny::actionButton(session$ns("xlsx_sheet_ok"), "Use this sheet", class = "btn-primary")
              )
            ))
            return()
          }
          if (remove_src && !identical(normalizePath(src, mustWork = FALSE), res$path)) unlink(src)
          data_info$input_file <- res$path
          data_info$input_file_name <- res$name
          if (length(res$notes) > 0) {
            shiny::showNotification(paste(res$notes, collapse = " "), type = "message", duration = 15)
          }
          warn_if_large_upload(res$path, session)
        }
      )
    }
    shiny::observeEvent(input$xlsx_sheet_ok, {
      p <- pending_sheet()
      shiny::removeModal()
      pending_sheet(NULL)
      shiny::req(p, p$upload == n_uploads, input$xlsx_sheet)
      prepare_upload(p$path, p$name, input$xlsx_sheet, p$upload, p$out_dir,
                     paste0(p$label, " [", input$xlsx_sheet, "]"), remove_src = TRUE)
    })

    shiny::observeEvent(input$input_file_upload, {
      n_uploads <<- n_uploads + 1
      this_upload <- n_uploads
      up <- input$input_file_upload
      # Only the accepted types (the file inside a .gz / .zip is checked when it is unpacked);
      # another file keeps the earlier data
      problem <- upload_type_problem(up$name)
      if (!is.null(problem)) {
        file_problem_modal("This file cannot be used as the data file", shiny::p(problem))
        return()
      }
      # A text file: its format from its first lines (quick). Not an MS-DIAL export: the earlier
      # data is kept
      ext <- tolower(tools::file_ext(up$name))
      as_is <- FALSE
      if (ext %in% c("txt", "csv", "tsv")) {
        fmt <- tryCatch(input_file$detect_text_format(up$datapath), error = function(e) e)
        if (inherits(fmt, "error")) {
          file_problem_modal("The file could not be read", shiny::p(conditionMessage(fmt)))
          return()
        }
        as_is <- !fmt$needs_rewrite
      }
      # A new file makes all earlier results stale (data, exported tables, reports)
      unlink(list.files(ws$input, full.names = TRUE), recursive = TRUE)
      unlink(list.files(ws$intermediate, full.names = TRUE))
      unlink(list.files(ws$tables, full.names = TRUE), recursive = TRUE)
      unlink(list.files(ws$report, full.names = TRUE), recursive = TRUE)
      pending_sheet(NULL)

      if (!as_is) {
        # A folder per upload: an older preparation cannot overwrite a newer file of the same name
        out_dir <- file.path(ws$input, this_upload)
        dir.create(out_dir)
        prepare_upload(up$datapath, up$name, NULL, this_upload, out_dir, up$name)
        return()
      }

      safe_name <- gsub("[^A-Za-z0-9._-]", "_", basename(up$name))
      dest <- file.path(ws$input, safe_name)
      file.copy(up$datapath, dest, overwrite = TRUE)
      data_info$input_file <- normalizePath(dest, winslash = "/", mustWork = TRUE)
      data_info$input_file_name <- up$name
      warn_if_large_upload(data_info$input_file, session)
    }, priority = 100)   # Before the outputs re-read the (deleted) data files
    output$upload_hint <- shiny::renderUI(upload_hint(session))
    # The dataset saved in a loaded parameters file; otherwise the uploaded file (also shown in the
    # upload field)
    output$input_file_out <- shiny::renderText({
      prev <- data_info$previous_dataset
      if (isTRUE(nzchar(prev))) return(paste0("Previously processed dataset:\n", prev))
      nm <- data_info$input_file_name
      if (is.null(nm) || !nzchar(nm)) "No file uploaded" else nm
    })

    # Load parameters file -> update the inputs. Earlier results (read data, processing, tables,
    # reports) no longer match the settings: they are removed, after a confirmation when there are
    # any (the uploaded data file stays)
    has_results <- function() {
      length(c(list.files(ws$intermediate), list.files(ws$tables), list.files(ws$report))) > 0
    }
    pending_params <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$load_params, {
      p <- tryCatch(
        read_params_file(input$load_params$datapath),
        error = function(e) {
          shiny::showNotification(conditionMessage(e), type = "error")
          NULL
        }
      )
      shiny::req(p)
      if (!has_results()) return(apply_params(p))
      pending_params(p)
      shiny::showModal(shiny::modalDialog(
        title = "Load the parameters file?",
        shiny::p("Loading a parameters file removes all earlier results of this session: the read data,",
                 "the processing results, the exported tables and the reports, with their tables and figures.",
                 "The uploaded data file stays; read and process it again with the new settings."),
        easyClose = FALSE,
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(session$ns("load_params_confirm"), "Load and remove the results", class = "btn-primary")
        )
      ))
    })
    shiny::observeEvent(input$load_params_confirm, {
      shiny::removeModal()
      p <- pending_params()
      pending_params(NULL)
      shiny::req(p)
      apply_params(p)
    })
    apply_params <- function(p) {
      if (!is.null(on_params_loaded)) on_params_loaded()
      prev <- p[["dataset"]]
      data_info$previous_dataset <- if (length(prev) == 1 && !is.na(prev) && nzchar(prev)) as.character(prev) else ""
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
      # The export options: used when the export box is shown (after the processing)
      if (is.list(p[["export"]])) {
        data_info$loaded_export <- c(p[["export"]], list(.nonce = as.numeric(Sys.time())))
      }
      shiny::showNotification(
        paste("Parameters loaded from", shiny::isolate(input$load_params$name),
              "- read and process the data file (again)."),
        type = "message"
      )
    }

    # Download the current parameters: the user information, the processing settings and the
    # export options (the same file from page 2 and from the export box of page 5)
    params_download <- function() shiny::downloadHandler(
      filename = function() paste0("quanta_parameters_", format_client_time(session, "%Y%m%d"), ".yml"),
      content = function(file) {
        info <- shiny::reactiveValuesToList(data_info)
        p <- info[c(.text_param_ids, .textarea_param_ids)]
        # The name of the uploaded dataset, if any (shown when the file is loaded again)
        if (isTRUE(nzchar(info$input_file)) && isTRUE(nzchar(info$input_file_name))) p$dataset <- info$input_file_name
        ex <- export_params()
        yaml::write_yaml(c(p, list(processing = processing_params()), if (!is.null(ex)) list(export = ex)), file)
      }
    )
    output$download_params <- params_download()
    output$download_params_export <- params_download()

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
