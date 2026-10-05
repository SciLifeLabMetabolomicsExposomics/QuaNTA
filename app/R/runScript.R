# ------------------------------------------------------------------------------------------- #
# Shiny modules to run an Rscript in a child process and display its output
#
# On every click:
#   1. the current Shiny inputs are written to a private per-session params file
#   2. the script is started as a child process whose environment variable
#      `QUANTA_PARAMS_FILE` points to that file (read by `msdial$get_user_input()`)
# The Shiny process is never blocked, so other users' sessions stay responsive.
# ------------------------------------------------------------------------------------------- #

#' A Shiny UI module to run a script and display the output
#'
#' @param uiId A string that identifies the UI module.
#' @param label A string that specifies the label for the action button.
#' @export
runScriptUI <- function(uiId, label, download=FALSE) {
  ns <- shiny::NS(uiId)
  shiny::tagList(
    shiny::actionButton(ns("run_button"), label),
    shiny::verbatimTextOutput(ns("script_output"), placeholder = TRUE),
    shiny::br(),
    if(download){
      shiny::downloadButton(ns("download_tables"), "Download exported tables (.zip)")
    }
  )
}

# Shared implementation --------------------------------------------------------------------

.runScriptCore <- function(serverId, script_path, what, param_ids, data_info, ws, extra_args) {
  shiny::moduleServer(serverId, function(input, output, session) {
    output_txt <- shiny::reactiveVal("")
    proc <- shiny::reactiveVal(NULL)
    done_at <- shiny::reactiveVal(NULL)   # Set every time a run has finished

    shiny::observeEvent(input[["run_button"]], {
      old <- proc()
      if (!is.null(old) && old$is_alive()) {
        output_txt(paste0(output_txt(), "\nA job is already running. Please wait until it finishes.\n"))
        return()
      }
      tryCatch({
        # 1. Hand the current inputs over to the script
        params_file <- write_session_params(shiny::reactiveValuesToList(data_info), ws)

        # 2. Script arguments from this module's inputs (+ optional extra ones)
        script <- normalizePath(script_path, mustWork = TRUE)
        args <- script
        for (param_id in param_ids) {
          v <- input[[param_id]]
          if (!is.null(v)) args <- c(args, paste0("--", param_id, "=", v))
        }
        if (!is.null(extra_args)) args <- c(args, extra_args())

        # 3. Child process with its own environment
        env <- c(unlist(as.list(Sys.getenv())),
                 QUANTA_PARAMS_FILE = params_file,
                 R_BOX_PATH = find_box_root())
        env <- env[!duplicated(names(env), fromLast = TRUE)]
        p <- processx::process$new(
          command = "Rscript",
          args = args,
          stdout = "|",
          stderr = "|",
          poll_connection = TRUE,
          wd = "..",          # Every script is written to be executed at the project root
          env = env
        )
        proc(p)
        output_txt(paste(what, "is running...\nTime:", Sys.time(), "\n"))
      }, error = function(e) {
        output_txt(paste("Error running script:", conditionMessage(e), sep = "\n"))
        proc(NULL)
      })
    })

    # Poll the child process only while one is running
    shiny::observe({
      p <- proc()
      shiny::req(p)
      shiny::invalidateLater(1000, session)
      alive <- p$is_alive()
      if (alive) {
        out <- p$read_output_lines()
        err <- p$read_error_lines()
      } else {
        out <- p$read_all_output_lines()
        err <- p$read_all_error_lines()
      }
      shiny::isolate({
        txt <- paste0(output_txt(), "#")
        lines <- c(out, err)
        if (length(lines) > 0) txt <- paste(txt, paste(lines, collapse = "\n"), "", sep = "\n")
        if (!alive) {
          status <- p$get_exit_status()
          txt <- paste(
            txt,
            if (identical(status, 0L)) {
              "Finished."
            } else {
              paste0("Script ended with exit status ", status, ". See the messages above. ",
                     "(Non-target data intentionally stops before calibration.)")
            },
            sep = "\n"
          )
          proc(NULL)
          done_at(Sys.time())
        }
        output_txt(txt)
      })
    })

    # Do not leave orphan processes behind when the user closes the browser tab
    session$onSessionEnded(function() {
      p <- shiny::isolate(proc())
      if (!is.null(p) && p$is_alive()) p$kill()
    })

    output[["script_output"]] <- shiny::renderText(output_txt())

    done_at
  })
}

#' Locate the folder that contains `projlib/msdial.R` (and `SumExp`), whatever it is called
#' (`code/`, `app/`, ...). Passed to the child processes as an absolute path so it does not
#' depend on their working directory.
#' @export
find_box_root <- function() {
  cands <- c(".", "..", "code", "../code", "app", "../app")
  hit <- cands[file.exists(file.path(cands, "projlib", "msdial.R"))]
  if (length(hit) == 0) {
    stop("Could not find `projlib/msdial.R` in any of: ", paste(cands, collapse = ", "),
         " (relative to ", getwd(), ").", call. = FALSE)
  }
  normalizePath(hit[1], winslash = "/", mustWork = TRUE)
}

#' A Shiny server module to run a script and display the output
#'
#' @param serverId A string that identifies the server module.
#' @param script_path Path to the script to be run (relative to the app directory).
#' @param what Context / purpose of the script, used in messages.
#' @param param_ids IDs of this module's inputs passed to the script as `--id=value`.
#' @param data_info The `reactiveValues` returned by [msdialInfoServer()].
#' @param ws The session workspace from [create_session_workspace()].
#'
#' @returns A reactive that changes every time a run has finished (use it to invalidate
#'   downstream reactives that load the results).
#' @export
runScriptServer <- function(serverId, script_path, what, param_ids = NULL, data_info, ws) {
  .runScriptCore(serverId, script_path, what, param_ids, data_info, ws, extra_args = NULL)
}

#' Same as [runScriptServer()] for the export script, with the normalization method and a zip download
#'
#' @inheritParams runScriptServer
#' @param norm_method A reactive with the matrix ID of the normalization method to export
#'   (e.g. `"loess_norm"`), or `"auto"` for the best one determined by `process.R`
#' @export
runScriptServer2 <- function(serverId, script_path, what, param_ids = NULL, data_info, ws,
                             norm_method) {
  extra_args <- function() paste0("--norm_method=", shiny::isolate(norm_method()))
  done_at <- .runScriptCore(serverId, script_path, what, param_ids, data_info, ws, extra_args)

  # Second module server with the same ID: the outputs share the namespace of the UI
  shiny::moduleServer(serverId, function(input, output, session) {
    output$download_tables <- shiny::downloadHandler(
      filename = function() {
        paste0("QuaNTA_tables_", norm_method(), "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
      },
      content = function(file) {
        # Only the tables of the selected method: "(base name).(method).(norm|norm_blk|conc).xlsx"
        method <- sub("_norm$", "", norm_method())
        pattern <- paste0("\\.", method, "\\.(norm|norm_blk|conc)\\.xlsx$")
        files <- list.files(ws$tables, pattern = pattern, full.names = TRUE)
        if (length(files) == 0) stop("No exported tables of this method yet. Run the export first.")
        zip::zip(zipfile = file, files = files, mode = "cherry-pick")
      }
    )
  })
  done_at
}

#' A Shiny app to run a script and display the output, for testing purposes
runScriptApp <- function() {
  ui <- shiny::fluidPage(runScriptUI("script1", "Run script 1"))
  server <- function(input, output, session) {
    ws <- create_session_workspace(session)
    data_info <- msdialInfoServer("data_info", ws = ws)
    runScriptServer("script1", "scripts/read-msdial.R", "Read MS-Dial", data_info = data_info, ws = ws)
  }
  shiny::shinyApp(ui = ui, server = server)
}
