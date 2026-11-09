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

#' Whether a step of the session is running (reading, processing, export or a report), as one
#' `reactiveVal` shared by all modules of the session: the steps read and write the same files,
#' so only one runs at a time
#'
#' @param session A (module) Shiny session
#' @export
session_busy <- function(session) {
  if (is.null(session$userData$quanta_busy)) session$userData$quanta_busy <- shiny::reactiveVal(FALSE)
  session$userData$quanta_busy
}

.runScriptCore <- function(serverId, script_path, what, param_ids, data_info, ws, extra_args,
                           clear = NULL, trigger = NULL) {
  shiny::moduleServer(serverId, function(input, output, session) {
    busy <- session_busy(session)
    output_txt <- shiny::reactiveVal("")
    proc <- shiny::reactiveVal(NULL)
    done_at <- shiny::reactiveVal(NULL)   # Set every time a run has finished
    started_at <- shiny::reactiveVal(NULL)   # To report the elapsed time
    # Log time in front of each non-empty line
    stamp <- function(lines) ifelse(nzchar(trimws(lines)), paste0(format(Sys.time(), "[%H:%M:%S]  "), lines), "")

    # "Clear log" (a reactive from the app, e.g. a button); a running job keeps logging
    if (!is.null(clear)) shiny::observeEvent(clear(), output_txt(""), ignoreInit = TRUE)

    # The run starts on the button, or on `trigger()` (e.g. after a confirmation popup)
    shiny::observeEvent(if (is.null(trigger)) input[["run_button"]] else trigger(), {
      if (busy()) {
        output_txt(paste0(output_txt(), "\nAnother step (reading, processing, export or a report) is running. ",
                          "Please wait until it finishes.\n"))
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
        busy(TRUE)
        started_at(Sys.time())
        output_txt(paste0(stamp(paste(what, "is running ...")), "\n"))
      }, error = function(e) {
        output_txt(paste0(stamp(paste("Error running script:", conditionMessage(e))), "\n"))
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
        # New lines with the time they were received
        lines <- c(out, err)
        if (!alive) {
          status <- p$get_exit_status()
          lines <- c(lines,
                     if (identical(status, 0L)) "Finished." else
                       paste0("Script ended with exit status ", status, ". See the messages above."),
                     paste("Elapsed time:", format_elapsed(started_at())))
          proc(NULL)
          busy(FALSE)
          done_at(Sys.time())
        }
        if (length(lines) > 0) output_txt(add_log_lines(output_txt(), stamp(lines), lines))
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

#' Add lines to a log text. A progress bar line ("[####------]  40%", e.g. of process.R) replaces
#' the progress bar line just before it, so that the bar advances in place.
#'
#' @param txt The log so far (lines ending with "\n")
#' @param stamped The new lines as shown (e.g. with the time)
#' @param lines The new lines as printed (to recognise the progress bars)
#' @export
add_log_lines <- function(txt, stamped, lines) {
  is_bar <- function(x) grepl("^\\s*\\[[#-]+\\]\\s+[0-9]+%\\s*$", x)
  last_is_bar <- grepl("\\[[#-]+\\]\\s+[0-9]+%\\s*\n$", txt)
  for (i in seq_along(lines)) {
    if (is_bar(lines[i]) && last_is_bar) {
      txt <- sub("[^\n]*\n$", paste0(stamped[i], "\n"), txt)   # Replace the previous bar
    } else {
      txt <- paste0(txt, stamped[i], "\n")
    }
    last_is_bar <- is_bar(lines[i])
  }
  txt
}

#' Time elapsed since `start`, e.g. "2 min 35 s"
#'
#' @param start A `POSIXct` time
#' @export
format_elapsed <- function(start) {
  if (is.null(start)) return("unknown")
  secs <- round(as.numeric(difftime(Sys.time(), start, units = "secs")))
  if (secs < 60) return(paste(secs, "s"))
  h <- secs %/% 3600
  m <- (secs %% 3600) %/% 60
  s <- secs %% 60
  if (h > 0) paste(h, "h", m, "min", s, "s") else paste(m, "min", s, "s")
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
#' @param clear Optional reactive (e.g. a "Clear log" button); its changes empty the log.
#' @param trigger Optional reactive that starts the run instead of the module's "run_button"
#'   (ignore its initial value: `NULL` or 0).
#'
#' @returns A reactive that changes every time a run has finished (use it to invalidate
#'   downstream reactives that load the results).
#' @export
runScriptServer <- function(serverId, script_path, what, param_ids = NULL, data_info, ws,
                            clear = NULL, trigger = NULL) {
  .runScriptCore(serverId, script_path, what, param_ids, data_info, ws, extra_args = NULL,
                 clear = clear, trigger = trigger)
}

#' Same as [runScriptServer()] for the export script, with the normalization method and a zip download
#'
#' @inheritParams runScriptServer
#' @param norm_method A reactive with the matrix ID of the normalization method to export
#'   (e.g. `"loess_norm"`), or `"auto"` for the best one determined by `process.R`
#' @export
runScriptServer2 <- function(serverId, script_path, what, param_ids = NULL, data_info, ws,
                             norm_method) {
  extra_args <- function() c(paste0("--norm_method=", shiny::isolate(norm_method())), serve_export_args())
  done_at <- .runScriptCore(serverId, script_path, what, param_ids, data_info, ws, extra_args)

  # Second module server with the same ID: the outputs share the namespace of the UI
  shiny::moduleServer(serverId, function(input, output, session) {
    output$download_tables <- shiny::downloadHandler(
      filename = function() {
        paste0("QuaNTA_tables_", norm_method(), "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
      },
      content = function(file) {
        # Only the tables of the selected method:
        # "(base name).(method).(norm|norm_blk|conc|conc_all|history).xlsx" and their CSV copies
        # "(base name).(method).(norm|norm_blk|conc|conc_all|history).(sheet).csv"
        method <- sub("_norm$", "", norm_method())
        pattern <- paste0("\\.", method, "\\.(norm|norm_blk|conc|conc_all|history)(\\.[^.]+)?\\.(xlsx|csv)$")
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
