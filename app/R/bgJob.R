# ------------------------------------------------------------------------------------------- #
# Run an R script as a background (child) process of one session
#
# Long tasks (rendering reports, saving / loading session data) must not run in the app's own R
# process: on SciLifeLab Serve that process is shared by all users and R is single-threaded, so a
# long task would freeze the app for everyone. The child process gets its task as a "job" .rds
# file in the session's private workspace and writes its log there too.
# ------------------------------------------------------------------------------------------- #

#' Start an R script in a background process and call `on_done()` when it has finished
#'
#' @param session The (module) Shiny session.
#' @param ws The session workspace from [create_session_workspace()].
#' @param script Path of the R script (relative to the app directory). It is called as
#'   `Rscript <script> --job=<job.rds>`.
#' @param job A list saved as the job file for the script.
#' @param progress_message Text of the progress notification shown while the process runs.
#' @param on_done A function `function(ok, log)` called in the app when the process has finished;
#'   `ok` is `TRUE` for exit status 0 and `log` holds the output lines of the process.
#' @param env A named character vector of extra environment variables for the process.
#' @param on_progress `NULL`, or a function `function(log)` called every second while the process
#'   runs, with the output lines so far (e.g. to show the progress of a report in the page). The
#'   progress notification is then not shown.
#' @returns The `processx::process` object, invisibly.
#' @export
run_bg_rscript <- function(session, ws, script, job, progress_message, on_done, env = character(),
                           on_progress = NULL) {
  jobs_dir <- file.path(ws$base, "jobs")
  dir.create(jobs_dir, showWarnings = FALSE)
  id <- paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_", sample.int(1e6, 1))
  job_file <- file.path(jobs_dir, paste0(id, ".rds"))
  log_file <- file.path(jobs_dir, paste0(id, ".log"))
  saveRDS(job, job_file)

  app_dir <- find_box_root()
  full_env <- c(unlist(as.list(Sys.getenv())), R_BOX_PATH = app_dir, APP_ROOT = app_dir, env)
  full_env <- full_env[!duplicated(names(full_env), fromLast = TRUE)]
  p <- processx::process$new(
    command = "Rscript",
    args = c(normalizePath(script, mustWork = TRUE), paste0("--job=", job_file)),
    stdout = log_file,
    stderr = "2>&1",          # Both into the log file (a pipe could fill up and stall the process)
    wd = ws$base,             # Any stray file stays in the session's private workspace
    env = full_env,
    cleanup_tree = TRUE
  )
  session$onSessionEnded(function() if (p$is_alive()) p$kill_tree())

  progress <- if (is.null(on_progress)) shiny::Progress$new(session)
  if (!is.null(progress)) progress$set(message = progress_message, value = 0.3)

  # Poll the process without blocking the app
  watcher <- shiny::observe({
    shiny::invalidateLater(1000, session)
    if (p$is_alive()) {
      if (!is.null(on_progress) && file.exists(log_file)) {
        on_progress(tryCatch(readLines(log_file, warn = FALSE), error = function(e) character(0)))
      }
      return()
    }
    watcher$destroy()
    if (!is.null(progress)) progress$close()
    ok <- identical(p$get_exit_status(), 0L)
    log <- if (file.exists(log_file)) readLines(log_file, warn = FALSE) else character(0)
    unlink(c(job_file, log_file))
    on_done(ok, log)
  })
  invisible(p)
}

#' The error message of a failed background process, from its log
#'
#' For a report, also where it failed: knitr's "Quitting from report-internal.qmd:912-918 [label]"
#' line(s), which hold only file names, line numbers and chunk labels (no data).
#'
#' @param log The log lines passed to `on_done()` of [run_bg_rscript()].
#' @export
bg_error_message <- function(log) {
  log <- gsub("\033\\[[0-9;]*m", "", log)          # Without colour codes
  log <- log[!grepl("^Execution halted", log)]   # Rscript's closing line adds nothing for users
  where <- unique(trimws(sub(".*Quitting from\\s*", "", grep("Quitting from", log, value = TRUE))))
  where <- if (length(where) > 0) paste0(" (where: ", paste(where, collapse = "; "), ")") else ""
  i <- grep("^Error|error:", log)
  if (length(i) == 0) return(paste0(paste(utils::tail(log, 3), collapse = " "), where))
  msg <- paste(log[i[1]:min(length(log), i[1] + 2)], collapse = " ")
  paste0(sub("^Error( in [^:]*)?:\\s*", "", msg), where)   # "Error: ..." -> "..."
}
