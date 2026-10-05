# ------------------------------------------------------------------------------------------- #
# Save / load session data in a child process (started by the app, see R/sessionData.R)
#
# Usage: Rscript session_data.R --job=<path of a job .rds file>
# The job is a list with `mode`:
#   - "save": `input_file`, `input_file_name`, `intermediate_dir`, `user_params`, `processing`
#             -> writes the session data to `out_file`
#   - "load": `file` (the uploaded .rds), `input_dir`, `intermediate_dir`, `tables_dir`
#             -> checks it, restores it into the workspace and writes a summary to `result_file`
# Environment: R_BOX_PATH (app directory)
# ------------------------------------------------------------------------------------------- #

options(box.path = Sys.getenv("R_BOX_PATH", "app/"))
box::use(session = projlib/session)

args <- commandArgs(trailingOnly = TRUE)
job_file <- sub("^--job=", "", grep("^--job=", args, value = TRUE))
stopifnot("Usage: session_data.R --job=<job.rds>" = length(job_file) == 1, file.exists(job_file))
job <- readRDS(job_file)

if (identical(job$mode, "save")) {
  bundle <- session$build_bundle(
    input_file = job$input_file,
    input_file_name = job$input_file_name,
    intermediate_dir = job$intermediate_dir,
    user_params = job$user_params,
    processing = job$processing
  )
  saveRDS(bundle, job$out_file)
  cat("Session data saved.\n")

} else if (identical(job$mode, "load")) {
  bundle <- tryCatch(readRDS(job$file), error = function(e) NULL)
  problems <- session$validate_bundle(bundle)
  result <- list(problems = problems)
  if (length(problems) == 0) {
    result$input_file <- session$restore_bundle(
      bundle, job$input_dir, job$intermediate_dir, job$tables_dir
    )
    result$input_file_name <- bundle$input_file_name
    result$user_params <- bundle$user_params
    result$processing <- bundle$processing
    result$processed <- !is.null(bundle$to_report)
  }
  saveRDS(result, job$result_file)
  cat("Session data checked", if (length(problems) == 0) "and loaded.\n" else "(invalid).\n")

} else {
  stop("Unknown mode: ", job$mode)
}
