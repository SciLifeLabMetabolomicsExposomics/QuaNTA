# ------------------------------------------------------------------------------------------- #
# Render a Quarto report in a child process (started by the app, see R/genReport.R)
#
# The report sources are copied into a private working directory of the session, so that
# Quarto's intermediate files (.knit.md, .tex, figures, ...) of concurrent users never meet in the
# shared `reports/` folder. The working directory is deleted afterwards.
#
# Usage: Rscript render_report.R --job=<path of a job .rds file>
# The job is a list with:
#   - reports_dir: the app's `reports/` folder (read only)
#   - work_dir:    the private working directory (created, then deleted)
#   - qmd:         the report file name, e.g. "report-internal.qmd"
#   - output_format, out_name, execute_params: passed to `quarto::quarto_render()`
#   - out_dir:     the session's report directory, where the output is written
# Environment: QUANTA_PARAMS_FILE (session parameters), R_BOX_PATH and APP_ROOT (app directory)
# ------------------------------------------------------------------------------------------- #

args <- commandArgs(trailingOnly = TRUE)
job_file <- sub("^--job=", "", grep("^--job=", args, value = TRUE))
stopifnot("Usage: render_report.R --job=<job.rds>" = length(job_file) == 1, file.exists(job_file))
job <- readRDS(job_file)

# A function, because `on.exit()` (deleting the working directory, also after an error) does not
# work at the top level of a script
render_in_private_copy <- function(job) {
  # 1. Private copy of the report sources (without Quarto's cache folder)
  unlink(job$work_dir, recursive = TRUE, force = TRUE)
  dir.create(job$work_dir, recursive = TRUE)
  on.exit(unlink(job$work_dir, recursive = TRUE, force = TRUE), add = TRUE)
  srcs <- setdiff(list.files(job$reports_dir, all.files = TRUE, no.. = TRUE), ".quarto")
  ok <- file.copy(file.path(job$reports_dir, srcs), job$work_dir, recursive = TRUE)
  if (!all(ok)) stop("Could not copy the report sources: ", paste(srcs[!ok], collapse = ", "))

  # Run the report code in the private copy, not in `../..` of it (a folder shared by all sessions)
  quarto_yml <- file.path(job$work_dir, "_quarto.yml")
  yml <- readLines(quarto_yml, warn = FALSE)
  i_root <- grep("^\\s*root\\.dir:", yml)
  stopifnot("`root.dir` not found in _quarto.yml" = length(i_root) == 1)
  yml[i_root] <- sub("root\\.dir:.*$", "root.dir: \".\"", yml[i_root])
  writeLines(yml, quarto_yml)

  # 2. Render into the session's report directory
  dir.create(job$out_dir, recursive = TRUE, showWarnings = FALSE)
  rel_out <- as.character(fs::path_rel(job$out_dir, start = job$work_dir))
  # Time of generation under the title (title page of the PDF, title block of the HTML)
  generated_at <- job$execute_params$generated_at
  subtitle <- if (length(generated_at) == 1 && nzchar(generated_at)) {
    c("--metadata", paste0("subtitle:Report generated: ", generated_at))
  }
  quarto::quarto_render(
    file.path(job$work_dir, job$qmd),
    output_format = job$output_format,
    output_file = job$out_name,
    execute_params = job$execute_params,
    quarto_args = c("--output-dir", rel_out, subtitle)
  )
  out <- file.path(job$out_dir, job$out_name)
  if (!file.exists(out)) stop("The report was not created: ", out)
  # One self-contained HTML file (not by Pandoc's `embed-resources`, see the script)
  if (identical(tolower(tools::file_ext(out)), "html")) {
    source(file.path(job$reports_dir, "..", "scripts", "embed_html_resources.R"), local = TRUE)
    embed_html_resources(out)
  }
  # What Quarto copies next to a report is inside the report now
  unlink(file.path(job$out_dir, c(paste0(tools::file_path_sans_ext(job$qmd), "_files"), "logos", "styles.css", "chempages")),
         recursive = TRUE, force = TRUE)
  cat("Report created:", basename(out), "\n")
}

render_in_private_copy(job)
