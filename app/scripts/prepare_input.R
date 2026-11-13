# ------------------------------------------------------------------------------------------- #
# Make an uploaded data file ready for reading, in a child process (started by the app, see
# R/msdialInfo.R): unpack a .gz / .zip, convert an .xlsx sheet to text, and rewrite text that is
# not tab-delimited UTF-8 with decimal points (projlib/input_file.R). Compressed uploads stay below
# upload size limits, e.g. 1000 MB on SciLifeLab Serve; MS-DIAL text exports compress 5-10 times.
#
# Usage: Rscript prepare_input.R --job=<path of a job .rds file>
# The job is a list with:
#   - src:         the uploaded file
#   - name:        its original file name (e.g. "data.txt.gz", "data.zip" or "data.xlsx")
#   - out_dir:     directory for the file to read (the session's input directory)
#   - sheet:       for an .xlsx file, the sheet to use (NULL: the only one)
#   - result_file: where to save the result of `input_file$prepare()`: list(path, name, notes),
#                  or list(sheets, path, name) for an .xlsx file with several sheets
# ------------------------------------------------------------------------------------------- #

args <- commandArgs(trailingOnly = TRUE)
job_file <- sub("^--job=", "", grep("^--job=", args, value = TRUE))
stopifnot("Usage: prepare_input.R --job=<job.rds>" = length(job_file) == 1, file.exists(job_file))
job <- readRDS(job_file)

options(box.path = Sys.getenv("R_BOX_PATH", "app/"))
box::use(input_file = projlib/input_file)

# The largest decompressed file: the disk (/tmp) is shared by all users of the app
res <- input_file$prepare(job$src, job$name, job$out_dir, sheet = job$sheet, max_bytes = 10e9)
saveRDS(res, job$result_file)
if (is.null(res$sheets)) {
  cat("Ready to read:", res$name, sprintf("(%.1f MB)", file.size(res$path) / 1e6), "\n")
  if (length(res$notes) > 0) cat(paste0("  - ", res$notes, "\n"), sep = "")
} else {
  cat("The workbook has", length(res$sheets), "sheets:", paste(res$sheets, collapse = ", "), "\n")
}
