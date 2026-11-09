# ------------------------------------------------------------------------------------------- #
# Decompress an uploaded MS-DIAL file (.gz or .zip) in a child process (started by the app, see
# R/msdialInfo.R). Compressed uploads stay below upload size limits, e.g. 1000 MB on SciLifeLab
# Serve; MS-DIAL text exports typically compress 5-10 times.
#
# Usage: Rscript decompress_input.R --job=<path of a job .rds file>
# The job is a list with:
#   - src:         the uploaded file
#   - name:        its original file name (e.g. "data.txt.gz" or "data.zip")
#   - out_dir:     directory for the decompressed file (the session's input directory)
#   - result_file: where to save list(path, name) of the decompressed file
# ------------------------------------------------------------------------------------------- #

args <- commandArgs(trailingOnly = TRUE)
job_file <- sub("^--job=", "", grep("^--job=", args, value = TRUE))
stopifnot("Usage: decompress_input.R --job=<job.rds>" = length(job_file) == 1, file.exists(job_file))
job <- readRDS(job_file)

safe_name <- function(x) gsub("[^A-Za-z0-9._-]", "_", basename(x))
ext <- tolower(tools::file_ext(job$name))
# The largest decompressed file: the disk (/tmp) is shared by all users of the app, and a small
# compressed file can unpack to terabytes. Counted while writing (a zip header can state any size)
max_bytes <- 10e9

if (ext == "gz") {
  inner_name <- sub("\\.gz$", "", job$name, ignore.case = TRUE)
  if (!grepl("\\.txt$", inner_name, ignore.case = TRUE)) {
    stop("The .gz file must be of a .txt file (named <name>.txt.gz): ", job$name, call. = FALSE)
  }
  con_in <- gzfile(job$src, "rb")

} else if (ext == "zip") {
  listing <- utils::unzip(job$src, list = TRUE)
  files <- listing$Name[!grepl("/$", listing$Name) & !grepl("(^|/)__MACOSX/|(^|/)\\.", listing$Name)]
  if (length(files) != 1) {
    stop("The zip file must contain exactly one data file; it contains ", length(files), ": ",
         paste(utils::head(files, 5), collapse = ", "), call. = FALSE)
  }
  inner_name <- basename(files)
  if (!grepl("\\.txt$", inner_name, ignore.case = TRUE)) {
    stop("The file in the zip file must be a .txt file; it is: ", inner_name, call. = FALSE)
  }
  con_in <- unz(job$src, files, "rb")

} else {
  stop("Not a compressed file: ", job$name)
}

out <- file.path(job$out_dir, safe_name(inner_name))
con_out <- file(out, "wb")
written <- 0
repeat {   # In chunks, so that large files do not need to fit in memory
  chunk <- readBin(con_in, "raw", n = 1e7)
  if (length(chunk) == 0) break
  written <- written + length(chunk)
  if (written > max_bytes) {
    close(con_in); close(con_out); unlink(out)
    stop(sprintf("The decompressed file is larger than %.0f GB, the most this app accepts.", max_bytes / 1e9),
         call. = FALSE)
  }
  writeBin(chunk, con_out)
}
close(con_in)
close(con_out)

if (!file.exists(out) || file.size(out) == 0) stop("The decompressed file is empty.")
saveRDS(list(path = normalizePath(out, winslash = "/"), name = inner_name), job$result_file)
cat("Decompressed:", inner_name, sprintf("(%.1f MB)", file.size(out) / 1e6), "\n")
