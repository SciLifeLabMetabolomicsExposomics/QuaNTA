# ------------------------------------------------------------------------------------------- #
# The uploaded data file, made ready for reading (`scripts/read-msdial.R` reads tab-delimited
# UTF-8 text):
#   - .gz / .zip: unpacked (one data file inside)
#   - .xlsx: one sheet converted to tab-delimited text (the user chooses the sheet when there are
#     several)
#   - text separated by semicolons or commas, in UTF-16 ("Unicode text" of Excel) or with decimal
#     commas: rewritten as tab-delimited UTF-8 text with decimal points
# Used by the app (`scripts/prepare_input.R`, a background process) and by `run_quanta.R`.
# ------------------------------------------------------------------------------------------- #

#' The data file types that can be uploaded (for messages)
#' @export
accepted_types_text <- paste(
  "the MS-DIAL export as text (.txt or .csv; tab-delimited, or separated by semicolons or commas),",
  "as an Excel workbook (.xlsx), or one of these compressed as .gz or .zip"
)

#' The largest .xlsx file that is converted: a workbook is read into memory as a whole (the memory
#' is shared by all users of the app); larger data is uploaded as text
.max_xlsx_bytes <- 150e6

.safe_name <- function(x) gsub("[^A-Za-z0-9._-]", "_", basename(x))
.is_data_name <- function(name) grepl("\\.(txt|csv|tsv|xlsx)$", tolower(name))

#' Why an uploaded data file has a type that cannot be used
#'
#' @param name The original file name
#' @returns A message, or `NULL` when the type is accepted
#' @export
type_problem <- function(name) {
  nm <- tolower(name)
  if (.is_data_name(nm) || grepl("\\.zip$", nm) || grepl("\\.(txt|csv|tsv)\\.gz$", nm)) return(NULL)
  if (grepl("\\.(xls|xlsm|xlsb|ods)$", nm)) {
    return(paste0("\"", name, "\" is not an .xlsx workbook. Save it in Excel as \"Excel Workbook (.xlsx)\" ",
                  "or as \"Text (Tab delimited) (.txt)\"."))
  }
  if (grepl("\\.gz$", nm)) {
    return(paste0("\"", name, "\" is a .gz file, but not of a text file (the name must end in .txt.gz or .csv.gz)."))
  }
  paste0("\"", name, "\" is not a .txt, .csv, .xlsx, .gz or .zip file.")
}

#' The message when a file is not in the expected format
.format_error <- function(what, detail = NULL) {
  stop(what, " could not be read: its format is probably wrong",
       if (!is.null(detail)) paste0(" (", detail, ")"), ". QuaNTA reads the alignment result exported ",
       "by MS-DIAL: a tab-delimited text file with the sample information in the first rows ",
       "(\"File type\" in the 2nd row) and the column names (\"Alignment ID\", ...) in the 5th row.",
       call. = FALSE)
}

# -----  UNPACKING  ---------------------------------------------------------------------------

#' Unpack a .gz or .zip file with one data file inside into `out_dir`
#'
#' @param max_bytes The largest unpacked file: a small compressed file can unpack to terabytes.
#'   Counted while writing (a zip header can state any size).
#' @returns list(path, name) of the unpacked file
.unpack <- function(src, name, out_dir, max_bytes) {
  ext <- tolower(tools::file_ext(name))
  if (ext == "gz") {
    inner_name <- sub("\\.gz$", "", name, ignore.case = TRUE)
    if (!grepl("\\.(txt|csv|tsv)$", inner_name, ignore.case = TRUE)) {
      stop("The .gz file must be of a text file (named <name>.txt.gz or <name>.csv.gz): ", name, call. = FALSE)
    }
    con_in <- gzfile(src, "rb")
  } else {
    listing <- utils::unzip(src, list = TRUE)
    files <- listing$Name[!grepl("/$", listing$Name) & !grepl("(^|/)__MACOSX/|(^|/)\\.", listing$Name)]
    if (length(files) != 1) {
      stop("The zip file must contain exactly one data file; it contains ", length(files), ": ",
           paste(utils::head(files, 5), collapse = ", "), call. = FALSE)
    }
    inner_name <- basename(files)
    if (!.is_data_name(inner_name)) {
      stop("The file in the zip file must be a .txt, .csv or .xlsx file; it is: ", inner_name, call. = FALSE)
    }
    con_in <- unz(src, files, "rb")
  }
  out <- file.path(out_dir, .safe_name(inner_name))
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
  if (file.size(out) == 0) stop("The decompressed file is empty.", call. = FALSE)
  list(path = out, name = inner_name)
}

# -----  EXCEL  -------------------------------------------------------------------------------

#' The sheet names of an .xlsx file (only its workbook part is unpacked)
#'
#' @param path Path of the .xlsx file
#' @returns A character vector, in the order of the workbook
#' @export
xlsx_sheets <- function(path) {
  tmp <- tempfile("xlsx_")
  on.exit(unlink(tmp, recursive = TRUE))
  f <- tryCatch(suppressWarnings(utils::unzip(path, files = "xl/workbook.xml", exdir = tmp)),
                error = function(e) character(0))
  if (length(f) == 0 || !file.exists(f)) {
    stop("The .xlsx file could not be opened; is it an Excel workbook (.xlsx)?", call. = FALSE)
  }
  x <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = " ")
  tags <- regmatches(x, gregexpr("<([A-Za-z0-9]+:)?sheet\\s[^>]*>", x))[[1]]
  nm <- sub(".*\\sname=\"([^\"]*)\".*", "\\1", tags)
  for (e in list(c("&lt;", "<"), c("&gt;", ">"), c("&quot;", "\""), c("&apos;", "'"), c("&amp;", "&"))) {
    nm <- gsub(e[1], e[2], nm, fixed = TRUE)
  }
  if (length(nm) == 0) stop("The .xlsx file has no sheets.", call. = FALSE)
  nm
}

#' Write one sheet of an .xlsx file as tab-delimited text
#'
#' Cells are written as they are stored (numbers with up to 15 significant digits), empty cells
#' as empty fields.
#' @param path Path of the .xlsx file
#' @param sheet The sheet name
#' @param out Path of the text file
.xlsx_to_txt <- function(path, sheet, out) {
  x <- openxlsx::read.xlsx(path, sheet = sheet, colNames = FALSE, skipEmptyRows = FALSE,
                           skipEmptyCols = FALSE, detectDates = FALSE, na.strings = character(0))
  if (is.null(x) || nrow(x) < 6) {
    .format_error(paste0("The sheet \"", sheet, "\""), "it has fewer than 6 rows")
  }
  data.table::fwrite(x, out, sep = "\t", col.names = FALSE, quote = "auto", na = "")
  invisible(out)
}

# -----  TEXT FORMAT  -------------------------------------------------------------------------

.num_comma <- "^[-+]?[0-9]+,[0-9]+([eE][-+]?[0-9]+)?$"
.num_point <- "^[-+]?[0-9]*\\.[0-9]+([eE][-+]?[0-9]+)?$"

#' Split lines of text into fields (with quotes as in CSV files); `NULL` if that fails
.split_lines <- function(lines, sep, n_col = NULL) {
  dt <- tryCatch(
    suppressWarnings(data.table::fread(
      text = c(lines, ""), sep = sep, header = FALSE, skip = 0, colClasses = "character",
      fill = if (is.null(n_col)) TRUE else n_col, quote = "\"", na.strings = NULL, strip.white = FALSE,
      blank.lines.skip = FALSE, showProgress = FALSE
    )),
    error = function(e) NULL
  )
  if (is.null(dt)) return(NULL)
  # The added empty line ends the text (one line would otherwise be read as a file name)
  if (nrow(dt) > length(lines)) dt <- dt[seq_along(lines)]
  dt
}

#' The format of a text data file, from its first lines
#'
#' @param path Path of the text file
#' @param what How the file is named in error messages
#' @returns A list: `sep` (the field separator), `encoding` (for [file()]), `dec_comma` (numbers with
#'   decimal commas), `needs_rewrite` and `description` (what a rewrite changes). Stops with an
#'   explanation when the file does not look like an MS-DIAL export.
#' @export
detect_text_format <- function(path, what = "The file") {
  b <- readBin(path, "raw", 3)
  utf16 <- length(b) >= 2 && (identical(b[1:2], as.raw(c(0xff, 0xfe))) || identical(b[1:2], as.raw(c(0xfe, 0xff))))
  encoding <- if (utf16) {
    if (b[1] == as.raw(0xff)) "UTF-16LE" else "UTF-16BE"
  } else if (length(b) == 3 && identical(b, as.raw(c(0xef, 0xbb, 0xbf)))) {
    "UTF-8-BOM"
  } else {
    "UTF-8"
  }
  con <- file(path, open = "r", encoding = encoding)
  lines <- tryCatch(readLines(con, n = 206L, warn = FALSE), error = function(e) character(0))
  close(con)
  if (utf16) lines <- sub("^﻿", "", lines)
  # Not UTF-8 (e.g. text saved by Excel on Windows): Latin-1 (read the same as UTF-8 otherwise)
  latin1 <- !utf16 && !all(validUTF8(lines))
  if (latin1) {
    encoding <- "latin1"
    lines <- iconv(lines, "latin1", "UTF-8")
  }
  if (length(lines) < 6) .format_error(what, "it has fewer than 6 lines")

  # The separator with "File type" in a field of the 2nd line and several columns in the 5th
  sep <- NULL
  for (s in c("\t", ";", ",")) {
    head5 <- .split_lines(lines[1:5], s)
    if (is.null(head5) || nrow(head5) < 5 || ncol(head5) < 3) next
    if (any(trimws(unlist(head5[2])) == "File type")) {
      sep <- s
      break
    }
  }
  if (is.null(sep)) {
    has_ft <- any(grepl("File type", lines[1:5], fixed = TRUE))
    .format_error(what, if (has_ft) {
      "\"File type\" is not in the 2nd row, or the columns are not separated by tabs, semicolons or commas"
    } else {
      "it has no \"File type\" row"
    })
  }
  # Decimal commas: numbers with a comma and none with a point in the data rows
  data <- .split_lines(lines[-(1:5)], sep)
  cells <- if (is.null(data)) character(0) else unlist(data, use.names = FALSE)
  dec_comma <- any(grepl(.num_comma, cells)) && !any(grepl(.num_point, cells))

  changes <- c(
    if (sep != "\t") paste0("separated by ", c(";" = "semicolons", "," = "commas")[[sep]], " instead of tabs"),
    if (utf16) "saved as Unicode (UTF-16) text",
    if (dec_comma) "decimal commas instead of points"
  )
  list(
    sep = sep, encoding = encoding, dec_comma = dec_comma,
    needs_rewrite = length(changes) > 0,
    description = if (length(changes) > 0) paste0("The file was converted to tab-delimited text (it was ",
                                                  paste(changes, collapse = ", "), ").")
  )
}

#' Rewrite a text data file as tab-delimited UTF-8 text with decimal points
#'
#' In chunks of lines, so that large files do not need to fit in memory.
#' @param path The text file
#' @param out The new file (not `path`)
#' @param fmt Its format from [detect_text_format()]
.rewrite_as_tab <- function(path, out, fmt) {
  part <- paste0(out, ".part")
  unlink(part)
  con <- file(path, open = "r", encoding = fmt$encoding)
  on.exit(close(con))
  n_col <- NULL
  first <- TRUE
  repeat {
    lines <- readLines(con, n = 10000L, warn = FALSE)
    if (length(lines) == 0) break
    if (first) lines[1] <- sub("^﻿", "", lines[1])
    dt <- .split_lines(lines, fmt$sep, n_col)
    if (is.null(dt)) stop("The file could not be converted to tab-delimited text.", call. = FALSE)
    if (first) n_col <- ncol(dt)
    if (ncol(dt) < n_col) {   # Short lines: the same number of columns as the first lines
      for (j in (ncol(dt) + 1L):n_col) data.table::set(dt, j = paste0("V", j), value = "")
    }
    if (fmt$dec_comma) {
      rows <- if (first) seq_len(nrow(dt)) > 5L else rep(TRUE, nrow(dt))   # Not the 5 header lines
      for (j in seq_len(ncol(dt))) {
        x <- dt[[j]]
        hit <- rows & grepl(.num_comma, x)
        if (any(hit)) {
          x[hit] <- sub(",", ".", x[hit], fixed = TRUE)
          data.table::set(dt, j = j, value = x)
        }
      }
    }
    data.table::fwrite(dt, part, sep = "\t", col.names = FALSE, quote = "auto", na = "", append = !first)
    first <- FALSE
  }
  close(con)
  on.exit()
  file.rename(part, out)
  invisible(out)
}

# -----  ALL STEPS  ---------------------------------------------------------------------------

#' Make an uploaded data file ready for reading
#'
#' @param src Path of the uploaded file
#' @param name Its original file name
#' @param out_dir Directory for the file to read
#' @param sheet For an .xlsx file: the sheet to use. `NULL`: the only sheet; with several sheets,
#'   nothing is converted and the sheet names are returned (the user chooses one).
#' @param max_bytes The largest unpacked file of a .gz / .zip
#' @returns A list: `path` and `name` of the file to read, and `notes` (what was done); or, for an
#'   .xlsx file with several sheets and no `sheet`, `sheets` and the `path` and `name` of the
#'   (unpacked) .xlsx file.
#' @export
prepare <- function(src, name, out_dir, sheet = NULL, max_bytes = 10e9) {
  problem <- type_problem(name)
  if (!is.null(problem)) stop(problem, call. = FALSE)
  notes <- character(0)
  path <- src
  if (tolower(tools::file_ext(name)) %in% c("gz", "zip")) {
    u <- .unpack(src, name, out_dir, max_bytes)
    notes <- c(notes, paste0("Decompressed ", name, " to ", u$name, "."))
    path <- u$path
    name <- u$name
  }
  temp_files <- if (!identical(path, src)) path
  on.exit(unlink(setdiff(temp_files, path)), add = TRUE)
  txt_name <- paste0(tools::file_path_sans_ext(name), ".txt")
  what <- "The file"

  if (tolower(tools::file_ext(name)) == "xlsx") {
    sheets <- xlsx_sheets(path)
    if (is.null(sheet)) {
      if (length(sheets) > 1) return(list(sheets = sheets, path = path, name = name, notes = notes))
      sheet <- sheets[1]
    }
    if (!sheet %in% sheets) stop("The workbook has no sheet \"", sheet, "\".", call. = FALSE)
    if (file.size(path) > .max_xlsx_bytes) {
      stop(sprintf(paste("The .xlsx file is larger than %.0f MB, the most this app converts. Save the sheet",
                         "in Excel as \"Text (Tab delimited) (.txt)\" and upload that (compressed as .zip",
                         "if it is large)."), .max_xlsx_bytes / 1e6), call. = FALSE)
    }
    out <- file.path(out_dir, .safe_name(txt_name))
    .xlsx_to_txt(path, sheet, out)
    notes <- c(notes, paste0("Converted the sheet \"", sheet, "\" of ", name, " to tab-delimited text."))
    temp_files <- c(temp_files, path)
    path <- out
    name <- txt_name
    what <- paste0("The sheet \"", sheet, "\"")
  }

  fmt <- detect_text_format(path, what)
  if (fmt$needs_rewrite) {
    out <- file.path(out_dir, .safe_name(txt_name))
    if (identical(normalizePath(out, mustWork = FALSE), normalizePath(path))) {
      out <- file.path(out_dir, .safe_name(paste0(tools::file_path_sans_ext(txt_name), "_tab.txt")))
    }
    .rewrite_as_tab(path, out, fmt)
    notes <- c(notes, fmt$description)
    if (!identical(path, src)) temp_files <- c(temp_files, path)
    path <- out
    name <- txt_name
  } else if (identical(path, src)) {   # A file that is used as it is: copied into `out_dir`
    out <- file.path(out_dir, .safe_name(name))
    file.copy(src, out, overwrite = TRUE)
    path <- out
  }
  list(path = normalizePath(path, winslash = "/"), name = name, notes = notes)
}
