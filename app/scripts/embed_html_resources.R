# ------------------------------------------------------------------------------------------- #
# Make a rendered HTML report self-contained, without Pandoc's `embed-resources` (which takes
# several GB for a large report): the local files of its <script>, <link> and <img> tags, and the
# files that the style sheets refer to, are written into the page as data URIs. Only these tags
# are touched, and the page is written piece by piece, so little memory is needed.
# ------------------------------------------------------------------------------------------- #

.mime_of <- function(path) {
  types <- c(js = "application/javascript", mjs = "application/javascript", css = "text/css",
             png = "image/png", jpg = "image/jpeg", jpeg = "image/jpeg", gif = "image/gif",
             svg = "image/svg+xml", ico = "image/x-icon", woff = "font/woff", woff2 = "font/woff2",
             ttf = "font/ttf", otf = "font/otf", eot = "application/vnd.ms-fontobject")
  unname(types[tolower(tools::file_ext(path))] %|na|% "application/octet-stream")
}
`%|na|%` <- function(x, y) if (is.na(x)) y else x

.local_path <- function(url, base) {
  if (grepl("^([a-zA-Z][a-zA-Z0-9+.-]*:|//|#)", url)) return(NA_character_)   # http:, data:, ...
  p <- file.path(base, utils::URLdecode(sub("[?#].*$", "", url)))
  if (file.exists(p) && !dir.exists(p)) p else NA_character_
}

.data_uri <- function(bytes, path) {
  paste0("data:", .mime_of(path), ";base64,", gsub("[\r\n]", "", jsonlite::base64_enc(bytes)))
}

#' The bytes of a file; of a style sheet with the files it refers to embedded
.file_bytes <- function(path) {
  bytes <- readBin(path, "raw", file.size(path))
  ext <- tolower(tools::file_ext(path))
  if (!ext %in% c("css", "js", "mjs")) return(bytes)
  pattern <- if (ext == "css") {
    "url\\(\\s*['\"]?([^'\")]+?)['\"]?\\s*\\)"
  } else {     # The modules a script imports: `from "./x.js"`, `import "./x.js"`, `import("./x.js")`
    "(?:\\bfrom|\\bimport)\\s*\\(?\\s*[\"'](\\.{1,2}/[^\"']+)[\"']"
  }
  css <- rawToChar(bytes)
  m <- gregexpr(pattern, css, perl = TRUE, useBytes = TRUE)[[1]]
  if (m[1] < 0) return(bytes)
  st <- attr(m, "capture.start")[, 1]
  ln <- attr(m, "capture.length")[, 1]
  urls <- substring(css, st, st + ln - 1L)
  out <- raw()
  from <- 1L
  parts <- list()
  for (i in seq_along(urls)) {
    p <- .local_path(urls[i], dirname(path))
    if (is.na(p)) next
    parts[[length(parts) + 1L]] <- bytes[seq_len(st[i] - from) + from - 1L]
    parts[[length(parts) + 1L]] <- charToRaw(.data_uri(.file_bytes(p), p))
    from <- st[i] + ln[i]
  }
  parts[[length(parts) + 1L]] <- bytes[seq.int(from, length.out = length(bytes) - from + 1L)]
  unlist(parts)
}

#' Write the local resources of an HTML file into it
#'
#' @param html_file Path to the HTML file, overwritten
#' @returns The paths of the embedded files, invisibly
embed_html_resources <- function(html_file) {
  base <- dirname(html_file)
  page <- readBin(html_file, "raw", file.size(html_file))
  txt <- rawToChar(page)
  tags <- gregexpr("<(?:script|link|img)\\b[^>]*>", txt, perl = TRUE, useBytes = TRUE)[[1]]
  rm(txt)
  tmp <- tempfile(tmpdir = base, fileext = ".tmp")
  con <- file(tmp, "wb")
  on.exit(close(con), add = TRUE)
  from <- 1L
  used <- character()
  if (tags[1] > 0) for (i in seq_along(tags)) {
    st <- tags[i]
    ln <- attr(tags, "match.length")[i]
    tag <- rawToChar(page[st:(st + ln - 1L)])
    m <- regexec("\\b(src|href)=(\"|')([^\"']+)\\2", tag, perl = TRUE, useBytes = TRUE)
    hit <- regmatches(tag, m)[[1]]
    if (length(hit) == 0) next
    p <- .local_path(hit[4], base)
    if (is.na(p)) next
    new_tag <- paste0(substr(tag, 1L, m[[1]][4] - 1L), .data_uri(.file_bytes(p), p),
                      substr(tag, m[[1]][4] + nchar(hit[4], "bytes"), nchar(tag, "bytes")))
    writeBin(page[seq.int(from, length.out = st - from)], con)
    writeBin(charToRaw(new_tag), con)
    from <- st + ln
    used <- c(used, p)
  }
  writeBin(page[seq.int(from, length.out = length(page) - from + 1L)], con)
  close(con)
  on.exit()
  file.rename(tmp, html_file)
  invisible(used)
}
