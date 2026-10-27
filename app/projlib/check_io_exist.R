#' Check all files exist
#'
#' @param fns file names. or list of list of ... files
#' @export
chq_all_files_exist <- function(fns) {
  fns <- unlist(fns)
  ex <- file.exists(fns)
  if(! all(ex)) {
    files <- paste(fns[!ex], "\n")
    stop(paste("\nThese files are required but not exist.\n", files))
  }
  invisible(TRUE)
}

#' Make directories if not exist
#'
#' If the directories do not exist, it will be created with the specified mode.
#' Write permission is required to the parent directories.
#'
#' @param dirs directory names, or list of list of ... directories
#' @param mode The mode of the creating directories, when they do not exist
#' @return The directory paths
#' @export
mkdir_if_not_exist <- function(dirs, mode = "0777") {
  dirs <- unique(unlist(dirs))   # Remove duplicates
  stopifnot(is.character(dirs))
  missing <- !dir.exists(dirs)
  if (any(missing)) {
    warning(paste("These directories are not exist. Creating them now.\n", dirs[missing]))
    are_created <- sapply(dirs[missing], \(.x) {
      dir.create(.x, showWarnings = FALSE, recursive = TRUE, mode = mode)
    })
    stopifnot("The directories could not be created" = all(are_created))
  } else {
    for(dir in dirs) {
      if (file.access(dir, 2L) != 0) stop("Write permission is required to", dir)
    }
  }
  invisible(dirs)
}

#' Check input files and output directories
#'
#' @param FILE a list that contains input files in `i` and output in `o`
#' @return a logical vector for input and output existence
#' @export
check_io_exist <- function(FILE) {
  i <- if (!is.null(FILE$i)) {
    chq_all_files_exist(FILE$i)  # brief check if all input files exist
  } else {
    TRUE
  }
  if (!is.null(FILE$o)) {
    mkdir_if_not_exist(dirname(unlist(FILE$o)))
  }
  o <- TRUE
  invisible(c(i, o))
}
