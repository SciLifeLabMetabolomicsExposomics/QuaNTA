# ------------------------------------------------------------------------------------------- #
# Per-session workspace
#
# Every browser session gets its own directory tree below `tempdir()`, so concurrent users
# never overwrite each other's uploads / intermediate .rds files / exported tables / reports,
# no matter whether the hosting platform runs one container per user or many users per container.
# The directory is deleted when the session ends.
# ------------------------------------------------------------------------------------------- #

#' Create a private workspace for one Shiny session
#'
#' @param session The (root) Shiny session, i.e. the `session` argument of `server()`.
#' @param root Parent directory. Defaults to the per-process temp directory.
#'
#' @returns A list with absolute paths: `base`, `input`, `intermediate`, `tables`, `report`,
#'   and `prefix`, the URL prefix under which `report` is served (used for the report iframe).
#' @export
create_session_workspace <- function(session, root = tempdir()) {
  id <- substr(digest::digest(session$token, algo = "sha256"), 1, 24)
  base <- file.path(root, paste0("quanta_", id))
  dirs <- list(
    base         = base,
    input        = file.path(base, "input"),
    intermediate = file.path(base, "intermediate"),
    tables       = file.path(base, "tables"),
    report       = file.path(base, "report")
  )
  for (d in dirs) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  dirs <- lapply(dirs, normalizePath, winslash = "/", mustWork = TRUE)

  # Resource paths are process-wide, so the prefix must be unique per session
  prefix <- paste0("rep_", id)
  shiny::addResourcePath(prefix, dirs$report)
  dirs$prefix <- prefix

  session$onSessionEnded(function() {
    try(shiny::removeResourcePath(prefix), silent = TRUE)
    unlink(dirs$base, recursive = TRUE, force = TRUE)
  })
  dirs
}
