# ------------------------------------------------------------------------------------------- #
# Screenshots of the app for the tutorial (`images/*.png`), taken from the running app with the
# example data. Run by hand when the look of the app changed, from this folder, and commit the
# images:
#     Rscript _screenshots.R
#
# Needs R with the packages of the app, `shinytest2` and `png` (install.packages(c("shinytest2",
# "png"))) and Chrome or
# Microsoft Edge. The pages use the images as they are; rendering the tutorial does not run this.
# ------------------------------------------------------------------------------------------- #

tut_root <- normalizePath(".", winslash = "/")                     # tutorial/
app_dir <- normalizePath(file.path(tut_root, "..", "app"), winslash = "/")
example <- normalizePath(file.path(tut_root, "..", "example_data"), winslash = "/")
img_dir <- file.path(tut_root, "images")
dir.create(img_dir, showWarnings = FALSE)
if (!file.exists(file.path(app_dir, "app.R"))) stop("Run this script from the tutorial/ folder.")

# shinytest2 only runs outside of CRAN checks
Sys.setenv(NOT_CRAN = "true")
# Chrome, or else Microsoft Edge (also Chromium-based)
if (is.null(tryCatch(suppressMessages(chromote::find_chrome()), error = function(e) NULL))) {
  edge <- c("C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe",
            "C:/Program Files/Microsoft/Edge/Application/msedge.exe",
            "/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge")
  edge <- edge[file.exists(edge)]
  if (length(edge) == 0) stop("Neither Chrome nor Microsoft Edge was found (or set CHROMOTE_CHROME).")
  Sys.setenv(CHROMOTE_CHROME = edge[1])
}

# The app started as usual (`shiny::runApp()`), in a process of its own; shinytest2 drives it by its
# address (given the app folder, it would load it as a package)
port <- httpuv::randomPort()
server <- processx::process$new(
  # test.mode: shinytest2 can read the app's values (e.g. the logs)
  "Rscript", c("-e", sprintf("shiny::runApp('%s', port = %d, launch.browser = FALSE, test.mode = TRUE)", app_dir, port)),
  stdout = "|", stderr = "2>&1", cleanup = TRUE
)
url <- sprintf("http://127.0.0.1:%d", port)
until <- Sys.time() + 120
while (!isTRUE(tryCatch({ readLines(url, n = 1, warn = FALSE); TRUE }, error = function(e) FALSE))) {
  if (!server$is_alive()) stop("The app did not start:\n", paste(server$read_all_output_lines(), collapse = "\n"))
  if (Sys.time() > until) stop("The app did not start within 2 minutes.")
  Sys.sleep(1)
}
# The window of the screenshots (as on a laptop screen; made taller for a shot when its columns
# do not fit)
win_width <- 1600
win_height <- 1000
app <- shinytest2::AppDriver$new(url, name = "tutorial", width = win_width, height = win_height,
                                 load_timeout = 60 * 1000, timeout = 30 * 1000)
# Stopped at the end, or on an error (`on.exit()` does not work at the top level of a script)
stop_app <- function() { try(app$stop(), silent = TRUE); invisible(server$kill_tree()) }
options(error = function() { stop_app(); q(status = 1) })

# The columns of a page scroll within the window: the window made as tall as needed to show them
# whole (a few rounds, as e.g. the logs grow with the window)
fit_columns <- function() {
  for (i in 1:5) {
    over <- app$get_js("
      Math.max(0, ...Array.from(document.querySelectorAll('.card-body'))
        .filter(e => e.offsetParent !== null && ['auto', 'scroll'].includes(getComputedStyle(e).overflowY))
        .map(e => e.scrollHeight - e.clientHeight))")
    if (over <= 1) break
    app$set_window_size(win_width, app$get_js("window.innerHeight") + over + 10)
    app$wait_for_idle()
  }
}
# A screenshot of the window, or of one element of it (`selector`): cut out of the window's
# screenshot (shinytest2 cuts elements in a modal at a wrong place)
shot <- function(name, selector = NULL, fit = TRUE) {
  if (fit) fit_columns()
  f <- file.path(img_dir, paste0(name, ".png"))
  unlink(f)   # Replaced (shinytest2 does not overwrite)
  app$get_screenshot(f, selector = "viewport")
  if (!is.null(selector)) {
    box <- app$get_js(sprintf("(() => { const r = document.querySelector('%s').getBoundingClientRect();
      return [r.left, r.top, r.right, r.bottom].map(v => Math.round(v * window.devicePixelRatio)); })()", selector))
    img <- png::readPNG(f)
    rows <- seq(max(1, box[[2]] + 1), min(dim(img)[1], box[[4]]))
    cols <- seq(max(1, box[[1]] + 1), min(dim(img)[2], box[[3]]))
    png::writePNG(img[rows, cols, , drop = FALSE], f)
  }
  cat("  images/", name, ".png (", app$get_js("window.innerHeight"), " px high)\n", sep = "")
  flush(stdout())
}
# Wait until a log (a `verbatimTextOutput`) says that its script has finished
wait_for_log <- function(output_id, minutes = 15) {
  until <- Sys.time() + minutes * 60
  repeat {
    txt <- tryCatch(app$get_value(output = output_id), error = function(e) "")
    if (grepl("Finished\\.", txt)) return(invisible(txt))
    if (grepl("exit status", txt)) stop("The app reported an error; see its log:\n", txt)
    if (Sys.time() > until) stop("Timed out waiting for ", output_id)
    Sys.sleep(2)
  }
}

cat("Taking the screenshots of page 1 ...\n")
# 1. Page 1 as it opens
app$wait_for_idle()
shot("page1-start")

# 2. The example parameters file and data file uploaded, and the data read
app$upload_file(`data_info-load_params` = file.path(example, "QuaNTA_example_parameters.yml"))
app$upload_file(`data_info-input_file_upload` = file.path(example, "QuaNTA_example_ESIpos.zip"))
app$wait_for_idle()
app$click("read_msdial-run_button")
wait_for_log("read_msdial-script_output")
app$wait_for_idle()
shot("page1-read")

# 3. The popup of "Process data" (notes on the data and the settings), when there is one
app$click("proc-run_button")
app$wait_for_idle()
has_popup <- isTRUE(app$get_js("!!document.getElementById('proc-proceed')"))
if (has_popup) {
  shot("page1-process-popup", selector = ".modal-content", fit = FALSE)
  app$click("proc-proceed")
}

# 4. Processing finished: the log on the right
wait_for_log("proc-script_output", minutes = 30)
app$wait_for_idle()
shot("page1-processed")

stop_app()
cat("Done. Check the images and commit them.\n")
