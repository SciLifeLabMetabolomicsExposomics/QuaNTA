# ------------------------------------------------------------------------------------------- #
# A Shiny module to read MS-Dial output files and display the output
# ------------------------------------------------------------------------------------------- #

# "params.yml": The parameter file in YAML format that stores user's MS-DIAL info
# .yml_file <- "../params.yml"

#' A Shiny UI module to read MS-Dial output files and display the output
#'
#' @param uiId A string that identifies the UI module.
#'
#' @returns A list of UI elements including an action button and a text output area.
#' @export
runReadMsdialUI <- function(uiId) {
  ns <- shiny::NS(uiId)
  settings_group(
    "Processing", "layer-group",
    # `per batch processing` options
    shiny::checkboxInput(
      inputId = ns("per_batch"),
      label = label_tip("Process data per batch",
                        paste("Blank subtraction and calibration per batch (Batch ID). A batch without",
                              "enough calibration replicates uses the global curve of all batches.")),
      value = TRUE,
    ),
    shiny::actionButton(ns("run_button"), "Read data file", icon = shiny::icon("file-import"))
  )
}


#' A Shiny UI module to read MS-Dial output files and display the output
#'
#' @param uiId A string that identifies the UI module.
#' 
#' @returns A list of UI elements including an action button and a text output area.
#' @export
runReadMsdialUI_2 <- function(uiId) {
  ns <- shiny::NS(uiId)
  shiny::tagList(

    shiny::verbatimTextOutput(ns("script_output"), placeholder = TRUE),

  )
}

#' A Shiny server module to read MS-Dial output files and display the output
#'
#' @param serverId A string that identifies the server module.
#'
#' @export
runReadMsdialServer <- function(serverId, data_info, ws, clear = NULL) {
  runScriptServer(serverId,
                  script_path = "scripts/read-msdial.R",
                  what = "Reading the data file",
                  param_ids = c("per_batch"),
                  data_info = data_info,
                  ws = ws,
                  clear = clear)
}

#' A Shiny app to run a script and display the output, for testing purposes
runReadMsdialApp <- function() {
  ui <- shiny::fluidPage(
    runReadMsdialUI("read_msdial"),
  )
  server <- function(input, output, session) {
    runReadMsdialServer("read_msdial")
  }
  shiny::shinyApp(ui = ui, server = server)
}

# Module test on `code/` where `app.R` is located
# shiny::runApp(runReadMsdialApp())
