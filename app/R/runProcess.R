# ------------------------------------------------------------------------------------------- #
# A Shiny module to run a processing script allowing some selections
# ------------------------------------------------------------------------------------------- #

#' A Shiny UI module to run a processing script allowing some selections
#'
#' @param uiId A string that identifies the UI module.
#'
#' @returns A list of UI elements
#' @export
runProcessUI <- function(uiId) {
  ns <- shiny::NS(uiId)
  shiny::tagList(
    shiny::tags$style(HTML("
      .shiny-options-group label {
        white-space: nowrap;
      }
    ")),
    # JS handler to enable/disable weight options
    shiny::tags$script(HTML("
      Shiny.addCustomMessageHandler('toggleWeightOptions', function(message) {
        let radios = document.querySelectorAll('input[name=\"' + message.inputId + '\"]');
        if (radios.length >= 4) {
          radios[0].disabled = !message.enabled; // largestR2
          radios[2].disabled = !message.enabled; // 1/x
          radios[3].disabled = !message.enabled; // 1/x^2
        }
      });
    ")),
    settings_group(
    "Quality control", "chart-simple",
    # Outlier removal
    shiny::checkboxInput(
      inputId = ns("rm_outlier"),
      label = label_tip("Remove outlier samples",
                        paste("A sample is removed when more than 20% of the internal standards deviate",
                              "by more than 3 SD from their mean (log scale).")),
      value = TRUE,
      width = "100%"
    ),
    shiny::checkboxInput(
      inputId = ns("blank_filter"),
      label = label_tip("Blank filtering",
                        paste("A feature is kept when its maximum or mean in the samples is above the",
                              "factor times its mean in the blanks.")),
      value = TRUE,
    ),
    # Only shown when blank filtering is selected (as the other options with settings)
    shiny::conditionalPanel(
      condition = "input.blank_filter == true",
      ns = ns,
      shiny::numericInput(
        inputId = ns("blank_filter_factor"),
        label = "Blank filtering factor",
        value = 5,
        min = 0,
      )
    ),
    # QC RSD% filter: remove features with a high RSD% among the QC replicates (after
    # normalization and blank subtraction)
    shiny::checkboxInput(
      inputId = ns("rsd_filter"),
      label = label_tip("Remove features with QC RSD% above a threshold",
                        paste("After normalization and blank subtraction. The RSD% is computed within",
                              "each QC group; with several groups, their median is used.")),
      value = FALSE,
      width = "100%"
    ),
    shiny::conditionalPanel(
      condition = "input.rsd_filter == true",
      ns = ns,
      shiny::radioButtons(
        inputId = ns("rsd_filter_scope"),
        label = "Apply to",
        choices = c(
          "Targeted and non-targeted" = "both",
          "Targeted only" = "targeted",
          "Non-targeted only" = "nontargeted"
        ),
        selected = "both",
        width = "100%"
      ),
      shiny::conditionalPanel(
        condition = "input.rsd_filter_scope != 'nontargeted'",
        ns = ns,
        shiny::numericInput(
          inputId = ns("rsd_max_targeted"),
          label = "Maximum QC RSD% of targeted features (up to 80)",
          value = 30, min = 1, max = 80, step = 1
        )
      ),
      shiny::conditionalPanel(
        condition = "input.rsd_filter_scope != 'targeted'",
        ns = ns,
        shiny::numericInput(
          inputId = ns("rsd_max_nontargeted"),
          label = "Maximum QC RSD% of non-targeted features (up to 80)",
          value = 30, min = 1, max = 80, step = 1
        )
      )
    )
    ),
    settings_group(
    "Calibration", "chart-line",
    # Option for keeping all calibration points (even though outside range)
    shiny::checkboxInput(
      inputId = ns("optimize_cal_points"),
      label = label_tip("Optimize calibration curves to exclude points outside the sample range",
                        paste("Calibration points above the highest sample signal are left out of the",
                              "curve, keeping at least 3 points. Without any sample or QC at or above",
                              "the LLOQ, the full range is kept.")),
      value = FALSE,
      width = "100%"
    ),
    shiny::checkboxInput(
      inputId = ns("set_minimum_number"),
      label = label_tip("Minimum number of calibration points per curve",
                        paste("A curve always needs at least 3 calibration levels (from the LLOQ",
                              "upwards); this is the default. Tick to require more.")),
      value = FALSE,
      width = "100%"
    ),
    shiny::conditionalPanel(
      condition = "input.set_minimum_number == true",
      ns = ns,
      shiny::numericInput(
        inputId = ns("min_cal_points"),
        label = shiny::tags$span(
          "Set minimum number of cal points per curve",
          style = "white-space: nowrap;"
        ),
        value = 3,
        min = 3,
        step = 1
      )
    ),
    shiny::checkboxInput(
      inputId = ns("keep_all_calcurves"),
      label = label_tip("Keep all calibration curves",
                        paste("Compounds whose calibration range is not valid (e.g. no level above the",
                              "LLOQ signal, or too few levels) keep a curve from the LLOQ (or the lowest",
                              "level) to the highest level. A batch still uses a valid global curve when",
                              "there is one. The reports show why each curve was not valid.")),
      value = FALSE,
      width = "100%"
    ),
    shiny::checkboxInput(
      inputId = ns("r2_filter"),
      label = label_tip("Minimum R² of the calibration curves",
                        paste("A compound whose calibration curve has a lower R² is considered failed:",
                              "no concentrations, not exported (kept and marked with 'Keep all",
                              "calibration curves'). The reports and tables show the reason.")),
      value = FALSE,
      width = "100%"
    ),
    shiny::conditionalPanel(
      condition = "input.r2_filter == true",
      ns = ns,
      shiny::numericInput(
        inputId = ns("min_r2"),
        label = "Minimum R² (0-1)",
        value = 0.99, min = 0, max = 1, step = 0.005
      )
    ),
    shiny::checkboxInput(
      inputId = ns("linear_only"),
      label = label_tip("Linear models only",
                        paste("Fit only linear calibration curves (with the chosen weighting), even when",
                              "a quadratic curve would fit better.")),
      value = FALSE,
      width = "100%"
    ),
    shiny::checkboxInput(
      inputId = ns("log_calibration"),
      label = label_tip("Log scale for calibration curve fitting",
                        "Fit on log-transformed concentrations and signals (weighting 1 only)."),
      value = FALSE,
      width = "100%"
    ),
    shiny::radioButtons(
      inputId = ns("weight"),
      label = label_tip("Weighting method",
                        paste("Largest R²: per compound, the weighting (1, 1/x, 1/x²) and model",
                              "(linear or quadratic) with the best fit.")),
      selected = "largestR2",
      inline = TRUE,   # ✅ add this
      choiceValues = list("largestR2", "1", "1_div_x", "1_div_x2"),
      choiceNames = list(
        shiny::HTML(paste0("Largest R", shiny::tags$sup("2"), "(iterative)")),
        "1",
        "1/x",
        shiny::HTML(paste0("1/x", shiny::tags$sup("2")))
      ),
    ),
    shiny::radioButtons(
      inputId = ns("llox_method"),
      label = label_tip("LOD/LLOQ method",
                        paste("LOD: the lowest calibration level whose mean signal is above the",
                              "threshold with factor 3; LLOQ: with factor 10.")),
      selected = "pt_signal_mean_plus_sd",
      choiceValues = list("pt_signal_mean", "pt_signal_mean_plus_sd"),
      choiceNames = list(
        shiny::HTML(paste0("pt. > 3 (or 10) * mean", shiny::tags$sub("Cal0"))),
        shiny::HTML(paste0("pt. > mean", shiny::tags$sub("Cal0"), " + 3 (or 10) * StdDev", shiny::tags$sub("Cal0")))
      ),
    ),
    # RSD% 20% threshold for LLOQ
    shiny::checkboxInput(
      inputId = ns("use_rsd20"),
      label = label_tip("Use RSD% 20% threshold for LLOQ",
                        "The replicates at the LLOQ level must also have an RSD% of at most 20%."),
      value = TRUE,
    )
    ),
    shiny::hr(class = "q-divider"),
    shiny::actionButton(ns("run_button"), label = "Process data", icon = shiny::icon("play"))

  )
}


#' A Shiny UI module to run a processing script allowing some selections
#'
#' @param uiId A string that identifies the UI module.
#' 
#' @returns A list of UI elements
#' @export
runProcessUI_2 <- function(uiId) {
  ns <- shiny::NS(uiId)
  shiny::tagList(
    # Option for keeping all calibration points (even though outside range)

  
    shiny::verbatimTextOutput(ns("script_output"), placeholder = TRUE)
    
  )
}

#' A Shiny server module
#'
#' @param serverId A string that identifies the server module.
#' @export
runProcessServer <- function(serverId, data_info, ws, clear = NULL) {
  
  shiny::moduleServer(serverId, function(input, output, session) {
    # Disable/enable weight options based on log_calibration
    shiny::observe({
      enabled <- !isTRUE(input$log_calibration)
      session$sendCustomMessage("toggleWeightOptions", list(
        inputId = session$ns("weight"), enabled = enabled
      ))

      # Reset selection if user had picked a disabled option
      if (!enabled && input$weight %in% c("largestR2", "1_div_x", "1_div_x2")) {
        updateRadioButtons(session, "weight", selected = "1")
      }
    })
    # The minimum number of calibration points is 3 unless set (the hidden field would keep
    # a value set before)
    shiny::observeEvent(input$set_minimum_number, {
      if (!isTRUE(input$set_minimum_number)) shiny::updateNumericInput(session, "min_cal_points", value = 3)
    })
  })
  runScriptServer(
    serverId,
    script_path = "scripts/process.R",
    what = "Processing data",
    param_ids = c(
      "optimize_cal_points",
      "min_cal_points",
      "keep_all_calcurves",
      "r2_filter",
      "min_r2",
      "linear_only",
      "rm_outlier",
      "log_calibration",
      "weight",
      "llox_method",
      "use_rsd20",
      "blank_filter",
      "blank_filter_factor",
      "rsd_filter",
      "rsd_filter_scope",
      "rsd_max_targeted",
      "rsd_max_nontargeted"
    ),
    data_info = data_info,
    ws = ws,
    clear = clear
  )
}

#' A Shiny app for module testing purposes
runProcessApp <- function() {
  ui <- shiny::fluidPage(
    runProcessUI("proc1"),
  )
  server <- function(input, output, session) {
    runProcessServer("proc1", ws=ws)
  }
  shiny::shinyApp(ui = ui, server = server)
}

# Module test on `code/` where `app.R` is located
# shiny::runApp(runProcessApp())
