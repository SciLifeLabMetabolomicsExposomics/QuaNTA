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
    # JS handler to enable/disable one radio button choice (see the server)
    shiny::tags$script(HTML("
      // Enable / disable one choice of a radio button group
      Shiny.addCustomMessageHandler('toggleRadioOption', function(message) {
        document.querySelectorAll('input[name=\"' + message.inputId + '\"]').forEach(function(r) {
          if (r.value === message.value) r.disabled = !message.enabled;
        });
      });
    ")),
    settings_group(
    "Quality control", "chart-simple",
    # Every setting in a grey box (`option_group()`), with its suboptions and hint if any
    # Pre-normalization by the volumetric internal standard(s), before all other steps
    option_group(
    shiny::checkboxInput(
      inputId = ns("vis_prenorm"),
      label = label_tip("Pre-normalize by the volumetric IS",
                        paste("Before all other steps, every peak area (also of the IS) is divided by the",
                              "volumetric IS area of its sample and multiplied by the average vIS area of",
                              "all injections. With several vIS, one after the other; the vIS themselves",
                              "are not changed. Greyed out when the data has no volumetric IS.")),
      value = FALSE,
      width = "100%"
    )
    ),
    # Outlier removal
    option_group(
    shiny::checkboxInput(
      inputId = ns("rm_outlier"),
      label = label_tip("Remove outlier samples",
                        paste("Before normalization (after the removal of failed internal standards).",
                              "A sample is removed when more than 20% of the internal standards deviate",
                              "by more than 3 SD from their mean (log scale) or are missing (zero) in it.")),
      value = TRUE,
      width = "100%"
    ),
    # Which samples can be removed as outliers (shown when the option is selected)
    shiny::conditionalPanel(
      condition = "input.rm_outlier == true",
      ns = ns,
      shiny::radioButtons(
        inputId = ns("rm_outlier_scope"),
        label = label_tip("Outliers among",
                          paste("All injections: any sample can be removed as an outlier. Non-calibration",
                                "samples only: the calibration samples are always kept (all calibration",
                                "points stay available for the curves); they still count for the mean and",
                                "SD of the internal standards.")),
        choices = c("All injections" = "all", "Non-calibration samples only" = "no_calib"),
        selected = "all",
        width = "100%"
      )
    )
    ),
    option_group(
    shiny::checkboxInput(
      inputId = ns("blank_filter"),
      label = label_tip("Blank filter",
                        paste("After normalization, before blank subtraction.",
                              "A feature is kept when its maximum or mean (see below) in the study samples",
                              "is above the factor times its mean in the blanks. QCs and calibration",
                              "standards do not count. Internal standards (also the vIS) are not filtered.")),
      value = TRUE,
    ),
    # Only shown when blank filtering is selected (as the other options with settings)
    shiny::conditionalPanel(
      condition = "input.blank_filter == true",
      ns = ns,
      shiny::numericInput(
        inputId = ns("blank_filter_factor"),
        label = "Blank filter factor",
        value = 5,
        min = 0,
      ),
      shiny::radioButtons(
        inputId = ns("blank_filter_stat"),
        label = label_tip("Compare the blanks with",
                          paste("Maximum: kept when at least one study sample is above the threshold (lenient;",
                                "keeps features found in a few samples only). Mean: kept when the mean of",
                                "the study samples is above it (strict).")),
        choices = c("Maximum of the study samples" = "max", "Mean of the study samples" = "mean"),
        selected = "max",
        width = "100%"
      )
    )
    ),
    # QC RSD% filter: remove features with a high RSD% among the QC replicates (after
    # normalization and blank subtraction)
    option_group(
    shiny::checkboxInput(
      inputId = ns("rsd_filter"),
      label = label_tip("Remove features based on QC RSD%",
                        paste("After normalization and blank subtraction. The RSD% is computed within",
                              "each QC group (all batches together); with several groups, their median is used.",
                              "Without targeted (Quant) features, only non-targeted features can be filtered.")),
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
    )
    ),
    settings_group(
    "Calibration", "chart-line",
    # Grouped by theme, in the order of the processing: the levels of a curve, its limits (from
    # the zero calibrant), the fit from the LLOQ upwards, its linear range, and which curves are valid
    settings_subhead("Calibration levels"),
    option_group(
    shiny::checkboxInput(
      inputId = ns("average_cal_reps"),
      label = label_tip("Average calibration replicates per batch",
                        paste("The replicates of each calibration level are averaged within each batch,",
                              "giving one point per level and batch. If \"Process data per batch\" is ticked",
                              "they will be used separately, per batch. If unticked they will be pooled so that",
                              "each batch is represented in the global calibration curve by one point per concentration.")),
      value = FALSE,
      width = "100%"
    )
    ),
    # Option for keeping all calibration points (even though outside range)
    option_group(
    shiny::checkboxInput(
      inputId = ns("optimize_cal_points"),
      label = label_tip("Optimize calibration curves based on sample range",
                        paste("Calibration points above the highest sample signal are left out of the",
                              "curve, keeping at least 3 points (or the minimum number of calibration points,",
                              "if set higher). Without any sample or QC at or above",
                              "the LLOQ, the full range is kept. Greyed out when the data has no targeted (Quant) features.")),
      value = FALSE,
      width = "100%"
    )
    ),
    option_group(
    shiny::checkboxInput(
      inputId = ns("set_minimum_number"),
      label = label_tip("Minimum calibration points per curve",
                        paste("A curve always needs at least 3 calibration levels (from the LLOQ",
                              "upwards); this is the default. Tick to require more. Greyed out when the data has no targeted (Quant) features.")),
      value = FALSE,
      width = "100%"
    ),
    shiny::conditionalPanel(
      condition = "input.set_minimum_number == true",
      ns = ns,
      shiny::numericInput(
        inputId = ns("min_cal_points"),
        label = shiny::tags$span(
          "Minimum n cal points per curve",
          style = "white-space: nowrap;"
        ),
        value = 3,
        min = 3,
        step = 1
      )
    )
    ),
    settings_subhead("LOD and LLOQ"),
    option_group(
    shiny::radioButtons(
      inputId = ns("llox_method"),
      label = label_tip("LOD/LLOQ method",
                        paste("LOD: the lowest calibration level whose mean signal is above the",
                              "threshold with factor 3; LLOQ: with factor 10. The 'mean + SD' method needs at least",
                              "2 Cal0 replicates (in a batch, or in all batches together for the global curve):",
                              "it is greyed out when the data has fewer, and with averaged calibration replicates.",
                              "A batch with fewer uses the global curve. Greyed out when the data has no targeted (Quant) features.")),
      selected = "pt_signal_mean_plus_sd",
      choiceValues = list("pt_signal_mean", "pt_signal_mean_plus_sd"),
      choiceNames = list(
        shiny::HTML(paste0("pt. > 3 (or 10) * mean", shiny::tags$sub("Cal0"))),
        shiny::HTML(paste0("pt. > mean", shiny::tags$sub("Cal0"), " + 3 (or 10) * StdDev", shiny::tags$sub("Cal0")))
      ),
    ),
    shiny::uiOutput(ns("hint_llox_method"))   # Why the option above is greyed out (see the server)
    ),
    # RSD% 20% threshold for LLOQ
    option_group(
    shiny::checkboxInput(
      inputId = ns("use_rsd20"),
      label = label_tip("Use RSD 20% precision threshold for LLOQ",
                        paste("The replicates at the LLOQ level must also have an RSD% of at most 20%. This needs",
                              "at least 2 replicates of every calibration level (in a batch, or in all batches",
                              "together for the global curve): greyed out when the data has fewer, and with averaged",
                              "calibration replicates. A batch with fewer uses the global curve. Greyed out when the data has no targeted (Quant) features.")),
      value = TRUE,
      width = "100%"
    ),
    shiny::uiOutput(ns("hint_use_rsd20"))   # Why the option above is greyed out (see the server)
    ),
    # Replace the concentrations below the LOD/LLOQ, or report them as "n.d." / "n.q."
    option_group(
    shiny::checkboxInput(
      inputId = ns("impute_llox"),
      label = label_tip("Replace values below the LOD/LLOQ",
                        paste("Ticked: values with a signal below the LOD signal are replaced by LLOQ/4 and those",
                              "below the mean signal of the LLOQ level by LLOQ/2. Unticked: they are reported as 'n.d.' (not",
                              "detected, below the LOD) and 'n.q.' (not quantified, below the LLOQ),",
                              "without a value, and left out of plots and statistics. Greyed out when the data has no targeted (Quant) features.")),
      value = TRUE,
      width = "100%"
    )
    ),
    settings_subhead("Curve fit"),
    option_group(
    shiny::checkboxInput(
      inputId = ns("log_calibration"),
      label = label_tip("Log scale for calibration curve fitting",
                        paste("Fit each curve as a straight line in log-log scale: ln(signal) on ln(concentration),",
                              "weighting 1, no quadratic curves. Points with a signal of 0 are left out of the fit",
                              "(a log of 0 does not exist); the ULOQ is found by SCALiR. Greyed out when the data has",
                              "no targeted (Quant) features.")),
      value = FALSE,
      width = "100%"
    )
    ),
    option_group(
    shiny::radioButtons(
      inputId = ns("weight"),
      label = label_tip("Weighting method",
                        paste("Largest R²: per compound, the weighting (1, 1/x, 1/x²) and model",
                              "(linear or quadratic) with the best fit. When choosing the model, the R²",
                              "of quadratic models is penalized by 0.01, so a quadratic curve is chosen",
                              "only when it fits clearly better (the R² shown is not penalized). With log-scale fitting,",
                              "only the weighting 1 is possible. Greyed out when the data has no targeted (Quant) features.")),
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
    shiny::uiOutput(ns("hint_weight"))   # Why the option above is greyed out (see the server)
    ),
    option_group(
    shiny::checkboxInput(
      inputId = ns("linear_only"),
      label = label_tip("Linear models only",
                        paste("Fit only linear calibration curves (with the chosen weighting), even when",
                              "a quadratic curve would fit better. Not used with log-scale fitting (always linear",
                              "then). Greyed out when the data has no targeted (Quant) features.")),
      value = FALSE,
      width = "100%"
    ),
    shiny::uiOutput(ns("hint_linear_only"))   # Why the option above is greyed out (see the server)
    ),
    settings_subhead("Linear range (ULOQ)"),
    option_group(
    shiny::numericInput(
      inputId = ns("linear_range_tol"),
      label = label_tip("ULOQ: max error% from nominal conc.",
                        paste("ULOQ calculated for all linear curves: ",
                              "mean signal of each calibration level is back-calculated;",
                              "when top level deviates from its nominal concentration by more than assigned %, it is",
                              "left out and the curve refitted, down to 3 levels. With",
                              "log-scale fitting the linear range is found by SCALiR instead.",
                              "Quadratic curves have no ULOQ. Greyed out when the data has no targeted (Quant) features.")),
      value = 20, min = 5, max = 50, step = 5
    ),
    shiny::uiOutput(ns("hint_linear_range_tol"))   # Why the option above is greyed out (see the server)
    ),
    option_group(
    shiny::checkboxInput(
      inputId = ns("enforce_linear_range"),
      label = label_tip("Enforce linear range (cut the curves at the ULOQ)",
                        paste("Ticked: each curve is cut at its ULOQ and refitted; higher concentrations are",
                              "extrapolated. A curve keeps at least the minimum number of levels, so it can end",
                              "above its ULOQ: the levels between are marked as above the linear range. Unticked: the curves keep their range; concentrations between the",
                              "ULOQ and the highest level are marked as above the linear range. The levels above",
                              "the ULOQ are named in the notes either way. Greyed out when the data has no",
                              "targeted (Quant) features.")),
      value = FALSE,
      width = "100%"
    )
    ),
    settings_subhead("Curve validity"),
    option_group(
    shiny::checkboxInput(
      inputId = ns("r2_filter"),
      label = label_tip("Minimum R² of the calibration curves",
                        paste("A compound whose calibration curve has a lower R² is considered failed:",
                              "no concentrations, not exported (kept and marked with 'Keep all",
                              "calibration curves'). The reports and tables show the reason. Greyed out when the data has no targeted (Quant) features.")),
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
    )
    ),
    option_group(
    shiny::checkboxInput(
      inputId = ns("keep_all_calcurves"),
      label = label_tip("Keep all calibration curves",
                        paste("Compounds whose calibration range is not valid (e.g. no level above the",
                              "LLOQ signal, or too few levels) keep a curve from the LLOQ (or the lowest",
                              "level) to the highest level. A batch still uses a valid global curve when",
                              "there is one. The reports show why each curve was not valid. Compounds with a",
                              "curve are also exported when no study sample is above the LLOQ (e.g. data with",
                              "QCs only); otherwise they are not. Greyed out when the data has no targeted (Quant) features.")),
      value = FALSE,
      width = "100%"
    )
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
runProcessServer <- function(serverId, data_info, ws, clear = NULL, data_caps = shiny::reactive(NULL),
                             param_notes = NULL) {
  start <- shiny::reactiveVal(NULL)   # Starts the processing (see "Process data" below)

  shiny::moduleServer(serverId, function(input, output, session) {
    # Options that are not feasible are greyed out (and switched off), from the settings and from
    # what the read data allows (`data_caps()`, `NULL` before a read: nothing greyed out then):
    # - no targeted (Quant) features: all calibration options; the QC RSD% filter of targeted features
    # - log-scale fitting: only the weighting 1, no "linear models only" (always linear), no linear
    #   range tolerance (SCALiR)
    # - averaged calibration replicates: no RSD% 20% threshold, no LOD/LLOQ method "mean + SD"
    # - fewer than 2 Cal0 replicates (in every batch and all together): no "mean + SD"
    # - fewer than 2 replicates per calibration level (same): no RSD% 20% threshold
    # - no volumetric IS: no pre-normalization by it
    radio_option <- function(id, value, enabled) {
      session$sendCustomMessage("toggleRadioOption", list(inputId = session$ns(id), value = value, enabled = enabled))
    }
    shiny::observe({
      caps <- data_caps()
      # Targeted features and calibration samples: without either, no calibration
      tgt <- is.null(caps) || (isTRUE(caps$has_targeted) && isTRUE(caps$has_calib))
      avg <- isTRUE(input$average_cal_reps)
      log_fit <- isTRUE(input$log_calibration)
      sd_ok <- tgt && !avg && (is.null(caps) || isTRUE(caps$cal0_ok))
      rsd20_ok <- tgt && !avg && (is.null(caps) || isTRUE(caps$reps_ok))
      vis_ok <- is.null(caps) || isTRUE(caps$has_vis)

      for (id in c("optimize_cal_points", "set_minimum_number", "min_cal_points", "average_cal_reps",
                   "keep_all_calcurves", "r2_filter", "min_r2", "log_calibration",
                   "impute_llox")) {
        shinyjs::toggleState(id, condition = tgt)
      }
      # Linear models only: log-scale curves are always straight lines (not used then)
      shinyjs::toggleState("linear_only", condition = tgt && !log_fit)
      # Not only greyed out but also switched off (e.g. ticked in a loaded parameters file): the
      # settings then show what is used, also in the reports
      if (!tgt) {
        for (id in c("optimize_cal_points", "set_minimum_number", "average_cal_reps", "keep_all_calcurves",
                     "r2_filter", "linear_only", "log_calibration")) {
          if (isTRUE(input[[id]])) shiny::updateCheckboxInput(session, id, value = FALSE)
        }
      }
      # Weighting: none without targeted features, only "1" with log-scale fitting
      for (w in c("largestR2", "1_div_x", "1_div_x2")) radio_option("weight", w, tgt && !log_fit)
      radio_option("weight", "1", tgt)
      if (log_fit && !identical(input$weight, "1")) shiny::updateRadioButtons(session, "weight", selected = "1")
      # Linear range: the tolerance is not used with log-scale fitting (SCALiR finds the range then)
      shinyjs::toggleState("linear_range_tol", condition = tgt && !log_fit)
      shinyjs::toggleState("enforce_linear_range", condition = tgt)
      if (!tgt && isTRUE(input$enforce_linear_range)) {
        shiny::updateCheckboxInput(session, "enforce_linear_range", value = FALSE)
      }
      # LOD/LLOQ method "mean + SD" and the RSD% 20% threshold
      radio_option("llox_method", "pt_signal_mean", tgt)
      radio_option("llox_method", "pt_signal_mean_plus_sd", sd_ok)
      if (!sd_ok && identical(input$llox_method, "pt_signal_mean_plus_sd")) {
        shiny::updateRadioButtons(session, "llox_method", selected = "pt_signal_mean")
      }
      if (!rsd20_ok && isTRUE(input$use_rsd20)) shiny::updateCheckboxInput(session, "use_rsd20", value = FALSE)
      shinyjs::toggleState("use_rsd20", condition = rsd20_ok)
      # QC RSD% filter: non-targeted features only without targeted ones
      has_t <- is.null(caps) || isTRUE(caps$has_targeted)   # (Also without calibration samples)
      for (s in c("both", "targeted")) radio_option("rsd_filter_scope", s, has_t)
      shinyjs::toggleState("rsd_max_targeted", condition = has_t)
      if (!has_t && !identical(input$rsd_filter_scope, "nontargeted")) {
        shiny::updateRadioButtons(session, "rsd_filter_scope", selected = "nontargeted")
      }
      # Pre-normalization by the volumetric IS
      if (!vis_ok && isTRUE(input$vis_prenorm)) shiny::updateCheckboxInput(session, "vis_prenorm", value = FALSE)
      shinyjs::toggleState("vis_prenorm", condition = vis_ok)
    })

    # Visible notes under the options greyed out by another option or by the data (not when the
    # data has no targeted features: then all calibration options are greyed out)
    hint_when <- function(id, f) output[[paste0("hint_", id)]] <- shiny::renderUI({
      caps <- data_caps()
      if (!is.null(caps) && !(isTRUE(caps$has_targeted) && isTRUE(caps$has_calib))) return(NULL)
      txt <- f(caps, isTRUE(input$average_cal_reps), isTRUE(input$log_calibration))
      if (length(txt) == 0) NULL else settings_hint(txt)
    })
    hint_when("llox_method", function(caps, avg, log_fit) {
      if (avg) "Mean + SD: not available with averaged calibration replicates (no SD)."
      else if (!is.null(caps) && !isTRUE(caps$cal0_ok)) "Mean + SD: not available, fewer than 2 Cal0 replicates in the data."
    })
    hint_when("use_rsd20", function(caps, avg, log_fit) {
      if (avg) "Not available with averaged calibration replicates (no RSD%)."
      else if (!is.null(caps) && !isTRUE(caps$reps_ok)) "Not available: fewer than 2 replicates per calibration level in the data."
    })
    hint_when("weight", function(caps, avg, log_fit) {
      if (log_fit) "With log-scale fitting only the weighting 1 is possible."
    })
    hint_when("linear_only", function(caps, avg, log_fit) {
      if (log_fit) "Not used with log-scale fitting: the curves are then always straight lines in log-log scale."
    })
    hint_when("linear_range_tol", function(caps, avg, log_fit) {
      if (log_fit) "Not used with log-scale fitting: SCALiR finds the linear range."
    })

    # On "Process data": warnings about the data in a popup; processing starts only on "Proceed"
    # (without warnings it starts at once): Comment values read by a partial match or not
    # recognised; not the intended use without targeted features; batches that use the global
    # curve because of a chosen option
    shiny::observeEvent(input$run_button, {
      caps <- data_caps()
      msgs <- list()
      # Values replaced by their default when a parameters file was loaded (not valid there)
      notes <- if (is.null(param_notes)) NULL else param_notes()
      if (length(notes) > 0) {
        msgs <- c(msgs, list(shiny::tags$li(
          "Settings from the loaded parameters file that were not valid and were set to their default:",
          shiny::tags$ul(lapply(notes, shiny::tags$li)))))
      }
      # Numbers outside their allowed range (or empty): set to the closest allowed value (or the
      # default). Always shown in the popup, so that the corrected values are used on "Proceed".
      fix <- processing_value_corrections(input)
      if (nrow(fix) > 0) {
        for (i in seq_len(nrow(fix))) shiny::updateNumericInput(session, fix$id[i], value = fix$to[i])
        msgs <- c(msgs, list(shiny::tags$li(
          "Settings outside their allowed range were corrected:",
          shiny::tags$ul(lapply(seq_len(nrow(fix)), function(i) shiny::tags$li(
            shiny::tags$code(fix$id[i]), ": ", fix$from[i], " → ", format(fix$to[i]), " (", fix$range[i], ")"))))))
      }
      if (is.null(caps) && length(msgs) == 0) {
        start(Sys.time())
        return()
      }
      # Not read yet: no checks of the data (processing then says that the data must be read)
      if (is.null(caps)) caps <- list(has_targeted = TRUE, has_calib = TRUE)
      # Empty or non-numeric values in the file, read as 0
      mv <- caps$missing_values
      if (!is.null(mv) && nrow(mv) > 0) {
        shown <- utils::head(mv, 10)
        msgs <- c(msgs, list(shiny::tags$li(paste0(
          sum(mv$n), " value(s) in the file are empty or not numbers, in ", nrow(mv), " feature(s): ",
          paste0(shown$feature, " (", shown$n, ")", collapse = ", "), if (nrow(mv) > 10) ", ...", ". ",
          "They were read as 0 (not detected). A data export has a number in every cell, so the file ",
          "may have been edited; please check it."))))
      }
      # Columns whose names contain a required column name but are not it (not used), e.g. "comment"
      cols <- caps$column_notes
      if (!is.null(cols) && nrow(cols) > 0) {
        red <- "color: #D55E00; font-weight: 600;"
        cell <- "padding: 2px 10px; border-bottom: 1px solid #DDD;"
        n_all <- nrow(cols)
        cols <- utils::head(cols, 20)
        rows <- lapply(seq_len(nrow(cols)), function(i) {
          shiny::tags$tr(
            shiny::tags$td(style = cell, shiny::tags$code(style = red, cols$column[i])),
            shiny::tags$td(style = cell, shiny::tags$code(cols$looks_like[i]))
          )
        })
        sticky <- "position: sticky; top: 0; background: #FFF;"
        msgs <- c(msgs, list(shiny::tags$li(
          shiny::tags$b(tags$u("Column naming convention:")),
          "the data file must have these columns, with exactly these names (upper / lower case too):",
          shiny::tags$ul(
            shiny::tags$li(shiny::tags$code("Alignment ID")),
            shiny::tags$li(shiny::tags$code("Metabolite name")),
            shiny::tags$li(shiny::tags$code("Average Mz"), "(or", shiny::tags$code("Quant mass"), ")"),
            shiny::tags$li(shiny::tags$code("Average Rt(min)")),
            shiny::tags$li(shiny::tags$code("Comment"))
          ),
          shiny::tags$b("Columns not following the naming convention "), "(similar names, not used):",
          shiny::div(
            style = "max-height: 260px; overflow-y: auto; margin: 6px 0 8px; display: block; width: fit-content; border: 1px solid #DDD;",
            shiny::tags$table(
              style = "border-collapse: collapse; font-size: 0.9em;",
              shiny::tags$thead(shiny::tags$tr(
                shiny::tags$th(style = paste(cell, sticky), "Column"),
                shiny::tags$th(style = paste(cell, sticky), "Looks like")
              )),
              shiny::tags$tbody(rows)
            )
          ),
          if (n_all > 20) shiny::div(style = "font-size: 0.9em; color: #555; margin-bottom: 6px;",
                                     paste0("The first 20 of ", n_all, " columns are shown.")),
          "Only the exactly named columns are read. If one of these was meant to be used, rename it and upload the file again."
        )))
      }
      # Comment values that do not follow the naming conventions: non-targeted features
      notes <- caps$comment_notes
      if (!is.null(notes) && nrow(notes) > 0 && "looks_like" %in% names(notes)) {
        # One row per Comment value; the ones that look like a convention (likely typos) in red
        red <- "color: #D55E00; font-weight: 600;"
        cell <- "padding: 2px 10px; border-bottom: 1px solid #DDD;"
        # Those that look like a convention (Quant, IS, ...) first, then the most frequent; at most 20
        n_all <- nrow(notes)
        notes <- notes[order(is.na(notes$looks_like), -notes$n, notes$comment), ]
        notes <- utils::head(notes, 20)
        rows <- lapply(seq_len(nrow(notes)), function(i) {
          sus <- !is.na(notes$looks_like[i])
          shiny::tags$tr(
            style = if (sus) red,
            shiny::tags$td(style = cell, shiny::tags$code(style = if (sus) red, notes$comment[i])),
            shiny::tags$td(style = paste(cell, "text-align: right;"), notes$n[i])
          )
        })
        msgs <- c(msgs, list(shiny::tags$li(
          shiny::tags$b(tags$u("Comment naming convention:")),
          shiny::tags$ul(
            shiny::tags$li(shiny::tags$code("Quant"), "targeted compound"),
            shiny::tags$li(shiny::tags$code("Quant\\<IS>"), "targeted compound with specified IS"),
            shiny::tags$li(shiny::tags$code("IS"), "an internal standard"),
            shiny::tags$li(shiny::tags$code("IS\\<compound>"), "an internal standard of a compound"),
            shiny::tags$li(shiny::tags$code("vIS"), "the volumetric internal standard"),
            shiny::tags$li(shiny::tags$code("R"), "a feature to remove (left out when the file is read)"),
            shiny::tags$li("empty: a non-targeted feature")
          ),
          shiny::tags$b("Comments not following naming convention "), "(treated as non-targeted features):",
          # Scrolls within the popup (the header stays visible)
          shiny::div(
            style = "max-height: 260px; overflow-y: auto; margin: 6px 0 8px; display: block; width: fit-content; border: 1px solid #DDD;",
            shiny::tags$table(
              style = "border-collapse: collapse; font-size: 0.9em;",
              shiny::tags$thead(shiny::tags$tr(
                shiny::tags$th(style = paste(cell, "position: sticky; top: 0; background: #FFF;"), "Comment"),
                shiny::tags$th(style = paste(cell, "text-align: right; position: sticky; top: 0; background: #FFF;"),
                               "Features")
              )),
              shiny::tags$tbody(rows)
            )
          ),
          if (n_all > 20) shiny::div(style = "font-size: 0.9em; color: #555; margin-bottom: 6px;",
                                     paste0("The first 20 of ", n_all, " Comment values are shown.")),
          "To use them as intended, correct the Comment column and upload the file again.")))
      }
      batch_txt <- function(b) paste(if (length(b) > 1) "Batches" else "Batch", paste(b, collapse = ", "),
                                     if (length(b) > 1) "have" else "has")
      if (!isTRUE(caps$has_targeted)) {
        msgs <- c(msgs, list(shiny::tags$li(
          tags$b(tags$u("The data has no targeted (Quant) features:")), tags$br(),
          "QuaNTA is made for targeted data with calibration",
          "curves, so this is not its intended use. The data is still processed (normalization, blank",
          "filter, QC RSD% filter of the non-targeted features), without calibration; the options for",
          "targeted compounds are greyed out.")))
      } else if (!isTRUE(caps$has_calib)) {
        msgs <- c(msgs, list(shiny::tags$li(
          "The data has no calibration curve (CalCurve) samples, so the targeted compounds cannot be",
          "quantified. The data is still processed (normalization, blank filter, QC RSD% filter),",
          "without calibration; the calibration options are greyed out.")))
      } else if (isTRUE(caps$multi_batch) && !isTRUE(input$average_cal_reps)) {
        if (identical(input$llox_method, "pt_signal_mean_plus_sd") && length(caps$cal0_fail_batches) > 0) {
          msgs <- c(msgs, list(shiny::tags$li(
            batch_txt(caps$cal0_fail_batches), "fewer than 2 Cal0 replicates, which",
            "the LOD/LLOQ method 'mean + SD' needs: the global curve of all batches is used there. The other",
            "batches use their own curves.")))
        }
        if (isTRUE(input$use_rsd20) && length(caps$reps_fail_batches) > 0) {
          msgs <- c(msgs, list(shiny::tags$li(
            batch_txt(caps$reps_fail_batches), "calibration levels with fewer than",
            "2 replicates, which the RSD% 20% threshold needs: the global curve of all batches is used there. The",
            "other batches use their own curves.")))
        }
      }
      if (length(msgs) == 0) {
        start(Sys.time())
        return()
      }
      shiny::showModal(shiny::modalDialog(
        title = "Please check before processing",
        # Wide, with only its content scrolling (www/quanta.css: .q-proc-popup)
        shiny::div(
          class = "q-proc-popup",
          shiny::tags$ul(msgs),
          shiny::p("Click \"Proceed\" to process the data anyway, or \"Cancel\" to change the file or the settings.")
        ),
        easyClose = FALSE,
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(session$ns("proceed"), "Proceed", class = "btn-primary")
        )
      ))
    })
    shiny::observeEvent(input$proceed, {
      shiny::removeModal()
      start(Sys.time())
    })
    # The notes of a loaded parameters file are shown once: until processing starts
    shiny::observeEvent(start(), {
      if (!is.null(param_notes)) param_notes(NULL)
    }, ignoreInit = TRUE)
    # The minimum number of calibration points is 3 unless set: the hidden field must not keep
    # another value (also one from a loaded parameters file, when the box is not ticked)
    shiny::observe({
      if (!isTRUE(input$set_minimum_number) && !identical(as.numeric(input$min_cal_points), 3)) {
        shiny::updateNumericInput(session, "min_cal_points", value = 3)
      }
    })
  })
  runScriptServer(
    serverId,
    script_path = "scripts/process.R",
    what = "Processing data",
    trigger = start,   # Set on "Process data" (after "Proceed" when there are warnings)
    param_ids = c(
      "optimize_cal_points",
      "average_cal_reps",
      "min_cal_points",
      "keep_all_calcurves",
      "r2_filter",
      "min_r2",
      "linear_only",
      "enforce_linear_range",
      "linear_range_tol",
      "vis_prenorm",
      "rm_outlier",
      "rm_outlier_scope",
      "log_calibration",
      "weight",
      "llox_method",
      "use_rsd20",
      "impute_llox",
      "blank_filter",
      "blank_filter_factor",
      "blank_filter_stat",
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
