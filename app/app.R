## ------------------------------------------------------------------------------------------- #
# This is a Shiny web application.
# ------------------------------------------------------------------------------------------- #

options(readr.show_progress = FALSE)

options(shiny.host = "0.0.0.0")
options(shiny.port = 7579)
options(shiny.maxRequestSize = 500 * 1024^2)

# `box` caches loaded modules for the whole R session. Clear the cache so that re-running the app
# in the same session (e.g. RStudio) picks up edits to `projlib/` instead of stale modules.
box::purge_cache()


# TODO: replace with box::use() + pkg:: qualification at call sites (r-lang-guide / CLAUDE.md),
# deferred for now since call sites are unqualified file-wide and this app can't be run here to verify.
library(shiny)
library(shinyjs)
library(shinyFiles)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(bslib)
library(lubridate)
library(dplyr)
library(echarts4r)
library(DT)
library(plotly)
library(shinycssloaders)

# Larger text in all plots of the app. Shiny draws ggplot2 plots at 72 dpi, so the default 11pt
# text is only ~11 pixels high however large the plot is. Plotly plots converted with
# `ggplotly()` follow this theme too.
QUANTA_PLOT_BASE_SIZE <- 16
ggplot2::theme_set(ggplot2::theme_grey(base_size = QUANTA_PLOT_BASE_SIZE))
# 1) Define theme
custom_theme <- bs_theme(
  version = 5,
  bootswatch = NULL,
  # Lato from www/fonts/ (the @font-face rules are in www/fonts/lato.css, linked in the UI header)
  base_font = font_collection("Lato", "sans-serif"),
  heading_font = font_google("Oswald"),
  code_font = font_google("JetBrains Mono"),
  bg = "#FFFFFF", # white background
  fg = "#000000", # black text


  primary = "black",
  secondary = "#6BCBFF",
  info = "#9ADCFF",
  success = "#3CD070",
  warning = "#FFD66B",
  danger = "#FF6B6B",

  ### >>>>> CARD COLOR UPDATED (BLUE) <<<<<
  "card-bg" = "#608cc4", # QuANTA deep blue 6FAAF7
  "card-border-color" = "#6FAAF7", # darker border to match
  "navbar-bg" = "#0D121C",
  "navbar-fg" = "#FFFFFF",
  "link-color" = "#d6a376", #F4A259
  "link-hover-color" = "#8F8DF7"
)

# Add custom CSS rules
custom_theme <- bs_add_rules(
  custom_theme,
  "
  /* Buttons */


.btn {
  background-color: #F4A259 !important;   /* ORANGE (normal) */
  border-color: #F4A259 !important;
  color: #000000 !important;              /* BLACK TEXT */
  border-width: 2px;
   font-size: 20px
}

.btn:hover, .btn:focus {
  background-color: #FFB733 !important;   /* LIGHT ORANGE on hover */
  border-color: #FFB733 !important;
  color: #000000 !important;              /* BLACK TEXT */
  box-shadow: 0 0 10px rgba(255, 165, 0, 0.5) !important;
}

.btn:active {
  background-color: #E69500 !important;   /* DARKER ORANGE when pressed */
  border-color: #E69500 !important;
  color: #000000 !important;
}

/* Disabled buttons (e.g. report / export until a final normalization method is chosen) */
.btn:disabled, .btn.disabled,
.btn:disabled:hover, .btn.disabled:hover {
  background-color: #C8C8C8 !important;   /* GREY */
  border-color: #C8C8C8 !important;
  color: #6B6B6B !important;
  box-shadow: none !important;
  cursor: not-allowed !important;
}

/* Outline button behavior */
.btn-outline-primary,
.btn-outline-secondary,
.btn-outline-info,
.btn-outline-success,
.btn-outline-warning,
.btn-outline-danger,
.btn-outline-dark,
.btn-outline-light {
  color: #000000 !important;               /* BLACK TEXT */
  border-color: #F4A259 !important;
}

.btn-outline-primary:hover {
  background-color: #F4A259 !important;    /* ORANGE FILL */
  color: #000000 !important;               /* BLACK TEXT */
}

pre.shiny-text-output,
pre.shiny-text-output.monospace {
  background-color: #0A0F18 !important;   /* Dark inner background */
  color: #FFFFFF !important;              /* White text */
  border: 2px solid #F4A259 !important;   /* Orange border */
  padding: 8px 12px !important;
  border-radius: 6px !important;
  font-family: JetBrains Mono, monospace !important;
}

/* Optional: highlight when hovered or focused */
pre.shiny-text-output:hover {
  border-color: #FFB733 !important;       /* Lighter orange hover */
  box-shadow: 0 0 6px rgba(255,165,0,0.4) !important;
}




  /* Inputs */
  .form-control, .selectize-input {
    background-color: #0F1624 !important;
    color: #FFFFFF !important;
    border-color: #1E2A3A !important;
  }

  .form-control:focus, .selectize-input.focus {
    border-color: #F4A259 !important;
    box-shadow: 0 0 6px rgba(255, 77, 166, 0.4) !important;
  }

  /* >>>>> CARD COLORS UPDATED (BLUE) <<<<< */




.card {
  color: #FFFFFF !important;


  overflow: auto;


}

/* Keep card headers white */

.card-header,
.card-header *,
.bslib-card-header,
.bslib-card-header *,
.bslib-card-header-title {
  /* Scales with the window width (min 1.25rem, max 2rem) so the titles fit on one line */
  font-size: clamp(1.25rem, 1.9vw, 2rem) !important;
  font-weight: 700 !important;
  line-height: 1.2 !important;   /* relative to the font size, so wrapped titles don't overlap */
}

/* Card titles on one line; they wrap only in small windows (below 992px) */
@media (min-width: 992px) {
  .card-header > div:first-child {
    white-space: nowrap !important;
  }
}



  /* ---------- NAV BAR: white background + black text ---------- */
.navbar, .navbar-nav, .navbar-brand {
  background-color: #FFFFFF !important;
  color: #000000 !important;
}

.navbar .nav-link,
.navbar .navbar-brand,
.navbar .navbar-nav .nav-link {
  color: #000000 !important;
}

/* Hover effect */
.navbar .nav-link:hover {
  color: #F4A259 !important;  /* optional orange hover */
}

/* ---------- NAV TABS (the page_navbar panels) ---------- */
.nav-tabs {
  background-color: #FFFFFF !important;
  border-bottom: 1px solid #CCCCCC !important;
}

.nav-tabs .nav-link {
  color: #000000 !important;
  background-color: #FFFFFF !important;
  border: 1px solid #CCCCCC !important;
}

/* Hover effect */
.nav-tabs .nav-link:hover {
  color: #F4A259 !important;
  background-color: #F7F7F7 !important;
}

/* Active tab styling */
.nav-tabs .nav-link.active {
  color: #000000 !important;
  background-color: #FFFFFF !important;
  border: 2px solid #F4A259 !important;   /* orange outline */
  border-bottom: 2px solid #FFFFFF !important; /* seamless effect */
  font-weight: bold;
}


/* Base table */
table.dataTable {
  background-color: #FFFFFF !important;
  color: #000000 !important;
  border-collapse: collapse !important;
}

/* Header */
table.dataTable thead th {
  background-color: #EAF2FF !important;   /* light blue header */
  color: #003366 !important;
  font-weight: 600;
  border-bottom: 2px solid #D0D7E5 !important;
}



/* Body rows */
table.dataTable tbody tr {
  background-color: #FFFFFF !important;
}

/* Zebra striping — SOFT, NO ORANGE */
table.dataTable tbody tr:nth-child(even) {
  background-color: #F8FAFF !important;
}

/* Hover effect */
table.dataTable tbody tr:hover {
  background-color: #EFF4FF !important;
}


/* Remove Bootstrap primary color bleed */
table.dataTable.no-footer {
  border-bottom: 1px solid #D0D7E5 !important;
}



/* FAILED rows */
table.dataTable tbody tr.failed-row td {
  background-color: lightcoral !important;
  color: black !important;
}

/* SELECTED row — must be stronger */

table.dataTable tbody tr.selected,
table.dataTable tbody tr.selected > td {
  background: black !important;
  --bs-table-bg: black !important;
  --bs-table-accent-bg: black !important;
  color: white !important;
}

/* ✅ also override hover-selected conflict */
table.dataTable tbody tr.selected:hover,
table.dataTable tbody tr.selected:hover > td {
  background-color: black !important;
  color: white !important;
}




/* ✅ FORCE checkbox label to stay inline */
.form-check-label {
  white-space: nowrap !important;
  display: inline-flex !important;
  flex-wrap: nowrap !important;
}

/* ✅ Ensure layout doesn't stack text */
.form-check {
  display: flex !important;
  align-items: center !important;
  flex-wrap: nowrap !important;
}


/* =========================================================
   FINAL FIX — FORCE ALL KABLE TABLE TEXT TO BLACK
   ========================================================= */


[data-bs-theme] .card table,
[data-bs-theme] .card table th,
[data-bs-theme] .card table td,
[data-bs-theme] .card table.table,
[data-bs-theme] .card table.table th,
[data-bs-theme] .card table.table td {
  color: #000000 !important;
}



  "
)


# Echarts font
e_common(
  font_family = "PT Sans",
  theme = NULL
)


spinner <- function(x, size = 1.5) {
  withSpinner(x, type = 4, color = "#0072B2", size = size,
              caption = "Loading ... Do not refresh or change tab")
}
# Normalization methods (matrix IDs) and their labels in the UI
NORM_METHOD_LABELS <- c(
  "LOESS" = "loess_norm",
  "Closest RT" = "closest_norm",
  "Multivariate (PCA)" = "mvis_norm",
  "Without normalization" = "raw"
)

quanta_card <- function(title, ...) {
  card(
    card_header(
      div(title, style = "margin-bottom: 10px;"),
      div(style = "height:3px; background:white; width:100%;")
    ),
  ...)
}



quanta_card_resize <- function(title, side, ...) { #"qc-left", "qc-right"
    div(
      id = side,
      card(
        card_header(
          div(title, style = "margin-bottom: 10px;"),
          div(style = "height:3px; background:white; width:100%;")
        ),
      ...)
    )
}
section <- function(title, ...) div(style = "padding: 0;", h3(title, style = "margin: 0;"), ...)

################################################################################################
######################################## UI ####################################################
################################################################################################

ui <-
  page_navbar(
    theme = custom_theme,
    title = tags$img(src = "QuaNTA.jpg", height = "40px", class = "navbar-logo"),
    window_title = "QuaNTA",   # text in the browser tab; fixes the last warning
    header = tags$head(
      tags$link(rel = "icon", type = "image/jpeg", href = "QuaNTA_square.jpg"),
      tags$script(src = "split.min.js"),
      tags$link(rel = "stylesheet", href = "fonts/lato.css"),   # Lato font (base_font of the theme)
      shinyjs::useShinyjs(),   # Enable / disable buttons from the server
      # Start a download from the server: clicks the (hidden) download link with the given ID.
      # Used when a file is ready after a background task (external report, session data).
      tags$script(HTML("
        Shiny.addCustomMessageHandler('quantaClickDownload', function(id) {
          var a = document.getElementById(id);
          if (a) a.click();
        });
      ")),
      tags$style(HTML("
        .navbar-brand img,
        .navbar-logo {
          height: 60px !important;
          width: auto !important;
          border-radius: 0 !important;   /* prevents forced rounding */
          object-fit: contain !important;
        }

        /* Reduce navbar vertical padding */
        .navbar, .navbar-nav .nav-link {
          padding-top: 4px !important;
          padding-bottom: 4px !important;
        }

        /* Optionally reduce brand/logo spacing */
        .navbar-brand {
          padding-top: 0 !important;
          padding-bottom: 0 !important;
        }

        .navbar-nav .nav-link {
          font-size: 1.2rem !important;   /* Increase text size */
          font-weight: 500;
        }

        table.dataTable tbody tr.selected td {
          background-color: black !important;
          color: white !important;
        }
        
        .split-pane {
          display: flex;
          flex-direction: row;
          width: 100%;
        }
        .split-pane > div {
          overflow: auto;   /* scroll instead of clipping when a pane gets narrow */
          min-width: 0;      /* let flex children shrink below content width */
        }
        .gutter {
          background-color: #F4A259;
          background-repeat: no-repeat;
          background-position: 50%;
          cursor: col-resize;
        }
        .gutter.gutter-horizontal {
          width: 8px;
        }
      "))
    ),
    nav_panel(
      title = HTML("Input and parameters  <strong> >> </strong>"),
      layout_columns(
        col_widths = c(4, 4, 4),
        card(
          card_header(
            div(
              "Input data settings",
              style = "margin-bottom: 10px;" # small spacing (optional)
            ),
            div(
              style = "height:3px; background:white; width:100%;"
            )
          ),
          sessionLoadUI("session_data"),   # Load saved session data (.rds), R/sessionData.R
          .hr(),
          msdialInfoUI("data_info")
        ),
        card(
          card_header(
            div(
              "Data processing settings",
              style = "margin-bottom: 10px;" # small spacing (optional)
            ),
            div(
              style = "height:3px; background:white; width:100%;"
            )
          ),
          runReadMsdialUI("read_msdial"),
          runProcessUI("proc"),
          sessionSaveUI("session_data"),   # Save session data (.rds), R/sessionData.R
        ),

        quanta_card("Log",
                    div(
                      "MS-Dial reading log:",
                      style = "margin: 0px 0 0px 0; font-weight: 600;"
                    ),
                    runReadMsdialUI_2("read_msdial"),
                    div(
                      "Processing log:",
                      style = "margin: 0px 0 0px 0; font-weight: 600;"
                    ),
                    runProcessUI_2("proc")),
      )
    ),
    nav_panel(
      title = HTML("Overview  <strong> >> </strong>"),
      div(
        id = "ov-split",
        class = "split-pane",
        #Card
        quanta_card_resize("Tables", "ov-left",
                    section("Samples by category/class (2.1.1)",
                            spinner(DT::DTOutput("tb_2_1_1"))),
                    section("The distribution of the features across m/z and retention time (2.2)",
                            spinner(DT::DTOutput("tb_2_1_2_2"))),
                    section("Calibration curve samples - quality control samples (2.1.3)",
                            spinner(htmlOutput("tb_2_1_3"))),
                    section("Features (2.2)",
                            spinner(htmlOutput("tb_2_2")))),
        #Card
        quanta_card_resize("Figures", "ov-right",
                    section("The distribution of the features across m/z and retention time (2.2)",
                            spinner(plotOutput("ggplot1_2_2"))),
                    section("The distribution of the features (2.2)",
                            spinner(plotOutput("ggplot2_2_2"))),
                    section("Distribution of the values (2.3)",
                            spinner(plotOutput("ggplot2_3"))))
        
      ),
      tags$script(HTML("
        $(document).on('shiny:sessioninitialized', function() {
          if (typeof Split === 'undefined') {
            console.warn('Split.js did not load — check app/www/split.min.js exists.');
            return;
          }
          Split(['#ov-left', '#ov-right'], {
            sizes: [50, 50],
            minSize: 250,
            gutterSize: 8,
            onDragEnd: function() { window.dispatchEvent(new Event('resize')); }
          });
        });
      "))
    ),
    nav_panel(
      title = HTML("Quality control and Normalization <strong> >> </strong>"),
      div(
        id = "qc-split",
        class = "split-pane",
        #Card
        quanta_card_resize("Tables", "qc-left",
                    section("Internal standards (3.1)",
                            spinner(DT::DTOutput("tb_3_1")),
                            spinner(verbatimTextOutput("df_selected_row")),
                            spinner(htmlOutput("text_3_1_1"))),
                    section("Failed internal standards (3.1.1)",
                            htmlOutput("text_3_1_1")),
                    section("Outlier samples (3.1.2)",
                            spinner(uiOutput("wrap_outlier")),
                            h3(
                              "Raw RSD% (3.1.2)",
                              style = "margin: 0;"
                            ),
                            spinner(DT::DTOutput("tb_3_4_1")))),
        #Card
        quanta_card_resize("Figures", "qc-right",
                    section("Distribution of the values of individual IS features (3.1.3)",
                            spinner(plotOutput("ggplot3_1_3")),
                            spinner(uiOutput("remove_failedIS_outlier"))),
                    div(
                      style = "padding: 0; display: flex; gap: 20px; align-items: flex-start;",
                      
                      div(
                        style = "width: 100%;",
                        h3(
                          "Samples by internal standards (3.2.1.1/2)",
                          style = "margin: 0;"
                        ),
                        br(),
                        radioButtons(
                          "group_choice",
                          "Samples by injection order:",
                          choices = c(
                            "Class" = "class",
                            "Batch ID" = "batch",
                            "Internal Standard" = "internal_std"
                          ),
                          selected = character(0),
                          inline = TRUE # ✅ no selection
                        ),
                        spinner(uiOutput("vis_or_text_3_2_1_1"))
                      )
                    ),
                    div(
                      # Only shown once a grouping is chosen above; otherwise its empty
                      # 400px placeholder leaves a gap
                      shiny::conditionalPanel(
                        condition = "input.group_choice",
                        spinner(plotlyOutput("ggplot3_2_1_2"))
                      ),
                      h3(
                        "Comparison of normalization methods 3.2.3",
                        style = "margin: 0;"
                      ),
                      spinner(plotlyOutput("ggplot3_2_3", height = "550px")),
                      h3(
                        "RSD% values (3.2.3)",
                        style = "margin: 0;"
                      ),
                      spinner(DT::DTOutput("tb_3_2_3")),
                    )
                  )
      ),
      tags$script(HTML("
        $(document).on('shiny:sessioninitialized', function() {
          if (typeof Split === 'undefined') {
            console.warn('Split.js did not load — check app/www/split.min.js exists.');
            return;
          }
          Split(['#qc-left', '#qc-right'], {
            sizes: [50, 50],
            minSize: 250,
            gutterSize: 8,
            onDragEnd: function() { window.dispatchEvent(new Event('resize')); }
          });
        });
      "))
    ),
    nav_panel(
      title = HTML("Calibration and Quantification <strong> >> </strong>"),
      layout_columns(
        col_widths = c(6, 6),
        quanta_card("Tables",
                    shiny::radioButtons(
                      inputId = "norm_method",
                      label = "Normalization method",
                      choices = NORM_METHOD_LABELS,   # Updated to the computed ones in the server
                      selected = "loess_norm",
                      inline = TRUE
                    ),
                    # Batch shown in the tables and figures of this panel (choices set in the server)
                    shiny::selectInput(
                      inputId = "calib_batch",
                      label = "Batch",
                      choices = NULL
                    ),
                    div(
                      style = "padding: 0;",
                      h3(
                        "Calibration curve samples (4.1)",
                        style = "margin: 0;"
                      ),
                      spinner(DT::DTOutput("tb_4_1")),
                    ),
                    div(
                      style = "padding: 0;",
                      h3(
                        "Calibration curve limits (4.2)",
                        style = "margin: 0;"
                      ),
                      spinner(DT::DTOutput("tb_4_2")),
                    ),
                    div(
                      style = "padding: 0;",
                      h3(
                        "Removal of chemicals with no measurement samples (4.5)",
                        style = "margin: 0;"
                      ),
                      spinner( DT::DTOutput("tb_4_5")),
                    )),
        quanta_card("Figures",
                    # Which calibration points to show for a chemical quantified with the global
                    # curve: all points of the global curve, or only those of the selected batch
                    div(
                      style = "display: flex; gap: 16px; align-items: center; flex-wrap: wrap;",
                      shiny::selectInput(
                        inputId = "calib_points_view",
                        label = "Calibration points shown",
                        choices = NULL,
                        width = "260px"
                      ),
                      shiny::uiOutput("calib_points_note", style = "flex: 1; min-width: 200px;")
                    ),
                    div(
                      h3(
                        "Calibration curves per chemical  (4.3)",
                        style = "margin: 0;"
                      ),
                      div(
                        style = "width: 100%;", # aspect-ratio: 4 / 3;",   # ✅ shorter height
                        spinner(plotlyOutput("ggplot4_3")),
                      )
                    ),
                    div(
                      h3(
                        "Calibration curves of all measurement samples (4.4.2)",
                        style = "margin: 0;"
                      ),
                      
                      
                      div(
                        style = "width: 100%;",
                        spinner(plotlyOutput("ggplot4_4_2")),
                      )
                    ),
                    div(
                      h3(
                        "Concentration (5.1)",
                        style = "margin: 0;"
                      ),
                      div(
                        style = "width: 100%;", # ✅ shorter height
                        spinner(plotOutput("ggplot5_1")),
                      )
                    )
                  )
      )
    ),
    nav_panel(
      title = HTML("Reports&nbsp;📄&nbsp <strong>  </strong>"),
      # Final normalization method: used by both reports and the exported tables. Nothing is
      # pre-selected; the generate / export buttons stay disabled until a method is chosen.
      div(
        style = "width: 40%; min-width: 320px;",
        quanta_card("Final normalization method",
                    spinner(uiOutput("final_norm_ui")))
      ),
      layout_columns(
        col_widths = c(9, 3),
        card(
          card_header(
            div(
              "Report",
              style = "margin-bottom: 10px;" # small spacing (optional)
            ),
            div(
              style = "height:3px; background:white; width:100%;"
            )
          ),
          genReportUI("report"),


          ###################################################################
          spinner(uiOutput("the_report")),
          ##############################################################


          div(
            class = "d-flex justify-content-center my-2",
            downloadButton(
              outputId = "download_report",
              label    = "Download HTML Report",
              class    = "btn btn-sm btn-primary",
              style    = "font-size:18px; padding:12px 24px;"
            )
          )
        ),
        quanta_card("Extract data",
                    # Exports the normalized, blank-subtracted and concentration tables of the
                    # final normalization method selected at the top of this panel
                    runScriptUI("export_data", label = "Export data into tables", download=T)
                  )
      )
    ),
    # Help icon in the top-right corner; the popup content is in R/helpPopup.R
    nav_spacer(),
    nav_item(helpPopupUI("help"))
  )

####################################################################################################################
####################################################################################################################
################################################################################################
###################################### SERVER ###################################################
################################################################################################

server <- function(input, output, session) {

  # Help popup (question-mark icon in the navbar)
  helpPopupServer("help")

  # ---- 1. Private workspace of this session (deleted when the session ends) ----
  ws <- create_session_workspace(session)
  
  # ---- 2. Parameters: come from the inputs; a loaded file only pre-fills them ----
  data_info <- msdialInfoServer(
    "data_info",
    ws = ws,
    processing_params = shiny::reactive(collect_processing_params(input))
  )
  shiny::observeEvent(data_info$loaded_processing, {
    apply_processing_params(session, data_info$loaded_processing)
  }, ignoreNULL = TRUE)
  
  # ---- Normalization methods computed by process.R and the final choice for report/export ----
  # (`to_report_r()` is defined in server_setup.R; reactives are evaluated lazily)
  # Matrix IDs computed in the processed data, e.g. "raw", "loess_norm", "closest_norm", "mvis_norm"
  available_norm_r <- shiny::reactive({
    ids <- to_report_r()[["matrix ids to blk subt"]]
    ids[order(match(ids, NORM_METHOD_LABELS))]
  })
  # Method with the lowest median QC RSD% (`loess_norm` for data processed by older versions)
  best_norm_r <- shiny::reactive({
    best <- to_report_r()[["best normalization"]]
    if (is.null(best)) "loess_norm" else best
  })
  # The final method used for the report and the exported tables: "auto" resolves to the best
  # Nothing is pre-selected: the user must choose (the report / export buttons stay disabled)
  final_norm_chosen_r <- shiny::reactive({
    sel <- input$final_norm_method
    !is.null(sel) && length(sel) == 1 && (sel == "auto" || sel %in% available_norm_r())
  })
  final_norm_method_r <- shiny::reactive({
    shiny::req(final_norm_chosen_r())
    sel <- input$final_norm_method
    if (sel == "auto") best_norm_r() else sel
  })

  # ---- 3. Child processes; each returns a reactive that changes when a run has finished ----
  read_done <- runReadMsdialServer("read_msdial", data_info = data_info, ws = ws)
  proc_done <- runProcessServer("proc", data_info = data_info, ws = ws)
  # Save / load the session data (.rds); changes when data has been loaded
  session_loaded <- sessionDataServer(
    "session_data",
    data_info = data_info,
    ws = ws,
    processing_params = shiny::reactive(collect_processing_params(input))
  )
  
  runScriptServer2(
    "export_data",
    script_path = "scripts/export_data.R",
    what = "Export data into tables",
    data_info = data_info,
    ws = ws,
    norm_method = final_norm_method_r
  )
  
  # ---- 4. Cached data reactives: raw_se_r, to_report_r, proc_r, ... ----
  # Keep whatever mechanism you use today to load this file, but it must run here, i.e. after
  # `ws`, `data_info`, `read_done` and `proc_done` exist.
  
  # ---- 5. Report ----
  report_obj <- genReportServer(
    "report",
    data_info = data_info,
    ws = ws,
    norm_method = final_norm_method_r
  )
  
  output$the_report <- shiny::renderUI({
    # Re-reading / a new upload deletes the report (see below / msdialInfo.R): hide it then
    input[["read_msdial-run_button"]]
    input[["data_info-input_file_upload"]]
    shiny::req(report_obj$report_ready())
    path <- report_obj$report_path()
    shiny::req(file.exists(path))
    # Relative URL (no leading "/"): keeps working behind a proxy / sub-path
    tags$iframe(
      src = paste0(ws$prefix, "/", basename(path), "?t=", as.numeric(Sys.time())),
      style = "width:100%; height:3000px; border:none;"
    )
  })
  
  output$download_report <- shiny::downloadHandler(
    filename = function() paste0("Internal Report ", final_norm_method_r(), ".html"),
    content = function(file) {
      path <- report_obj$report_path()
      if (!file.exists(path)) stop("Generate the report first.")
      file.copy(path, file, overwrite = TRUE)
    }
  )
  
  source("server_setup.R", local = TRUE)

  # ---- Re-reading makes earlier processing results outdated: remove them ----
  # The processed data, exported tables and reports of this session are deleted when "Read MS-Dial
  # output files" is clicked (before the read starts: higher priority than the module's observer),
  # so the QC / Calibration / Reports tabs stay empty until the data is processed again.
  shiny::observeEvent(input[["read_msdial-run_button"]], {
    f <- data_info[["input_file"]]
    if (!is.null(f) && nzchar(f)) unlink(c(rds_file("proc"), rds_file("to_report")))
    unlink(list.files(ws$tables, full.names = TRUE), recursive = TRUE)
    unlink(list.files(ws$report, full.names = TRUE), recursive = TRUE)
  }, priority = 100)

  # ---- Step buttons are disabled (greyed out) until their input exists ----
  # - "Read MS-Dial output files": an MS-DIAL file has been uploaded (or restored from a session)
  # - "Process data": the read data exists (after reading, or restored from a session)
  shiny::observe({
    read_done()
    session_loaded()
    input[["data_info-input_file_upload"]]   # A new upload deletes the earlier read data
    f <- data_info[["input_file"]]
    has_input <- !is.null(f) && nzchar(f) && file.exists(f)
    has_parsed <- has_input && file.exists(rds_file(""))
    shinyjs::toggleState("read_msdial-run_button", condition = has_input)
    shinyjs::toggleState("proc-run_button", condition = has_parsed)
  }, priority = -10)   # After the upload handler has deleted the earlier data

  # ---- Final normalization method (top of the Reports panel) ----
  output$final_norm_ui <- shiny::renderUI({
    ids <- available_norm_r()
    best <- best_norm_r()
    rsd <- to_report_r()[["median RSD% per normalization"]]
    basis <- to_report_r()[["median RSD% basis"]]
    label_of <- function(id) {
      nm <- names(NORM_METHOD_LABELS)[match(id, NORM_METHOD_LABELS)]
      if (is.na(nm)) nm <- id
      if (!is.null(rsd) && !is.na(rsd[id])) {
        nm <- paste0(nm, " (median RSD ", round(rsd[id], 1), "%)")
      }
      nm
    }
    basis_txt <- if (is.null(basis) || is.na(basis)) {
      paste("could not be determined (no replicate QC or calibration samples, or data processed",
            "by an older version), so LOESS is used.")
    } else if (basis == "QC") {
      "is the method with the lowest median RSD% across the replicate QC samples."
    } else {
      paste("is the method with the lowest median RSD% across the replicates of each calibration",
            "level (there are no replicate QC samples).")
    }
    choices <- c("auto", ids)
    names(choices) <- c(paste0("Best: ", label_of(best)), vapply(ids, label_of, character(1)))
    # Nothing pre-selected (forces a decision); keep an earlier choice when re-processed
    selected <- shiny::isolate(input$final_norm_method)
    if (is.null(selected) || !selected %in% choices) selected <- character(0)
    shiny::tagList(
      # Let the long labels wrap in the narrow column (the app's CSS keeps radio labels on one line)
      shiny::tags$style(shiny::HTML(
        "#final_norm_method .form-check-label { white-space: normal !important; }"
      )),
      shiny::radioButtons(
        inputId = "final_norm_method",
        label = paste("Choose the normalization method used for the internal and external reports",
                      "and the exported tables"),
        choices = choices,
        selected = selected,
        inline = FALSE,
        width = "100%"
      ),
      shiny::helpText(
        "\"Best\"", basis_txt,
        "RSD% is computed on the targeted features (all non-internal-standard features in",
        "non-target mode) before blank subtraction."
      ),
      if (!"mvis_norm" %in% ids) {
        shiny::helpText("Multivariate (PCA) normalization could not be computed for this data;",
                        "see the processing messages.")
      }
    )
  })

  # Report and export buttons are disabled (greyed out) until a final method has been chosen
  shiny::observe({
    chosen <- isTRUE(tryCatch(final_norm_chosen_r(), error = function(e) FALSE))
    for (id in c("report-gen_external", "report-gen_internal", "export_data-run_button")) {
      shinyjs::toggleState(id, condition = chosen)
    }
  })

  # Calibration tab: offer only the computed methods; start from the best one after processing
  shiny::observeEvent(to_report_r(), {
    ids <- available_norm_r()
    shiny::updateRadioButtons(
      session, "norm_method",
      choices = NORM_METHOD_LABELS[NORM_METHOD_LABELS %in% ids],
      selected = best_norm_r(),
      inline = TRUE
    )
  })

  r <- reactiveValues()

  is_target_mode <- reactive({
    req(raw_se_r())
    
    md <- SumExp::metadata(raw_se_r())
    
    !isTRUE(md$is_non_target_mode)
  })

  output$tb_2_1_1 <- DT::renderDataTable({
    # shiny::req(raw_se)
    # catg <- util$ctrl_smpl_cat( raw_se)


    shiny::req(raw_se_r())

    catg <- util$ctrl_smpl_cat(raw_se_r())

    catg[catg == ""] <- "Sample"

    catg_cl <- tibble::tibble(
      Category = catg,
      Class = SumExp::col_df(raw_se_r())$Class,
    ) |>
      dplyr::mutate(Class = ifelse(Category %in% c("CalCurve", "QC"), "", Class)) |>
      tidyr::unite("catg_cl", c(Category, Class), sep = ":", remove = FALSE)
    tb <- janitor::tabyl(catg_cl, catg_cl) |>
      janitor::adorn_pct_formatting() |>
      dplyr::right_join(dplyr::distinct(catg_cl), y = _, by = "catg_cl") |>
      dplyr::arrange(Category, Class)
    stopifnot(anyDuplicated(tb$catg_cl) == 0) # Ensure no duplicated category-class combinations

    tb <- tb |>
      dplyr::select(-catg_cl)


    tb <- as.data.frame(tb,
      check.names = FALSE
    )
    colnames(tb) <- c("Category", "Class", "Number of Samples", "Percent")

    DT::datatable(
      tb,
      options = list(
        scrollX = TRUE,
        lengthChange = FALSE,
        searching = FALSE,
        paging = FALSE, # remove Previous/Next
        info = FALSE, # remove "Showing X to Y of Z entries"
        dom = "t",
        headerCallback = JS(
          "function(thead){",
          "$(thead).find('th').css({'color': 'green',
                                            'font-weight': 'bold'});", "}"
        ),
        columnDefs = list(
          list(className = "dt-center", targets = "_all") # <--- centers all text
        )
      )
    ) |>
      DT::formatStyle(
        columns = names(tb),
        color = "#000000",
        backgroundColor = "#FFFFFF",
        target = "cell"
      ) |>
      DT::formatStyle(
        names(tb),
        backgroundColor = "#F8FAFF",
        target = "row"
      )
  })


  ################################################################################################################
  ### This is table 2.1.2.2
  #### There is problem of is target mode


  output$tb_2_1_2_2 <- DT::renderDataTable({
    shiny::req(raw_se_r())
    
    # -------------------------------------------------------------------
    # 1. Calibration sample table (dataframe)
    # -------------------------------------------------------------------

    df_show <- if (is_target_mode()) {
      SumExp::col_df(raw_se_r()) |>
        dplyr::filter(util$ctrl_smpl_cat(raw_se_r()) == "CalCurve") |>
        dplyr::arrange(c_conc) |>
        dplyr::select(sample_name, c_conc)
    } else {
      tibble::tibble(
        sample_name = character(),
        c_conc = numeric()
      )
    }

    # Ensure it is a plain data.frame if needed
    df_show <- as.data.frame(df_show,
      check.names = FALSE
    )

    # -------------------------------------------------------------------
    # 2. Concentration frequency table (dataframe)
    # -------------------------------------------------------------------

    df_conc_summary <- df_show |>
      dplyr::count(c_conc, name = "Number_of_Samples") |>
      dplyr::arrange(c_conc)

    df_conc_summary <- as.data.frame(df_conc_summary,
      check.names = FALSE
    )

    colnames(df_conc_summary) <- c("Calibrant Concentration", "Number of Samples")

    DT::datatable(
      df_conc_summary,
      options = list(
        scrollX = TRUE,
        lengthChange = FALSE,
        searching = FALSE,
        paging = FALSE, # remove Previous/Next
        info = FALSE, # remove "Showing X to Y of Z entries"
        dom = "t",
        headerCallback = JS(
          "function(thead){",
          "$(thead).find('th').css({'color': 'green',
                                            'font-weight': 'bold'});", "}"
        ),
        columnDefs = list(
          list(className = "dt-center", targets = "_all") # <--- centers all text
        )
      )
    ) |>
      DT::formatStyle(
        columns = names(df_conc_summary),
        color = "#000000",
        backgroundColor = "#FFFFFF",
        target = "cell"
      ) |>
      DT::formatStyle(
        names(df_conc_summary),
        backgroundColor = "#F8FAFF",
        target = "row"
      )
  })


  ################################################################################################################
  ### This is table 2.1.3


  output$tb_2_1_3 <- renderUI({
    shiny::req(raw_se_r())
    
    qc_lst <- show$extract_qc_samples_to_list(raw_se_r())
    if (length(qc_lst) == 0) return(HTML("<p><em>No QC samples were found.</em></p>"))
    tb_2_1_3 <- qc_lst |>
      sapply(ncol) |>
      knitr::kable(col.names = c("Class", "Number of Replicates")) |>
      kableExtra::kable_styling(full_width = FALSE)
    tb_2_1_3 |>
      HTML()
  })

  ################################################################################################################
  ### This is table 2.1.3


  output$tb_2_2 <- renderUI({
    shiny::req(raw_se_r())
    
    v <- util$std_type(raw_se_r())
    tb_2_2 <- replace(v, v == "", "Not targeted") |>
      show$kable_number_of(what = "Features")
    tb_2_2 |>
      HTML()
  })

  ################################################################################################################
  ### This is figure 2.2

  output$ggplot1_2_2 <- renderPlot({
    shiny::req(raw_se_r())
    
    colors <- local({
      x <- unique(util$std_type(raw_se_r()))
      n_non_target <- sum(x == "")
      col <- scales::hue_pal()(length(x) - n_non_target)
      col <- rlang::set_names(col, x[x != ""])
      if (n_non_target > 0) {
        c(col, "grey") # grey for non-targeted
      } else {
        col
      }
    })
    df1 <- SumExp::row_df(raw_se_r())
    ggplot1_2_2 <- ggplot(df1) +
      geom_point(aes(x = .rt, y = mz, color = .std_type),
        alpha = ifelse(df1$.std_type == "", 0.1, 1)
      ) +
      labs(
        x = show$label_if_has(df1$.rt),
        y = show$label_if_has(df1$mz),
        color = show$label_if_has(df1$.std_type),
      ) +
      scale_color_manual(values = colors)

    ggplot1_2_2
  })


  ################################################################################################################
  ### This is figure 2.2

  output$ggplot2_2_2 <- renderPlot({
    shiny::req(raw_se_r())
    
    df1 <- SumExp::row_df(raw_se_r())
    ggplot2_2_2 <- ggplot(df1) +
      geom_histogram(aes(x = mz), binwidth = 0.5) +
      labs(
        x = show$label_if_has(df1$mz),
        y = "Number of features"
      )
    ggplot2_2_2
  })


  ################################################################################################################
  ### This is figure 2.3

  output$ggplot2_3 <- renderPlot({
    shiny::req(raw_se_r())
    
    p <- SumExp::ggplot(raw_se_r()) +
      geom_histogram(aes(x = raw), bins = 50)
    ggplot2_3 <- gridExtra::grid.arrange( # Two ggplots next to each other
      p + labs(title = "Linear scale", x = "Values"),
      p + scale_x_log10() + labs(title = "Logarithmic scale", x = "Values"),
      ncol = 2
    )
    ggplot2_3
  })

  ################################################################################################################
  ### This is table 3.1


  output$tb_3_1 <- DT::renderDataTable({
    shiny::req(pre_norm_se_r())
    
    all_rts <- util$retention_time(pre_norm_se_r())
    overall_rt_range <- range(all_rts)
    targetted_rt_range <- range(all_rts[util$is_targeted_feature(pre_norm_se_r())])
    rt <- range(all_rts[util$is_internal_std(pre_norm_se_r())])

    box::use(projlib / proc[count_zeros_per_feature])

    df <- SumExp::row_df(internal_std_se_r()) |>
      dplyr::mutate(
        num_zeros = count_zeros_per_feature(internal_std_se_r()[["raw"]]),
        rsd = show$compute_rsd_per_feature(internal_std_se_r()[["raw"]]),
      )
    # Table of internal standard features
    # kable_internal_std <- function(se) {
    df <- df |>
      dplyr::select(feature_name, .rt, mz, rsd, num_zeros) |>
      dplyr::mutate(
        num_zeros = ifelse(num_zeros == 0, "", num_zeros) |>
          labelled::copy_labels_from(num_zeros)
      )

    df <- data.frame(df,
      check.names = FALSE
    )


    is_failed_istd <- to_report_r()[["is failed internal std."]]


    df$is_failed_istd <- is_failed_istd # ensure it's inside render
    DT::datatable(
      df,
      selection = "single", # or "multiple"
      rownames = FALSE,

      options = list(
        scrollX = TRUE,
        lengthChange = FALSE,
        searching = FALSE,
        paging = FALSE, # remove Previous/Next
        info = FALSE, # remove "Showing X to Y of Z entries"
        dom = "t",
        headerCallback = JS(
          "function(thead){",
          "$(thead).find('th').css({'color': 'green',
                                            'font-weight': 'bold'});", "}"
        ),

        rowCallback = JS("
  function(row, data) {

    $(row).removeClass('failed-row');

    if (data[data.length - 1] == true) {
      $(row).addClass('failed-row');
    }
  }
"),
        columnDefs = list(
          list(targets = ncol(df) - 1, visible = FALSE) # hide flag column
        )
      )
    ) |>
      DT::formatRound(
        columns = "rsd",
        digits = 2
      ) |>
      DT::formatStyle(
        columns = "is_failed_istd",
        target = "cell",
        visible = FALSE
      )
  })

  output$df_selected_row <- renderPrint({
    req(pre_norm_se_r())
    
    all_rts <- util$retention_time(pre_norm_se_r())
    overall_rt_range <- range(all_rts)
    targetted_rt_range <- range(all_rts[util$is_targeted_feature(pre_norm_se_r())])
    rt <- range(all_rts[util$is_internal_std(pre_norm_se_r())])


    box::use(projlib / proc[count_zeros_per_feature])


    df <- SumExp::row_df(internal_std_se_r()) |>
      dplyr::mutate(
        num_zeros = count_zeros_per_feature(internal_std_se_r()[["raw"]]),
        rsd = show$compute_rsd_per_feature(internal_std_se_r()[["raw"]]),
      )
    # Table of internal standard features
    # kable_internal_std <- function(se) {
    df <- df |>
      dplyr::select(feature_name, .rt, mz, rsd, num_zeros) |>
      dplyr::mutate(
        num_zeros = ifelse(num_zeros == 0, "", num_zeros) |>
          labelled::copy_labels_from(num_zeros)
      )

    df <- data.frame(df,
      check.names = FALSE
    )


    selected <- input$tb_3_1_rows_selected


    if (length(selected)) {
      df[selected, ]
    } else {
      "Click on a row above."
    }
  })
  box::use(patchwork)
  box_scatter_plot_of_one_feature <- function(se, ii, widths, classes_color) {
    feature_name <- SumExp::row_df(se)$feature_name[ii]
    se_tbl <- SumExp::as_tibble(se[ii, ])
    # Box plot
    p1 <- ggplot(se_tbl) +
      geom_boxplot(aes(x = Class, y = raw, fill = Class)) +
      scale_fill_manual(values = classes_color) +
      labs(title = feature_name, y = "Peak area") +
      theme(legend.position = "none") + # No legend for fill
      # Rotate x-axis labels
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      scale_y_log10()
    # Scatter plot across injection order
    p2 <- ggplot(se_tbl) +
      geom_point(aes(x = injection_order, y = raw, color = Class)) +
      scale_color_manual(values = classes_color) +
      labs(title = "", y = "", x = "Injection order") +
      theme(legend.position = "none") + # No legend for fill
      # No y-axis labels
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
      scale_y_log10()
    # Requires patchwork package
    p1 + p2 + patchwork::plot_layout(widths = widths)
  }


  output$ggplot3_1_3 <- renderPlot({
    se_show <- internal_std_se_r()
    selected <- shiny::req(input$tb_3_1_rows_selected)   # Nothing to draw before a row is selected
    ggplot3_1_3 <- box_scatter_plot_of_one_feature(
      se_show,
      selected,
      widths = c(0.3, 0.7),
      classes_color = colors_of_classes()
    )
    ggplot3_1_3
  })


  ################################################################################################################
  ### This is text 3.1.1

  ## the matrix internal_std_se_r() is changed after this with failed internal standards removed


  output$text_3_1_1 <- renderUI({
    req(pre_norm_se_r())
    
    all_rts <- util$retention_time(pre_norm_se_r())
    overall_rt_range <- range(all_rts)
    targetted_rt_range <- range(all_rts[util$is_targeted_feature(pre_norm_se_r())])
    rt <- range(all_rts[util$is_internal_std(pre_norm_se_r())])


    box::use(projlib / proc[count_zeros_per_feature])


    df <- SumExp::row_df(internal_std_se_r()) |>
      dplyr::mutate(
        num_zeros = count_zeros_per_feature(internal_std_se_r()[["raw"]]),
        rsd = show$compute_rsd_per_feature(internal_std_se_r()[["raw"]]),
      )
    # Table of internal standard features
    # kable_internal_std <- function(se) {
    df <- df |>
      dplyr::select(feature_name, .rt, mz, rsd, num_zeros) |>
      dplyr::mutate(
        num_zeros = ifelse(num_zeros == 0, "", num_zeros) |>
          labelled::copy_labels_from(num_zeros)
      )

    df <- data.frame(df,
      check.names = FALSE
    )

    is_failed_istd <- to_report_r()[["is failed internal std."]]


    df$is_failed_istd <- is_failed_istd # ensure it's inside render

    if (any(is_failed_istd)) {
      text_3_1_1 <- "The internal standard features  identified as failed IS were colored in the table above.\n\n"
      failed_istd <- internal_std_se_r()[is_failed_istd, ]
      # Table of failed internal standard features


      ################################################################# 3

      # Remove the failed internal standard features
    } else {
      text_3_1_1 <- "No failed internal standard features were identified.\n"
    }

    HTML(paste0("<h4 style='color:white;'>", text_3_1_1, "</h4>"))
  })


  ################################################################################################################
  ### something for 3.1.2 outlier removal


  output$wrap_outlier <- renderUI({
    is_outlier <- to_report_r()[["is outlier sample"]]
    # The following is added only when the outlier removal step has been set to perform

    if (!is.null(is_outlier)) {
      # Number of outlying internal standard features per sample
      n_out_istd <- to_report_r()[["number of outlier internal std. per sample"]]

      if (all(!is_outlier)) {
        text_3_1_2 <- paste0("No outliers were identified.\n")

        output$text_3_1_2 <- renderUI({
          HTML(paste0("<h3 style='color:white;'>", text_3_1_2, "</h3>"))
        })
      } else {
        text_3_1_2 <- paste0("The following samples were identified as outliers and excluded.\n\n")
        stopifnot(identical(names(is_outlier), names(n_out_istd)))
        # To retrieve the sample_name, Class, sample_type
        tb <- tibble::as_tibble(SumExp::col_df(internal_std_se_r()), rownames = "sample_id")
        show_tb <- tibble::as_tibble(n_out_istd, rownames = "sample_id") |>
          dplyr::mutate(is_outlier = is_outlier) |>
          labelled::set_variable_labels(
            "value" = "Number of Outlying IS",
            "sample_id" = "Sample ID"
          ) |>
          # Retrieve the sample_name, Class, sample_type
          dplyr::left_join(tb, by = "sample_id") |>
          dplyr::filter(is_outlier) |>
          dplyr::select(sample_name, Class, sample_type, value)

        show_tb <- as.data.frame(show_tb,
          check.names = FALSE
        )

        # Remove the outlier samples


        output$text_3_1_2 <- renderUI({
          HTML(paste0("<h4 style='color:white;'>", text_3_1_2, "</h4>"))
        })

        output$tb_3_1_2 <- DT::renderDataTable({
          DT::datatable(
            show_tb,
            options = list(
              scrollX = TRUE,
              lengthChange = FALSE,
              searching = FALSE,
              paging = FALSE, # remove Previous/Next
              info = FALSE, # remove "Showing X to Y of Z entries"
              dom = "t",
              headerCallback = JS(
                "function(thead){",
                "$(thead).find('th').css({'color': 'green',
                                            'font-weight': 'bold'});", "}"
              ),
              columnDefs = list(
                list(className = "dt-center", targets = "_all") # <--- centers all text
              )
            )
          ) |>
            DT::formatStyle(
              columns = names(show_tb),
              color = "#000000",
              backgroundColor = "#FFFFFF",
              target = "cell"
            ) |>
            DT::formatStyle(
              names(show_tb),
              backgroundColor = "#F8FAFF",
              target = "row"
            )
        })
      }
    }
  })


  output$remove_failedIS_outlier <- renderUI({
    is_outlier <- to_report_r()[["is outlier sample"]]
    is_failed_istd <- to_report_r()[["is failed internal std."]]

    if ((!is.null(is_failed_istd)) & (!is.null(is_outlier))) {
      internal_std_se_filtered <- internal_std_se_r()[!is_failed_istd, !is_outlier]
    } else if ((is.null(is_failed_istd)) & (!is.null(is_outlier))) {
      internal_std_se_filtered <- internal_std_se_r()[, !is_outlier]
    } else if ((!is.null(is_failed_istd)) & (is.null(is_outlier))) {
      internal_std_se_filtered <- internal_std_se_r()[!is_failed_istd, ]
    }

    NULL
  })

  ################################################################################################################
  ### This is for table 3.1.4.1 and 3.4.1.2


  output$tb_3_4_1 <- DT::renderDataTable({
    shiny::req(internal_std_se_r())
    # RSD% per QC class; no columns when there are no QC samples
    qc_lst <- show$extract_qc_samples_to_list(internal_std_se_r())
    m_rsd1 <- if (length(qc_lst) > 0) {
      m <- qc_lst |>
        lapply(function(se) {
          apply(se[["raw"]], 1, \(.x) round(show$rsd_perc(.x), 2))
        }) |>
        do.call(cbind, args = _) # Make sure the result is a matrix even if there is only one value
      # The `rownames` are syntactically valid names, not the original feature names
      stopifnot(identical(rownames(m), rownames(internal_std_se_r())))
      data.frame(m, check.names = FALSE)
    } else {
      data.frame(row.names = rownames(internal_std_se_r()))
    }
    rownames(m_rsd1) <- SumExp::row_df(internal_std_se_r())$feature_name


    se <- util$exclude_ctrl_smpl_cat(internal_std_se_r(), "QC")
    m_rsd2 <- setNames(nm = unique(SumExp::col_df(se)$Class)) |>
      lapply(function(cls) {
        se <- se[, SumExp::col_df(se)$Class == cls]
        apply(se[["raw"]], 1, \(.x) round(show$rsd_perc(.x), 2))
      }) |>
      do.call(cbind, args = _) # Make sure the result is a matrix even if there is only a vector
    stopifnot(identical(rownames(m_rsd2), rownames(se)))
    rownames(m_rsd2) <- SumExp::row_df(se)$feature_name

    m_rsd2 <- data.frame(m_rsd2,
      check.names = FALSE
    )

    m_rsd <- cbind.data.frame(m_rsd1, m_rsd2)

    m_rsd <- t(m_rsd)

    shiny::req(input$tb_3_1_rows_selected)
    selected <- input$tb_3_1_rows_selected

    m_rsd <- m_rsd[, selected, drop = FALSE]

    DT::datatable(
      m_rsd,
      options = list(
        scrollX = TRUE,
        lengthChange = FALSE,
        searching = FALSE,
        paging = FALSE, # remove Previous/Next
        info = FALSE, # remove "Showing X to Y of Z entries"
        dom = "t",
        headerCallback = JS(
          "function(thead){",
          "$(thead).find('th').css({'color': 'green',
                                            'font-weight': 'bold'});", "}"
        ),
        columnDefs = list(
          list(className = "dt-center", targets = "_all") # <--- centers all text
        )
      )
    ) |>
      DT::formatStyle(
        columns = names(m_rsd),
        color = "#000000",
        backgroundColor = "#FFFFFF",
        target = "cell"
      ) |>
      DT::formatStyle(
        names(m_rsd),
        backgroundColor = "#F8FAFF",
        target = "row"
      )
  })


  ################################################################################################################
  ### This is for Figure in 3.2.1.1


  output$vis_or_text_3_2_1_1 <- renderUI({
    norm_se <- to_report_r()[["normalized"]]
    is_vIS <- util$std_type(norm_se) == "vIS"
    mat_ids_to_compare <- c("raw")

    if (any(is_vIS)) {
      df_show <- norm_se[is_vIS, ] |>
        show$df_for_injection_order(mat_ids_to_compare)
      df_show <- as.data.frame(df_show,
        check.names = FALSE
      )
      ggplot3_2_1_1 <- plotly::ggplotly(
        show$ggplot_col_injection_order(df_show,
          fill = feature_name
        ) +
          labs(title = "Volumetric internal standards") +
          # Skip the legend. Too long feature names make the plots too narrow.
          theme(legend.position = "none")
      )

      output$ggplot3_2_1_1 <- renderPlotly({
        ggplot3_2_1_1
      })
      plotlyOutput("ggplot3_2_1_1")
    } else {
      HTML("<h4 style='color:orange;'>No volumetric internal standards found.</h4>")
    }
  })


  output$tb_test <- DT::renderDataTable({
    norm_se <- to_report_r()[["normalized"]]
    df_show <- norm_se[util$is_internal_std(norm_se), ] |>
      show$df_for_injection_order(mat_ids_to_compare) |>
      dplyr::mutate(
        batch_id = factor(batch_id) |> # Discrete color scale
          labelled::copy_labels_from(batch_id)
      )

    df_show <- as.data.frame(df_show,
      check.names = FALSE
    )
    DT::datatable(
      df_show,
      options = list(
        scrollX = TRUE,
        lengthChange = FALSE,
        searching = FALSE,
        paging = FALSE, # remove Previous/Next
        info = FALSE, # remove "Showing X to Y of Z entries"
        dom = "t",
        headerCallback = JS(
          "function(thead){",
          "$(thead).find('th').css({'color': 'green',
                                            'font-weight': 'bold'});", "}"
        ),
        columnDefs = list(
          list(className = "dt-center", targets = "_all") # <--- centers all text
        )
      )
    ) |>
      DT::formatStyle(
        columns = names(df_show),
        color = "#000000",
        backgroundColor = "#FFFFFF",
        target = "cell"
      ) |>
      DT::formatStyle(
        names(df_show),
        backgroundColor = "#F8FAFF",
        target = "row"
      )
  })


  ################################################################################################################
  ### This is for Figure in 3.2.1.2
  output$ggplot3_2_1_2 <- renderPlotly({
    norm_se <- to_report_r()[["normalized"]]
    is_vIS <- util$std_type(norm_se) == "vIS"
    mat_ids_to_compare <- c("raw", "loess_norm")

    df_show <- norm_se[util$is_internal_std(norm_se), ] |>
      show$df_for_injection_order(mat_ids_to_compare) |>
      dplyr::mutate(
        batch_id = factor(batch_id) |> # Discrete color scale
          labelled::copy_labels_from(batch_id)
      )

    df_show <- as.data.frame(df_show,
      check.names = FALSE
    )
    df_show$Class <- factor(df_show$Class)
    df_show$batch_id <- factor(df_show$batch_id)
    df_show$feature_name <- factor(df_show$feature_name)
    shiny::req(input$group_choice)
    if (input$group_choice == "class") {
      plotly::ggplotly(
        show$ggplot_col_injection_order(df_show, fill = Class) +
          labs(title = "Internal standards")
      )
    } else if (input$group_choice == "batch") {
      plotly::ggplotly(
        show$ggplot_col_injection_order(df_show, fill = batch_id) +
          labs(title = "Internal standards")
      )
    } else if (input$group_choice == "internal_std") {
      plotly::ggplotly(
        show$ggplot_col_injection_order(df_show, fill = feature_name) +
          labs(title = "Internal standards") +
          # Skip the legend. Too long feature names make the plots too narrow.
          theme(legend.position = "none")
      )
    }
  })


  ################################################################################################################
  ### This is for figure 3.2.3


  # One line per targeted chemical and QC class (panels): its RSD% among the QC replicates under
  # each method (x-axis), colored by the normalization method with the lowest RSD%. Interactive:
  # hover shows the chemical and all its RSD% values, the legend filters, drag to zoom.
  output$ggplot3_2_3 <- plotly::renderPlotly({
    norm_se <- to_report_r()[["normalized"]]
    shiny::validate(shiny::need(show$has_enough_qc(norm_se), show$not_enough_qc_msg))
    labs <- SumExp::name_labs(norm_se)
    names(labs) <- gsub(" .*", "", names(labs)) # First word only
    df_to_show <- show$extract_quant_qc(norm_se) |>
      show$calc_rsd_qstd(labs) |>
      show$mark_lowest_rsd_norm(labs) |>
      dplyr::left_join(feature_id_name_tbl(), by = "feature_id")

    # Hover text: the chemical with its RSD% under every method
    rsd_txt <- apply(as.matrix(df_to_show[unname(labs)]), 1, function(v) {
      paste0(names(labs), ": ", ifelse(is.na(v), "NA (all 0s)", paste0(round(v, 1), "%")),
             collapse = "<br>")
    })
    df_long <- df_to_show |>
      dplyr::mutate(
        txt = paste0("<b>", feature_name, "</b> (", QC, ")<br>", rsd_txt,
                     "<br>Lowest: ", sub("^\\S+ ", "", as.character(sign)))
      ) |>
      tidyr::pivot_longer(dplyr::all_of(unname(labs)), names_to = "Data", values_to = "RSD") |>
      dplyr::mutate(
        RSD = ifelse(is.na(RSD), 0, RSD),   # NA when all values are 0
        Data = factor(Data, levels = unname(labs), labels = names(labs))
      )

    p <- ggplot(df_long, aes(x = Data, y = RSD, group = feature_id, color = sign, text = txt)) +
      geom_line(alpha = 0.5) +
      geom_point(size = 1, alpha = 0.6) +
      scale_color_manual(values = show$norm_sign_colors, drop = FALSE) +
      facet_wrap(~QC) +
      labs(x = NULL, y = "RSD%", color = "Lowest RSD%")
    plotly::ggplotly(p, tooltip = "text") |>
      plotly::layout(legend = list(title = list(text = "Lowest RSD%<br>(click to filter)")))
  })


  ################################################################################################################
  ### This is for table 3.2.3


  output$tb_3_2_3 <- DT::renderDataTable({
    norm_se <- to_report_r()[["normalized"]]
    shiny::validate(shiny::need(show$has_enough_qc(norm_se), show$not_enough_qc_msg))
    labs <- SumExp::name_labs(norm_se)
    names(labs) <- gsub(" .*", "", names(labs)) # First word only
    df_to_show <- show$extract_quant_qc(norm_se) |>
      show$calc_rsd_qstd(labs)

    df_to_show <- df_to_show |>
      dplyr::left_join(feature_id_name_tbl(), by = "feature_id") |> # Add feature names
      dplyr::select(QC, feature_name, dplyr::all_of(unname(labs))) |>
      dplyr::rename(all_of(labs), `Feature` = feature_name)

    df_to_show <- data.frame(df_to_show,
      check.names = FALSE
    )
    DT::datatable(
      df_to_show,
      options = list(
        scrollX = TRUE,
        lengthChange = FALSE,
        searching = FALSE,
        paging = FALSE, # remove Previous/Next
        info = FALSE, # remove "Showing X to Y of Z entries"
        dom = "t",
        headerCallback = JS(
          "function(thead){",
          "$(thead).find('th').css({'color': 'green',
                                            'font-weight': 'bold'});", "}"
        ),
        columnDefs = list(
          list(className = "dt-center", targets = "_all") # <--- centers all text
        )
      )
    ) |>
      # ✅ Round numeric columns to 2 decimals
      DT::formatRound(
        columns = which(sapply(df_to_show, is.numeric)),
        digits = 2
      ) |>
      DT::formatStyle(
        columns = names(df_to_show),
        color = "#000000",
        backgroundColor = "#FFFFFF",
        target = "cell"
      ) |>
      DT::formatStyle(
        names(df_to_show),
        backgroundColor = "#F8FAFF",
        target = "row"
      )
  })

  ################################################################################################################
  ### This is for table 4.1

  # ---- Calibration and Quantification: batch selection ----
  # Calibrated data of the selected normalization method: batch ID -> list(calcurve, concn)
  calib_lst_proc_r <- shiny::reactive({
    shiny::req(is_target_mode())
    MAT_ID_BLANK_SUBT <- util$mat_id_of_blank_subtracted(shiny::req(input$norm_method))
    shiny::req(proc_r()[[MAT_ID_BLANK_SUBT]])
  })

  # Batch choices follow the processed data; keep the current batch if it still exists
  shiny::observe({
    batch_ids <- names(calib_lst_proc_r())
    current <- shiny::isolate(input$calib_batch)
    selected <- if (!is.null(current) && current %in% batch_ids) current else batch_ids[1]
    shiny::updateSelectInput(session, "calib_batch", choices = batch_ids, selected = selected)
  })

  # Calibrated data of the selected batch: list(calcurve, concn)
  calib_batch_proc_r <- shiny::reactive({
    lst_proc <- calib_lst_proc_r()
    shiny::req(input$calib_batch %in% names(lst_proc))
    lst_proc[[input$calib_batch]]
  })

  # Feature ID of the chemical selected in table 4.2. Kept when the batch or the normalization
  # method changes, so that the same chemical is selected again in the re-rendered table.
  calib_feature_id <- shiny::reactiveVal(NULL)
  shiny::observeEvent(input$tb_4_2_rows_selected, {
    ids <- rownames(shiny::isolate(calib_batch_proc_r())$calcurve)
    calib_feature_id(ids[input$tb_4_2_rows_selected[1]])
  })
  # Selected feature ID that exists in the selected batch (stops the figures until one is selected)
  calib_selected_feature_r <- shiny::reactive({
    shiny::req(input$tb_4_2_rows_selected)
    id <- rownames(calib_batch_proc_r()$calcurve)[input$tb_4_2_rows_selected[1]]
    shiny::req(!is.na(id))
    id
  })

  # ---- Calibration points shown in the figures ----
  # Calibration curve used for the selected chemical in the selected batch: "Batch", "Global" or
  # NA (no proper calibration range)
  calib_used_curve_r <- shiny::reactive({
    cc <- calib_batch_proc_r()$calcurve
    src <- SumExp::row_df(cc)[["src_calcurve"]]
    if (is.null(src)) return(NA_character_)   # Processed by an older version
    src[rownames(cc) == calib_selected_feature_r()]
  })
  # "All batches" is offered only for a chemical quantified with the global curve
  calib_global_used_r <- shiny::reactive({
    isTRUE(calib_used_curve_r() == "Global") && length(calib_lst_proc_r()) > 1
  })
  # The user's choice of points ("all" / "batch"). It is kept when only the batch or the
  # normalization method changes, and reset to the curve used ("all") for another chemical.
  calib_view_pref <- shiny::reactiveVal("all")
  calib_last_feature <- shiny::reactiveVal(NULL)
  shiny::observeEvent(input$calib_points_view, {
    # Only a choice between both options is the user's; "batch" alone is forced by the data
    if (shiny::isolate(calib_global_used_r())) calib_view_pref(input$calib_points_view)
  })
  shiny::observe({
    feature_id <- calib_selected_feature_r()
    if (!identical(feature_id, shiny::isolate(calib_last_feature()))) {
      calib_last_feature(feature_id)
      calib_view_pref("all")   # Default: the points of the curve used for quantification
    }
    choices <- c("All batches (global curve)" = "all", "Selected batch only" = "batch")
    if (calib_global_used_r()) {
      shiny::updateSelectInput(session, "calib_points_view", choices = choices,
                               selected = shiny::isolate(calib_view_pref()))
    } else {
      shiny::updateSelectInput(session, "calib_points_view", choices = choices["Selected batch only"],
                               selected = "batch")
    }
  })
  calib_points_view_r <- shiny::reactive({
    if (calib_global_used_r() && identical(input$calib_points_view, "all")) "all" else "batch"
  })

  # Calibration curve samples of the selected chemical (one row) to show in the figures. For
  # "all", the samples of all batches, i.e. those the global curve was fitted on, with the global
  # limits of the selected batch's data (identical for every batch using the global curve).
  calib_cc_se_r <- shiny::reactive({
    feature_id <- calib_selected_feature_r()
    cc_sel <- calib_batch_proc_r()$calcurve
    cc_sel <- cc_sel[rownames(cc_sel) == feature_id, ]
    if (calib_points_view_r() == "batch") return(cc_sel)

    # Combine the samples of all batches. A batch without its own calibration curve samples holds
    # all the global ones, so drop the samples already collected.
    seen <- character(0)
    se_lst <- list()
    for (b in names(calib_lst_proc_r())) {
      se <- calib_lst_proc_r()[[b]]$calcurve
      se <- se[rownames(se) == feature_id, !colnames(se) %in% seen]
      if (nrow(se) == 0 || ncol(se) == 0) next
      seen <- c(seen, colnames(se))
      SumExp::row_df(se) <- SumExp::row_df(cc_sel)   # Global limits, required to bind columns
      se_lst[[b]] <- se
    }
    se <- Reduce(cbind, se_lst)
    # Mask the calibration matrix outside the global range, as done in processing
    mat_src <- util$mat_id_of_blank_subtracted(input$norm_method)
    mat_calib <- util$mat_id_in_calibration(mat_src)
    rd <- SumExp::row_df(se)
    conc <- util$spiked_conc_pts(se)
    to_na <- !(conc >= rd$min_c_conc & conc <= rd$max_c_conc)
    to_na[is.na(to_na)] <- TRUE
    m <- se[[mat_src]]
    m[, to_na] <- NA_real_
    se[[mat_calib]] <- labelled::copy_labels(cc_sel[[mat_calib]], m)
    se
  })

  # Explanation next to the dropdown; red when the shown points are not those of the curve used
  output$calib_points_note <- shiny::renderUI({
    batch <- input$calib_batch
    used <- calib_used_curve_r()
    txt <- if (is.na(used)) {
      list("No calibration curve with a proper range for this chemical in batch ", batch, ".")
    } else if (used == "Batch") {
      list("Concentrations in batch ", batch, " were calculated with this batch's own calibration curve.")
    } else if (!calib_global_used_r()) {
      list("Concentrations were calculated with the calibration curve of all samples.")
    } else if (calib_points_view_r() == "all") {
      list("Concentrations in batch ", batch, " were calculated with the global calibration curve, ",
           "fitted on the calibration points of all batches shown here.")
    } else {
      return(shiny::tags$div(
        style = "color: #C62828; font-weight: 600;",
        paste0("Only batch ", batch, "'s calibration points are shown. The concentrations were ",
               "calculated with the global calibration curve, fitted on the calibration points of ",
               "all batches.")
      ))
    }
    shiny::tags$div(style = "color: #333;", do.call(paste0, txt))
  })

  output$tb_4_1 <- DT::renderDataTable({
    ###################################################
    ### There are two issues.
    ## (1) One is the reactivity of params_yml$norm_method
    ## (2) One is reactivity to the data

    if (is_target_mode()) {
      # Number of calibration curve samples per concentration in the selected batch
      se <- calib_batch_proc_r()$calcurve
      v <- util$spiked_conc_pts(se)
      v <- as.data.frame(table(v[!is.na(v)]), check.names = FALSE)
      colnames(v) <- c("Calibration curve samples", "Number of Samples")
      DT::datatable(
        v,
        options = list(
          scrollX = TRUE,
          lengthChange = FALSE,
          searching = FALSE,
          paging = FALSE, # remove Previous/Next
          info = FALSE, # remove "Showing X to Y of Z entries"
          dom = "t",
          headerCallback = JS(
            "function(thead){",
            "$(thead).find('th').css({'color': 'green',
                                              'font-weight': 'bold'});", "}"
          ),
          columnDefs = list(
            list(className = "dt-center", targets = "_all") # <--- centers all text
          )
        )
      ) |>
        DT::formatStyle(
          columns = names(v),
          color = "#000000",
          backgroundColor = "#FFFFFF",
          target = "cell"
        ) |>
        DT::formatStyle(
          names(v),
          backgroundColor = "#F8FAFF",
          target = "row"
        )
    }
  })


  ################################################################################################################
  ### This is for table 4.2
  output$tb_4_2 <- DT::renderDataTable({

    min_cal_points <- to_report_r()[["params"]][["min_cal_points"]]

    if (is_target_mode()) {
      # --------------------------------------------------------
      # Prepare data of the selected batch. Rows are in the order of `rownames(calcurve)`, which
      # the figures use to find the selected chemical.
      se <- calib_batch_proc_r()$calcurve
      limit_df <- SumExp::row_df(se) |>
        dplyr::select(
          min_c_conc,
          max_c_conc,
          lod,
          lloq,
          has_proper_range,
          src_calcurve
        ) |>
        tibble::as_tibble(rownames = "feature_id") |>
        dplyr::mutate(batch = shiny::isolate(input$calib_batch))
      # Select the same chemical as before the batch / normalization method changed
      prev_id <- shiny::isolate(calib_feature_id())   # NULL until a chemical has been selected
      preselect <- if (is.null(prev_id)) NA_integer_ else match(prev_id, limit_df$feature_id)
      preselect <- if (is.na(preselect)) NULL else preselect

      # --------------------------------------------------------
      show_df <- limit_df |>
        dplyr::rename(min_c = min_c_conc, max_c = max_c_conc) |>
        dplyr::mutate(
          lod = ifelse(is.na(lod), "N.A.", as.character(lod)),
          min_c = ifelse(is.na(min_c), "No valid pts.", as.character(min_c)),

          # ✅ flags for coloring
          is_bad = !has_proper_range,
          is_global = src_calcurve == "Global",
          # Calibration curve used for quantification: "Batch", "Global" or "None"
          calibration = dplyr::coalesce(src_calcurve, "None"),
          Note = dplyr::case_when(
            max_c == 0 ~ "All zero",
            is.na(max_c) ~ paste0("<", min_cal_points ," valid pts."),
            !has_proper_range ~ "Excluded",
            TRUE ~ ""
          ),
          max_c = dplyr::case_when(
            max_c == -9 ~ "No valid max",
            max_c == 0 | is.na(max_c) ~ "",
            TRUE ~ as.character(max_c)
          )
        ) |>
        dplyr::left_join(feature_id_name_tbl(), by = "feature_id") |>
        dplyr::select(
          "Batch" = batch,
          "Chemical Name" = feature_name,
          "Calibration" = calibration,
          "LOD" = lod,
          "Minimum conc. (LLOQ)" = min_c,
          "Maximum conc." = max_c,
          is_global,
          is_bad,
          Note
        )

      show_df <- as.data.frame(show_df, check.names = FALSE)

      # --------------------------------------------------------
      DT::datatable(
        show_df,
        selection = list(mode = "single", selected = preselect),
        options = list(
          scrollX = TRUE,
          paging = FALSE,
          searching = FALSE,
          info = FALSE,
          dom = "t",

          # ✅ hide helper columns
          columnDefs = list(
            list(
              targets = c(ncol(show_df) - 2, ncol(show_df) - 1),
              visible = FALSE
            )
          ),

          # ✅ ✅ ROW COLORING (JS)
          rowCallback = JS("
          function(row, data) {

            var is_bad = data[data.length - 2];
            var is_global = data[data.length - 1];

            // 🔴 priority 1: failed calibration
            if (is_bad) {
              $('td', row).each(function() {
                this.style.setProperty('background-color', 'lightcoral', 'important');
                this.style.setProperty('color', 'black', 'important');
              });
            }

            // 🔵 priority 2: global calibration
            else if (is_global) {
              $('td', row).each(function() {
                this.style.setProperty('background-color', 'lightblue', 'important');
                this.style.setProperty('color', 'black', 'important');
              });
            }

          }
        ")
        )
      )
    }
  })


  ################################################################################################################
  ### This is for Figrue 4.3
  output$ggplot4_3 <- renderPlotly({

    if (is_target_mode()) {
      ############################################
      # Parameters
      log_scale <- to_report_r()[["params"]][["log_calibration"]]

      ############################################
      # Calibration curve samples of the selected chemical (selected batch or all batches);
      # the batch of each point is that of the sample
      # Evaluate the reactives before passing them to S4 methods like `as_tibble`: a `req()`
      # inside an S4 argument becomes an error message instead of silently waiting
      cc_se <- calib_cc_se_r()
      calcurve_lst <- list(calib_batch_proc_r()$calcurve)   # Source of the labels below
      calcurve_to_show_df <- SumExp::as_tibble(cc_se) |>
        dplyr::mutate(Batch = as.character(batch_id))

      # Copy labels
      for (ii in names(calcurve_lst[[1]])) {
        labelled::label_attribute(calcurve_to_show_df[[ii]]) <-
          labelled::label_attribute(calcurve_lst[[1]][[ii]])
      }

      for (ii in names(SumExp::col_df(calcurve_lst[[1]]))) {
        l <- labelled::label_attribute(SumExp::col_df(calcurve_lst[[1]])[[ii]])
        labelled::label_attribute(calcurve_to_show_df[[ii]]) <- l
      }

      calcurve_to_show_df <- calcurve_to_show_df |>
        dplyr::rename(.cc_pt = util$spiked_conc_pts_name) |>
        dplyr::mutate(
          c_type = dplyr::case_when(
            .cc_pt >= min_c_conc & .cc_pt <= max_c_conc ~ "Within limits",
            .cc_pt == lod ~ "LOD",
            .cc_pt == 0 ~ "Cal0",
            TRUE ~ "Out-of-range"
          )
        )

      ############################################
      # Data of the chemical selected in table 4.2
      dfm <- calcurve_to_show_df[calcurve_to_show_df$.row_id == calib_selected_feature_r(), ]
      shiny::req(nrow(dfm) > 0)

      ############################################
      # Plot function
      calcurve_plot <- function(dfm, y = input$norm_method) {
        dfm <- dfm |>
          dplyr::mutate(
            Batch = factor(Batch),
            txt = paste0(
              "Batch: ", Batch,
              "\ninj: ", injection_order,
              ", conc: ", .cc_pt,
              "\nName: ", sample_name,
              "\nint: ", signif(.data[[y]], 6)
            )
          )

        facet_fill <- if (any(!dfm$has_proper_range)) "lightcoral" else "lightgray"

        ggplot(dfm, aes(x = .cc_pt, y = .data[[y]])) +

          # ✅ ONLY ONE LAYER
          geom_point(
            aes(
              color = c_type,
              shape = Batch,
              text = txt,
              group = interaction(c_type, Batch) # ✅ prevent merging
            ),
            size = 2
          ) +
          labs(
            x = show$label_if_has(dfm$.cc_pt, "Calibrant Concentration"),
            y = show$label_if_has(dfm[[y]], "Signal"),
            color = "(Concentration category,",
            shape = "Batch)"
          ) +
          scale_color_manual(values = c(
            "Within limits" = "green4",
            "Out-of-range" = "gray",
            "Cal0" = "red",
            "LOD" = "blue"
          )) +
          scale_shape_manual(
            values = rep_len(c(16, 17, 15, 18, 8, 3, 4, 7, 9, 10, 11, 12, 13, 14), nlevels(dfm$Batch))
          ) +

          # ✅ THIS IS THE KEY FIX
          guides(
            color = guide_legend(
              order = 1,
              override.aes = list(shape = 16) # ⬅ remove shape influence
            ),
            shape = guide_legend(
              order = 2,
              override.aes = list(color = "black") # ⬅ remove color influence
            )
          ) +
          facet_wrap(~feature_name, scales = "free", ncol = 1) +
          theme(
            strip.background = element_rect(fill = facet_fill),
            legend.position = "right"
          )
      }


      ############################################
      # ✅ Build ONE plot only
      p_main <- calcurve_plot(dfm)

      if (log_scale) {
        p_main <- p_main + scale_x_log10() + scale_y_log10()
      }

      ############################################
      # ✅ Convert to plotly (with legend)
      plotly::ggplotly(p_main, tooltip = "text") |>
        plotly::layout(
          legend = list(
            orientation = "v",
            x = 1.02,
            y = 1,
            xanchor = "left",
            tracegroupgap = 10 # ✅ separate color & shape visually
          ),
          margin = list(
            t = 50,
            b = 100,
            l = 50,
            r = 150
          )
        )
    }
  })

  ################################################################################################################
  ### This is for Figrue 4.4

  output$ggplot4_4_2 <- renderPlotly({

    MAT_ID_BLANK_SUBT <- input$norm_method |>
      util$mat_id_of_blank_subtracted()
    MAT_ID_IN_CALIB <- util$mat_id_in_calibration(MAT_ID_BLANK_SUBT)

    if (is_target_mode()) {
      ############################################
      # Prepare data of the selected batch and chemical
      log_scale <- to_report_r()[["params"]][["log_calibration"]]

      concn_se <- calib_batch_proc_r()$concn
      calib_se <- calib_cc_se_r()   # Calibration points of the selected batch or all batches
      feature_id <- calib_selected_feature_r()
      shiny::req(feature_id %in% rownames(concn_se))
      se_selected <- concn_se[rownames(concn_se) == feature_id, ]

      if (!isTRUE(SumExp::row_df(se_selected)$has_proper_range)) {


        ############################################
        # ✅ Output

        plotly::plot_ly() |>
          plotly::layout(
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE),
            annotations = list(
              list(
                text = "Not detected / quantified in samples",
                x = 0.5,
                y = 0.5,
                xref = "paper",
                yref = "paper",
                showarrow = FALSE,
                font = list(size = 24, color = "gray")
              )
            )
          )
      } else {
        p <- show$ggplot_calcurve_samples_facet(
          se_selected,
          calib_se,
          MAT_ID_IN_CALIB,
          colors_of_classes(),
          ncol = 1,
          log_scale = log_scale
        )
        ############################################
        # ✅ Convert to plotly
        plotly::ggplotly(p, tooltip = "text") |>
          plotly::layout(
            margin = list(t = 50, b = 100, l = 50, r = 50)
          )
      }
    }
  })

  ################################################################################################################
  ### This is for Figrue 5.1

  output$ggplot5_1 <- renderPlot({
    shiny::req(to_report_r())

    if (shiny::req(is_target_mode())) {
      log_scale <- to_report_r()[["params"]][["log_calibration"]]

      # ------------------------------------------------------------
      # Prepare data of the selected batch and chemical (by feature ID, not by name)
      feature_id <- calib_selected_feature_r()
      concn_se <- calib_batch_proc_r()$concn   # Evaluated before the S4 method (see 4.3)
      show_df <- SumExp::as_tibble(concn_se)
      df1 <- show_df[show_df$.row_id == feature_id, ]

      # ------------------------------------------------------------
      # ✅ ✅ DATA VALIDATION (IMPORTANT)
      if (
        is.null(df1) ||
          nrow(df1) == 0
      ) {
        # ✅ Show message plot instead of empty plot
        ggplot() +
          annotate(
            "text",
            x = 0.5,
            y = 0.5,
            label = "Not detected / quantified in samples",
            size = 10,
            color = "gray"
          ) +
          theme_void()
      } else {
        # ------------------------------------------------------------
        # Normal plot
        p <- ggplot(df1) +
          geom_boxplot(
            aes(x = Class, y = conc, fill = Class, color = Class),
            outlier.shape = NA
          ) +
          geom_point(
            aes(x = Class, y = conc, fill = Class, color = Class),
            position = position_jitter(width = 0.2),
            size = 1.8,
            alpha = 0.7
          ) +
          labs(
            title = df1$feature_name[1],
            x = labelled::get_label_attribute(df1$Class),
            y = labelled::get_label_attribute(df1$conc)
          ) +
          theme(
            axis.text.x = element_text(angle = 90, hjust = 1),
            legend.position = "none"
          ) +
          scale_fill_manual(values = colors_of_classes()) +
          scale_color_manual(values = colors_of_classes())

        # ✅ Safe log scale (avoid disappearing plots)
        if (log_scale) {
          p <- p + scale_y_continuous(
            trans = scales::pseudo_log_trans(base = 10)
          )
        }

        p
      }
    }
  })
  ################################################################################################################
  ### This is for text 4.5


  output$tb_4_5 <- DT::renderDataTable({
    MAT_ID_BLANK_SUBT <- input$norm_method |>
      util$mat_id_of_blank_subtracted()
    MAT_ID_IN_CALIB <- util$mat_id_in_calibration(MAT_ID_BLANK_SUBT)

    if (shiny::req(is_target_mode())) {
      # Load the processed data using the specified normalization method
      lst_proc <- shiny::req(proc_r())[[MAT_ID_BLANK_SUBT]]

      concn_lst <- lapply(lst_proc, \(ea) {
        se <- ea$concn
        se[SumExp::row_df(se)$has_proper_range, ]
      })


      df_nchem <- lapply(concn_lst, function(se) {
        tibble::tibble(
          "Number of chemicals" =
            apply(se[["conc"]], 1, function(r) any(!is.na(r))) |>
            sum()
        )
      }) |>
        purrr::list_rbind(names_to = "Batch")


      DT::datatable(
        df_nchem,
        options = list(
          scrollX = TRUE,
          lengthChange = FALSE,
          searching = FALSE,
          paging = FALSE, # remove Previous/Next
          info = FALSE, # remove "Showing X to Y of Z entries"
          dom = "t",
          headerCallback = JS(
            "function(thead){",
            "$(thead).find('th').css({'color': 'green',
                                              'font-weight': 'bold'});", "}"
          ),
          columnDefs = list(
            list(className = "dt-center", targets = "_all") # <--- centers all text
          )
        )
      ) |>
        DT::formatStyle(
          columns = names(df_nchem),
          color = "#000000",
          backgroundColor = "#FFFFFF",
          target = "cell"
        ) |>
        DT::formatStyle(
          names(df_nchem),
          backgroundColor = "#F8FAFF",
          target = "row"
        )
    }
  })

}


# Launch
shiny::shinyApp(ui = ui, server = server)
