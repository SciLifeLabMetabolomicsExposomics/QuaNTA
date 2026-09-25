## ------------------------------------------------------------------------------------------- #
# This is a Shiny web application.
# ------------------------------------------------------------------------------------------- #

options(readr.show_progress = FALSE)

options(shiny.host = "0.0.0.0")
options(shiny.port = 7579)
options(shiny.maxRequestSize = 500 * 1024^2)


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
# 1) Define theme
custom_theme <- bs_theme(
  version = 5,
  bootswatch = NULL,
  base_font = font_google("Inter"),
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
  "card-bg" = "#6FAAF7", # QuANTA deep blue
  "card-border-color" = "#6FAAF7", # darker border to match
  "navbar-bg" = "#0D121C",
  "navbar-fg" = "#FFFFFF",
  "link-color" = "#F4A259",
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
  font-size: 2rem !important;
  font-weight: 700 !important;
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
quanta_card <- function(title, ...) {
  card(card_header(div(title, style = "margin-bottom: 10px;"),
                   div(style = "height:3px; background:white; width:100%;")), ...)
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
      "))
    ),
    nav_panel(
      title = HTML("1. Input data and Processing  <strong> >> </strong>"),
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
      title = HTML("2. Data overview  <strong> >> </strong>"),
      layout_columns(
        col_widths = c(6, 6),
        #Card
        quanta_card("Tables",
                    section("Samples by category/class (2.1.1)",
                            spinner(DT::DTOutput("tb_2_1_1"))),
                    section("The distribution of the features across m/z and retention time (2.2)",
                            spinner(DT::DTOutput("tb_2_1_2_2"))),
                    section("Calibration curve samples - quality control samples (2.1.3)",
                            spinner(htmlOutput("tb_2_1_3"))),
                    section("Features (2.2)",
                            spinner(htmlOutput("tb_2_2")))),
        #Card
        quanta_card("Figures",
                    section("The distribution of the features across m/z and retention time (2.2)",
                            spinner(plotOutput("ggplot1_2_2"))),
                    section("The distribution of the features (2.2)",
                            spinner(plotOutput("ggplot2_2_2"))),
                    section("Distribution of the values (2.3)",
                            spinner(plotOutput("ggplot2_3")))),
        
      )
    ),
    nav_panel(
      title = HTML("3. Quality control and Normalization <strong> >> </strong>"),
      layout_columns(
        col_widths = c(6, 6),
        #Card
        quanta_card("Tables",
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
        quanta_card("Figures",
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
                      spinner(plotlyOutput("ggplot3_2_1_2")),
                      h3(
                        "Comparison of normalization methods 3.2.3",
                        style = "margin: 0;"
                      ),
                      spinner(plotOutput("ggplot3_2_3")),
                      h3(
                        "RSD% values (3.2.3)",
                        style = "margin: 0;"
                      ),
                      spinner(DT::DTOutput("tb_3_2_3")),
                    )
                  )
      )
    ),
    nav_panel(
      title = HTML("4. Calibration and Quantification <strong> >> </strong>"),
      layout_columns(
        col_widths = c(6, 6),
        quanta_card("Tables",
                    shiny::radioButtons(
                      inputId = "norm_method",
                      label = "Normalization method",
                      choices = c(
                        "LOESS" = "loess_norm",
                        "Closest RT" = "closest_norm",
                        "Without normalization" = "raw"
                      ),
                      selected = "loess_norm",
                      inline = TRUE
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
      title = HTML("5. Report export&nbsp;📄&nbsp <strong>  </strong>"),
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
                    # shiny::radioButtons(
                    #   inputId = "norm_method_2",
                    #   label = "Normalization method",
                    #   choices = c(
                    #     "LOESS" = "loess_norm",
                    #     "Closest RT" = "closest_norm",
                    #     "Without normalization" = "raw"
                    #   ),
                    #   selected = "loess_norm"
                    # ),
                    # shiny::checkboxInput(
                    #   inputId = "blk_substraction",
                    #   label = "The output is blank substracted",
                    #   value = FALSE,
                    # ),
                    # shiny::checkboxInput(
                    #   inputId = "blk_filtering2",
                    #   label = "The output is blank filtered",
                    #   value = FALSE,
                    #   width = "100%"
                    # ),
                    # numericInput(
                    #   inputId = "blank_filtering_factor2",
                    #   label = "Blank filtering factor (> 1):",
                    #   value = 1,
                    #   min = 1,
                    #   width = "100%"
                    # ),
                    
                    
                    ##  the selected thinsg needs to be injected in to the runScriptUI() in the runScript.R
                    ## actually it needs to be injected in to the export_data.R
                    
                    
                    runScriptUI("export_data", label = "Export data into tables", download=T)
                  )
      )
    )
  )

####################################################################################################################
####################################################################################################################
################################################################################################
###################################### SERVER ###################################################
################################################################################################

server <- function(input, output, session) {
  
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
  
  # ---- 3. Child processes; each returns a reactive that changes when a run has finished ----
  read_done <- runReadMsdialServer("read_msdial", data_info = data_info, ws = ws)
  proc_done <- runProcessServer("proc", data_info = data_info, ws = ws)
  
  runScriptServer2(
    "export_data",
    script_path = "scripts/export_data.R",
    what = "Export data into tables",
    data_info = data_info,
    ws = ws,
    input1 = shiny::reactive(input$norm_method_2),
    input2 = shiny::reactive(input$blk_substraction)
  )
  
  # ---- 4. Cached data reactives: raw_se_r, to_report_r, proc_r, ... ----
  # Keep whatever mechanism you use today to load this file, but it must run here, i.e. after
  # `ws`, `data_info`, `read_done` and `proc_done` exist.
  
  # ---- 5. Report ----
  report_obj <- genReportServer(
    "report",
    data_info = data_info,
    ws = ws,
    norm_method = shiny::reactive(input$norm_method)
  )
  
  output$the_report <- shiny::renderUI({
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
    filename = function() paste0("Internal Report ", input$norm_method, ".html"),
    content = function(file) {
      path <- report_obj$report_path()
      if (!file.exists(path)) stop("Generate the report first.")
      file.copy(path, file, overwrite = TRUE)
    }
  )
  
  source("server_setup.R", local = TRUE)

  r <- reactiveValues()

  is_target_mode <- reactive({
    req(raw_se_r())
    
    md <- SumExp::metadata(raw_se_r())
    
    str(md)
    
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
    
    tb_2_1_3 <- show$extract_qc_samples_to_list(raw_se_r()) |>
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
    selected <- input$tb_3_1_rows_selected
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

    # cat(is_outlier)
    if (!is.null(is_outlier)) {
      cat(
        "### Outlier samples\n\n",
        "Each internal standard (IS) is expected to have consistent values across all samples.\n",
        "After applying a log-transformation ($log(1 + x)$),",
        "the mean and standard deviation for each IS are calculated.\n",
        "A sample is flagged as an outlier for a given IS",
        "if its value deviates by more than 3 standard deviations from the IS mean.\n",
        "If more than 20% of IS features in a sample are flagged this way,",
        "the sample itself is classified as an outlier.\n",
        "The outlier samples are then removed from the data.\n\n"
      )
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
    m_rsd1 <- show$extract_qc_samples_to_list(internal_std_se_r()) |>
      lapply(function(se) {
        apply(se[["raw"]], 1, \(.x) round(show$rsd_perc(.x), 2))
      }) |>
      do.call(cbind, args = _) # Make sure the result is a matrix even if there is only one value
    # The `rownames` are syntactically valid names, not the original feature names
    stopifnot(identical(rownames(m_rsd1), rownames(internal_std_se_r())))
    rownames(m_rsd1) <- SumExp::row_df(internal_std_se_r())$feature_name


    m_rsd1 <- data.frame(m_rsd1,
      check.names = FALSE
    )


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
      cat("\n\nThe bars represent the peak areas of volumetric internal standards (vIS).\n\n")
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


  output$ggplot3_2_3 <- renderPlot({
    norm_se <- to_report_r()[["normalized"]]
    labs <- SumExp::name_labs(norm_se)
    names(labs) <- gsub(" .*", "", names(labs)) # First word only
    df_to_show <- show$extract_quant_qc(norm_se) |>
      show$calc_rsd_qstd(labs) |>
      dplyr::mutate(
        sign = ifelse(loess_norm <= closest_norm, "↓ LOESS", "↓ Closest"),
        sign = ifelse(is.na(sign), "All 0s", sign),
        sign = factor(sign, levels = c("↓ LOESS", "↓ Closest", "All 0s"))
      )
    ggplot3_2_3 <- show$ggplot_rsdp_metab(df_to_show, labs) +
      aes(color = sign) +
      scale_color_manual(values = c("#00BFC4", "#F8766D", "#7f7f7f")) +
      facet_grid(~QC)
    ggplot3_2_3
  })


  ################################################################################################################
  ### This is for table 3.2.3


  output$tb_3_2_3 <- DT::renderDataTable({
    norm_se <- to_report_r()[["normalized"]]
    labs <- SumExp::name_labs(norm_se)
    names(labs) <- gsub(" .*", "", names(labs)) # First word only
    df_to_show <- show$extract_quant_qc(norm_se) |>
      show$calc_rsd_qstd(labs) |>
      dplyr::mutate(
        sign = ifelse(loess_norm <= closest_norm, "↓ LOESS", "↓ Closest"),
        sign = ifelse(is.na(sign), "All 0s", sign),
        sign = factor(sign, levels = c("↓ LOESS", "↓ Closest", "All 0s"))
      )

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

  output$tb_4_1 <- DT::renderDataTable({
    ###################################################
    ### There are two issues.
    ## (1) One is the reactivity of params_yml$norm_method
    ## (2) One is reactivity to the data

    MAT_ID_BLANK_SUBT <- input$norm_method |>
      util$mat_id_of_blank_subtracted()
    MAT_ID_IN_CALIB <- util$mat_id_in_calibration(MAT_ID_BLANK_SUBT)

    if (is_target_mode()) {
      lst_proc <- shiny::req(proc_r())[[MAT_ID_BLANK_SUBT]]

      calcurve_lst <- lst_proc |>
        lapply(\(x) x$calcurve)

      for (ii in names(calcurve_lst)) {
        se <- calcurve_lst[[ii]]

        v <- util$spiked_conc_pts(se)
        v <- v[!is.na(v)] |>
          labelled::copy_labels_from(v)

        # ✅ convert table → data.frame
        v <- as.data.frame(table(v),
          check.names = FALSE
        )

        colnames(v) <- c(
          "Calibration curve samples",
          "Number of Samples"
        )

        # ✅ IF you still want a table for display, create separately
        v_table <- knitr::kable(
          v,
          align = c("r", "r"),
          caption = paste("Batch", ii)
        ) |>
          kableExtra::kable_styling(
            bootstrap_options = "striped",
            full_width = FALSE
          )

        # Now:
        # v        -> data.frame ✅
        # v_table  -> formatted table ✅
      }

      v <- data.frame(v,
        check.names = FALSE
      )
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

    MAT_ID_BLANK_SUBT <- input$norm_method |>
      util$mat_id_of_blank_subtracted()
    
    min_cal_points <- to_report_r()[["params"]][["min_cal_points"]]

    if (is_target_mode()) {
      lst_proc <- shiny::req(proc_r())[[MAT_ID_BLANK_SUBT]]
      
      # --------------------------------------------------------
      # Prepare data
      calcurve_lst <- lapply(lst_proc, function(x) x$calcurve)

      limit_df <- lapply(calcurve_lst, function(se) {
        SumExp::row_df(se) |>
          dplyr::select(
            min_c_conc,
            max_c_conc,
            lod,
            lloq,
            has_proper_range,
            src_calcurve
          ) |>
          tibble::as_tibble(rownames = "feature_id")
      }) |>
        purrr::list_rbind(names_to = "batch")

      # --------------------------------------------------------
      show_df <- limit_df |>
        dplyr::rename(min_c = min_c_conc, max_c = max_c_conc) |>
        dplyr::mutate(
          lod = ifelse(is.na(lod), "N.A.", as.character(lod)),
          min_c = ifelse(is.na(min_c), "No valid pts.", as.character(min_c)),

          # ✅ flags for coloring
          is_bad = !has_proper_range,
          is_global = src_calcurve == "Global",
          Note = dplyr::case_when(
            max_c == 0 ~ "All zero",
            src_calcurve == "Global" ~ "Global calibration",
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
        selection = "single",
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

    MAT_ID_BLANK_SUBT <- input$norm_method |>
      util$mat_id_of_blank_subtracted()
    
    if (is_target_mode()) {
      lst_proc <- shiny::req(proc_r())[[MAT_ID_BLANK_SUBT]]

      ############################################
      # Parameters
      log_scale <- to_report_r()[["params"]][["log_calibration"]]

      ############################################
      # Prepare data
      calcurve_lst <- lapply(lst_proc, \(ea) ea$calcurve)

      seq_feat_id <- rownames(calcurve_lst[[1]])

      calcurve_to_show_df <- lapply(calcurve_lst, SumExp::as_tibble) |>
        dplyr::bind_rows(.id = "Batch")

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

      # Split by feature
      lst_calcurve_to_show_df <- split(calcurve_to_show_df, calcurve_to_show_df$.row_id)

      lst_calcurve_to_show_df <- lst_calcurve_to_show_df[
        match(seq_feat_id, names(lst_calcurve_to_show_df), nomatch = 0)
      ]

      ############################################
      # ✅ Get selection EARLY (fast)
      selected_4_3 <- input$tb_4_2_rows_selected
      shiny::req(selected_4_3)

      dfm <- lst_calcurve_to_show_df[[selected_4_3[1]]]

      ############################################
      # Plot function
      calcurve_plot <- function(dfm, y = input$norm_method) {
        dfm <- dfm |>
          dplyr::mutate(
            Batch = factor(Batch),
            txt = paste0(
              "inj: ", injection_order,
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
          scale_shape_manual(values = c(16, 17, 15, 18)) +

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
      lst_proc <- shiny::req(proc_r())[[MAT_ID_BLANK_SUBT]]

      ############################################
      # Prepare data

      log_scale <- to_report_r()[["params"]][["log_calibration"]]

      calcurve_lst <- lapply(lst_proc, \(ea) ea$calcurve)

      concn_lst <- lapply(lst_proc, \(ea) {
        se <- ea$concn
        se
      })

      calib_lst <- lapply(calcurve_lst, \(se) {
        se
      })

      seq_feat_id <- rownames(calcurve_lst[[1]])

      ############################################
      # ✅ Selection
      selected <- input$tb_4_2_rows_selected
      shiny::req(selected)

      ############################################
      # ✅ Use ONE batch (same logic as ggplot4_3)
      batch_id <- names(concn_lst)[1]

      concn_se <- concn_lst[[batch_id]]
      calib_se <- calib_lst[[batch_id]]

      ############################################
      # Split into features
      lst_concn_se <- SumExp::split_rows(
        concn_se,
        rownames(SumExp::row_df(concn_se))
      )

      # Keep order consistent
      lst_concn_se <- lst_concn_se[
        match(seq_feat_id, names(lst_concn_se), nomatch = 0)
      ]


      se_selected <- lst_concn_se[[selected[1]]]


      ############################################
      # ✅ Build ONE plot


      df_check <- SumExp::as_tibble(se_selected)

      valid_ids <-
        which(SumExp::row_df(concn_se)$has_proper_range)


      invalid_ids <-
        which(!SumExp::row_df(concn_se)$has_proper_range)


      if (is.null(se_selected)) {
        message("se_selected is NULL")
      } else {
        message("selected")
        print(selected)
        message("invalid_ids ")
        print(invalid_ids)

        message("se_selected itself")

        se_selected_print <- as_tibble(se_selected)
        print(se_selected_print)
        message("Class of se_selected:")
        print(class(se_selected))

        message("Dimensions of se_selected:")
        print(dim(se_selected))

        df_check <- SumExp::as_tibble(se_selected)

        message("Preview of df_check:")
        print(df_check)
        print(colnames(df_check))
        message("Summary of raw:")
        print(summary(df_check$raw))

        message("Summary of lod:")
        print(summary(df_check$lod))
      }


      if (selected %in% invalid_ids) {


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
    shiny::req(input$tb_4_2_rows_selected)
    shiny::req(to_report_r())

    MAT_ID_BLANK_SUBT <- input$norm_method |>
      util$mat_id_of_blank_subtracted()

    if (shiny::req(is_target_mode())) {
      # Load the processed data using the specified normalization method
      lst_proc <- shiny::req(proc_r())[[MAT_ID_BLANK_SUBT]]
      selected <- input$tb_4_2_rows_selected
      selected_row <- selected[1]

      log_scale <- to_report_r()[["params"]][["log_calibration"]]

      # ------------------------------------------------------------
      # Prepare data
      calcurve_lst <- lapply(lst_proc, function(ea) ea$calcurve)
      concn_lst <- lapply(lst_proc, function(ea) ea$concn)

      feature_id <- rownames(calcurve_lst[[1]])[selected_row]

      concn_se <- concn_lst[[1]]

      show_df <- SumExp::as_tibble(concn_se)
      show_df_by_feature <- split(show_df, show_df$feature_name)

      df1 <- show_df_by_feature[[feature_id]]

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
