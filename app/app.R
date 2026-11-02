## ------------------------------------------------------------------------------------------- #
# This is a Shiny web application.
# ------------------------------------------------------------------------------------------- #

options(readr.show_progress = FALSE)

options(shiny.host = "0.0.0.0")
options(shiny.port = 7579)
# Upload size limit of the app itself: no practical limit (uploads are stored on disk). On
# SciLifeLab Serve, its web proxy allows at most 1000 MB anyway (see R/uploadLimit.R).
options(shiny.maxRequestSize = 100 * 1024^3)

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
# Theme: Bootstrap variables only; the QuaNTA design itself is in www/quanta.css (loaded in the
# UI header, after the theme). Colours of the logo: blue #1A0DF2, orange #FFB326.
custom_theme <- bs_theme(
  version = 5,
  bootswatch = NULL,
  # All fonts are served from www/fonts/ (@font-face rules in www/fonts/fonts.css, linked in the UI
  # header), so the app never contacts Google Fonts (`font_google()` downloads at start-up)
  base_font = font_collection("Lato", "sans-serif"),
  heading_font = font_collection("Lato", "sans-serif"),
  code_font = font_collection("JetBrains Mono", "monospace"),
  bg = "#FFFFFF",
  fg = "#1F2A44",
  primary = "#1A0DF2",
  secondary = "#667085",
  info = "#9ADCFF",
  success = "#2E9E5B",
  warning = "#FFB326",
  danger = "#D92D20",
  "body-bg" = "#F5F7FA",
  "border-radius" = "6px",
  "link-color" = "#1A0DF2",
  "navbar-bg" = "#FFFFFF",
  "navbar-fg" = "#1F2A44"
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

quanta_card <- function(title, ..., icon_name = NULL, subtitle = NULL, actions = NULL, below = NULL) {
  card(quanta_card_header(title, icon_name, subtitle, actions = actions, below = below), ...)
}

# A card in a resizable pane (Split.js) of the analysis tabs, e.g. side = "qc-left"
quanta_card_resize <- function(title, side, ...) {
  icon_name <- switch(title, Tables = "table", Figures = "chart-column", NULL)
  div(id = side, card(quanta_card_header(title, icon_name), ...))
}
section <- function(title, ...) div(style = "padding: 0;", h3(class = "q-section-title", title), ...)

# Explanation of each table and figure, shown on hover over the info icon next to its title
SECTION_TIPS <- list(
  "2.1" = "Number and share of samples per category (blank, calibration curve, QC, study sample) and class, from the sample information rows of the data file.",
  "2.2" = "Number of calibration curve samples per concentration level, per batch and in total. The concentrations are read from the calibrant names (e.g. Cal0-5 = 0.5).",
  "2.3" = "Number of features per type, from the Comment column: Quant (targeted), IS (internal standards), vIS (volumetric internal standard), empty (non-targeted).",
  "2.4" = "Every feature by retention time and m/z. After processing, the targeted compounds are coloured by their outcome (quantified, no valid calibration, removed, ...). Non-targeted features can be shown via the legend.",
  "2.5" = "Number of features per m/z, in bins of 0.5. Hover over a bar for its range; drag to zoom, double-click to reset.",
  "2.6" = "All raw peak areas of all features and samples, in linear scale (left) and log scale (right, positive values only): the dynamic range of the data and the share of zeros.",
  "3.1" = "All internal standards (IS) with retention time, m/z, the RSD% of the raw peak areas over all samples and the number of zero values; failed IS are coloured. Select a row (or use the arrow keys) to show the IS in 3.4 and 3.5.",
  "3.2" = "An IS fails when more than 10% of the samples have a zero value. Failed IS are removed before normalization.",
  "3.3" = "A sample is an outlier when more than 20% of the IS deviate by more than 3 SD from their mean (log scale) or are missing (zero). Outlier samples are removed before normalization; with the processing option \"Non-calibration samples only\", calibration samples are always kept.",
  "3.4" = "RSD% of the raw peak areas of the IS selected in 3.1, within each QC group and each sample class (with the number of samples). An RSD% needs at least 2 samples.",
  "3.5" = "Raw peak areas of the IS selected in 3.1, by sample group (left) and along the injection order (right).",
  "3.6" = "Raw peak area of each volumetric IS per sample along the injection order (all samples, also outliers), and its RSD% per group: how constant the added volume was.",
  "3.7" = "Peak areas of all IS per sample along the injection order, stacked, with a LOESS trend of the total. Two panels, e.g. to compare raw and normalized values.",
  "3.8" = "Each line is one targeted compound in one QC group: its RSD% among the QC replicates without normalization (Raw) and with each method, before blank filtering. The colour is the method with the lowest RSD%.",
  "3.9" = "The RSD% of 3.8 as a table: per targeted compound, QC group and method. Filter by QC group or search a feature.",
  "3.10" = "Features removed by the blank filter or by the QC RSD% filter, for the chosen normalization method, with the reason.",
  "3.11" = "One feature in the blanks next to the QCs, study samples (and calibrants), with the threshold of the blank filter (factor × blank mean). Groups: mean ± SD and the samples; per sample: one bar per injection.",
  "4.1" = "Number of calibration curve samples per concentration in the selected batch.",
  "4.2" = "Per chemical: the calibration curve used (batch or global), the LOD, the LLOQ (start of the curve), the highest level and the ULOQ (linear curves, or the top of the linear range with log calibration; empty for a quadratic curve: no ULOQ), with a note when the curve is not valid. Red: no valid curve; blue: global curve; orange: kept although not valid; grey: removed before calibration. Select a row (or use the arrow keys) to show the chemical in the figures; the first chemical is shown at the start.",
  "4.3" = "Number of chemicals per batch with a valid calibration curve and concentrations in the samples.",
  "4.4" = "The calibration points of the selected chemical with the fitted curve, coloured by their place: within the calibration range, at the LOD, Cal0 or out of range; one shape per batch. The title is coloured as the row in 4.2 (red: not valid, orange: kept although not valid, blue: global curve), where the reason is given.",
  "4.5" = "The samples of the selected chemical on its calibration curve (signal against the calculated concentration); the shaded areas are below the LOD and the LLOQ. The title is coloured as in 4.4.",
  "4.6" = "Concentrations of the selected chemical per class: a box per class and the samples as points; values imputed by the LOD/LLOQ rules are coloured.",
  "4.7" = "The selected chemical in the QC samples of all batches along the injection order, a line per QC group: the values should be stable over the run. Open points: values replaced by the LOD/LLOQ rules."
)
#' A title with its number and the info icon of `SECTION_TIPS`, e.g. tip_title("Features", "2.3")
tip_title <- function(title, num) tagList(paste0(title, " (", num, ")"), info_tip(SECTION_TIPS[[num]]))

#' `onInitialize` of a dropdown (selectize): the arrow keys up / down choose the previous / next
#' choice, so that the figure follows at once (as the arrow keys in the tables 3.1 and 4.2). An open
#' list is closed first (choosing in it would take the focus away); the box keeps the focus, so
#' that the next key goes on. While searching (text typed), the arrow keys move in the list.
ARROW_KEYS_SELECTIZE <- I("function() {
  var self = this;
  self.$control_input[0].addEventListener('keydown', function (e) {
    if (e.key !== 'ArrowDown' && e.key !== 'ArrowUp') return;
    if (self.isOpen && self.$control_input.val()) return;   // Searching: move in the found choices
    e.preventDefault(); e.stopImmediatePropagation();
    // From the highlighted choice of an open list, otherwise from the chosen one
    var from = self.isOpen && self.$activeOption && self.$activeOption.length ?
      self.$activeOption.attr('data-value') : self.getValue();
    var vals = Object.values(self.options).sort(function (a, b) { return a.$order - b.$order; })
      .map(function (o) { return String(o[self.settings.valueField]); });
    var i = vals.indexOf(String(from)) + (e.key === 'ArrowDown' ? 1 : -1);
    if (self.isOpen) self.close();
    if (i >= 0 && i < vals.length) self.setValue(vals[i]);
    // Keep the focus for the next key (choosing can blur the box); the list stays closed
    setTimeout(function () { self.focus(); if (self.isOpen) self.close(); }, 0);
  }, true);
}")

################################################################################################
######################################## UI ####################################################
################################################################################################

ui <-
  page_navbar(
    theme = custom_theme,
    title = div(
      class = "quanta-brand",
      div(
        class = "quanta-brand-main",
        tags$img(src = "QuaNTA.jpg", alt = "QuaNTA"),
        span(class = "quanta-tagline", "Quantitative & Nontargeted Analysis")
      ),
      span(class = "quanta-brand-divider"),
      tags$img(class = "quanta-brand-partner", src = "SciLifeLab_Logotype_Green_POS.png",
               alt = "SciLifeLab")
    ),
    window_title = "QuaNTA",   # text in the browser tab; fixes the last warning
    header = tags$head(
      tags$link(rel = "icon", type = "image/jpeg", href = "QuaNTA_square_2.jpg"),   # Browser tab icon
      tags$script(src = "split.min.js"),
      tags$link(rel = "stylesheet", href = "fonts/fonts.css"),   # Lato, Oswald, JetBrains Mono (local)
      # The QuaNTA design; "?v=" (the file's modification time) makes browsers load a changed file
      tags$link(rel = "stylesheet",
                href = paste0("quanta.css?v=", as.integer(file.mtime("www/quanta.css")))),
      shinyjs::useShinyjs(),   # Enable / disable buttons from the server
      # Start a download from the server: clicks the (hidden) download link with the given ID.
      # Used when a file is ready after a background task (external report, session data).
      tags$script(HTML("
        Shiny.addCustomMessageHandler('quantaClickDownload', function(id) {
          var a = document.getElementById(id);
          if (a) a.click();
        });
      ")),
      # All plotly plots: a light grey plot area (grid lines: www/quanta.css), as in the internal report
      tags$script(HTML("
        (function () {
          var BG = '#F4F5F7';
          // New plots get the colour when they are drawn ...
          (function patch() {
            if (!window.Plotly) return setTimeout(patch, 100);
            if (Plotly.__quantaBg) return;
            Plotly.__quantaBg = true;
            ['newPlot', 'react'].forEach(function (fn) {
              var orig = Plotly[fn];
              Plotly[fn] = function (gd, data, layout) {
                var lay = (data && !Array.isArray(data) && data.layout) ? data.layout : layout;
                if (lay && typeof lay === 'object') {
                  lay.plot_bgcolor = BG;
                  // Large numbers on the axes in scientific notation (1E+4), not as 10k
                  Object.keys(lay).forEach(function (k) {
                    var ax = lay[k];
                    if (/^[xy]axis[0-9]*$/.test(k) && ax && typeof ax === 'object' && !ax.exponentformat) ax.exponentformat = 'E';
                  });
                }
                return orig.apply(this, arguments);
              };
            });
          })();
          // ... and those drawn before the library was hooked (the first plots: plotly is only loaded
          // with the first plot) are recoloured
          setInterval(function () {
            if (!window.Plotly) return;
            document.querySelectorAll('.js-plotly-plot').forEach(function (el) {
              if (!el._fullLayout) return;
              var upd = {};
              if (el._fullLayout.plot_bgcolor !== BG) upd.plot_bgcolor = BG;
              // Scientific axis labels also for these plots (unless a plot sets its own format)
              Object.keys(el._fullLayout).forEach(function (k) {
                if (!/^[xy]axis[0-9]*$/.test(k)) return;
                var own = el.layout && el.layout[k] && el.layout[k].exponentformat;
                var fa = el._fullLayout[k];
                if (!own && fa.exponentformat !== 'E') upd[k + '.exponentformat'] = 'E';
                // Plotly writes exponents from 10000 on: from 1000 on as e.g. 2.5e+3 (while the axis
                // shows that range; also after zooming)
                if (fa.type !== 'linear' || !fa.range) return;
                el.__qTf = el.__qTf || {};
                var own_tf = el.layout && el.layout[k] && el.layout[k].tickformat && !el.__qTf[k];
                var m = Math.max(Math.abs(+fa.range[0]), Math.abs(+fa.range[1]));
                var want = m >= 1000 && m < 10000;
                if (!own_tf && want && !el.__qTf[k]) { upd[k + '.tickformat'] = '.2~e'; el.__qTf[k] = true; }
                if (!want && el.__qTf[k]) { upd[k + '.tickformat'] = ''; el.__qTf[k] = false; }
              });
              if (Object.keys(upd).length) Plotly.relayout(el, upd);
            });
          }, 1000);
        })();
      ")),
      upload_limit_script(),   # On SciLifeLab Serve: stop uploads above its 1000 MB limit early
      gdpr_notice_script(),    # On SciLifeLab Serve: GDPR popup before choosing a data file
      # Logs of the scripts (read, process, export): follow the newest lines, unless the user has
      # scrolled up to read earlier lines (their height is set in www/quanta.css)
      tags$script(HTML("
        $(document).on('shiny:value', function(e) {
          if (!/script_output$/.test(e.name)) return;
          var el = document.getElementById(e.name);
          if (!el) return;
          var atBottom = el.scrollHeight - el.scrollTop - el.clientHeight < 40;  // Before the update
          setTimeout(function() { if (atBottom) el.scrollTop = el.scrollHeight; }, 0);
        });
      ")),
      # Tables (DT) in scrolling boxes:
      # - the selected row (tables where a row chooses what is shown: 3.1, 4.2) is scrolled into view
      # - the arrow keys up / down select the previous / next row of the table clicked last,
      #   instead of scrolling the page
      # - the column headers are re-aligned when a hidden page is shown or a pane is resized
      tags$script(HTML("
        (function () {
          var lastTable = null;
          function scrollToSelected(wrapper) {
            var tr = $(wrapper).find('tbody tr.selected')[0], box = $(wrapper).find('.dataTables_scrollBody')[0];
            if (!tr || !box) return;
            var top = tr.offsetTop, h = tr.offsetHeight;
            if (top < box.scrollTop || top + h > box.scrollTop + box.clientHeight) box.scrollTop = top - box.clientHeight / 2 + h / 2;
          }
          $(document).on('init.dt', function (e, settings) {
            var wrapper = $(settings.nTable).closest('.dataTables_wrapper')[0];
            setTimeout(function () { scrollToSelected(wrapper); }, 200);
          });
          $(document).on('mousedown', '.dataTables_wrapper tbody tr', function () {
            lastTable = $(this).closest('.dataTables_wrapper')[0];
          });
          // A click on the selected row does not deselect it (3.1, 4.2: there is always one IS /
          // compound shown). Caught before the table's own handler (capture phase).
          document.addEventListener('mousedown', function (e) {
            var tr = e.target.closest && e.target.closest('.dataTables_wrapper tbody tr.selected');
            if (!tr) return;
            lastTable = tr.closest('.dataTables_wrapper');
            e.stopPropagation();
          }, true);
          $(document).on('keydown', function (e) {
            if (e.key !== 'ArrowDown' && e.key !== 'ArrowUp') return;
            if (!lastTable || !document.body.contains(lastTable) || $(lastTable).is(':hidden')) return;
            if ($(e.target).is('input, textarea, select, [contenteditable]')) return;
            var cur = $(lastTable).find('tbody tr.selected').first();
            if (!cur.length) return;   // Only tables where a row is selected
            var next = e.key === 'ArrowDown' ? cur.nextAll('tr:visible').first() : cur.prevAll('tr:visible').first();
            e.preventDefault();
            if (!next.length) return;
            next.trigger('mousedown');   // DT selects a row on mousedown
            scrollToSelected(lastTable);
          });
          function adjust() {
            if ($.fn.dataTable) $.fn.dataTable.tables({ visible: true, api: true }).columns.adjust();
          }
          $(document).on('shown.bs.tab', function () { setTimeout(adjust, 50); });
          $(window).on('resize', function () { clearTimeout(window.__qAdj); window.__qAdj = setTimeout(adjust, 150); });
        })();
      "))
    ),
    nav_panel(
      title = quanta_step(1, "Input data\nand processing"),
      value = "input",
      layout_columns(
        col_widths = c(4, 4, 4),
        quanta_card(
          "Input data settings",
          icon_name = "folder-open",
          subtitle = "Provide project information and input files.",
          # Session data save / load hidden for now (very large files); the code is kept:
          # sessionLoadUI("session_data"), .hr(),   # Load saved session data (.rds), R/sessionData.R
          msdialInfoUI("data_info")
        ),
        quanta_card(
          "Data processing settings",
          icon_name = "gear",
          subtitle = "Configure processing, quality control and calibration options.",
          # Parameters and processing settings (.yml), in a row of the header under the subtitle:
          # always visible, and the title keeps its full width
          below = bslib::tooltip(
            msdialInfoDownloadUI("data_info", label = "Download settings (.yml)", class = "btn-sm-quanta"),
            "Download the project information and all processing settings as a .yml file, to load them again later.",
            placement = "bottom"
          ),
          runReadMsdialUI("read_msdial"),
          runProcessUI("proc")
          # Session data save hidden for now (very large files); the code is kept:
          # sessionSaveUI("session_data"),   # Save session data (.rds), R/sessionData.R
        ),
        card(
          class = "q-log",
          quanta_card_header(
            "Log", "file-lines",
            subtitle = "Processing information and the data reading log.",
            actions = actionButton("clear_log", "Clear log", icon = icon("trash-can"),
                                   class = "btn-sm-quanta")
          ),
          navset_underline(
            id = "log_tabs",
            nav_panel("Data reading log", runReadMsdialUI_2("read_msdial")),
            nav_panel("Processing log", runProcessUI_2("proc")),
            # What happened to the features and samples (the event log of the processing)
            nav_panel("Processing history", uiOutput("history_ui"))
          )
        )
      )
    ),
    nav_panel(
      title = quanta_step(2, "Data\noverview"),
      value = "overview",
      div(
        id = "ov-split",
        class = "split-pane html-fill-item",   # Fills the window (see quanta.css)
        #Card
        quanta_card_resize("Tables", "ov-left",
                    section(tip_title("Samples by category", "2.1"),
                            spinner(DT::DTOutput("tb_2_1_1"))),
                    section(tip_title("Calibration levels", "2.2"),
                            spinner(DT::DTOutput("tb_2_1_2_2"))),
                    section(tip_title("Classes of analytes", "2.3"),
                            spinner(DT::DTOutput("tb_2_2")))),
        #Card
        quanta_card_resize("Figures", "ov-right",
                    section(tip_title("The distribution of analytes across m/z and retention time", "2.4"),
                            spinner(plotlyOutput("ggplot1_2_2", height = "540px"))),
                    section(tip_title("Distribution of m/z", "2.5"),
                            spinner(plotlyOutput("ggplot2_2_2", height = "420px"))),
                    section(tip_title("Distribution of intensities", "2.6"),
                            spinner(plotlyOutput("ggplot2_3", height = "440px"))))
        
      ),
      tags$script(HTML("
        $(document).on('shiny:sessioninitialized', function() {
          if (typeof Split === 'undefined') {
            console.warn('Split.js did not load — check app/www/split.min.js exists.');
            return;
          }
          Split(['#ov-left', '#ov-right'], {
            sizes: [33, 67],   // Tables 1/3, figures 2/3
            minSize: 250,
            gutterSize: 8,
            onDragEnd: function() { window.dispatchEvent(new Event('resize')); }
          });
        });
      "))
    ),
    nav_panel(
      title = quanta_step(3, "Quality control\nand normalization"),
      value = "qc",
      div(
        id = "qc-split",
        class = "split-pane html-fill-item",
        #Card
        quanta_card_resize("Tables", "qc-left",
                    section(tip_title("Internal standards", "3.1"),
                            spinner(DT::DTOutput("tb_3_1"))),
                    section(tip_title("Failed internal standards", "3.2"),
                            spinner(htmlOutput("text_3_1_1"))),
                    section(tip_title("Outlier samples", "3.3"),
                            spinner(uiOutput("wrap_outlier")),
                            h3(
                              tip_title("Raw RSD%", "3.4"),
                              style = "margin: 0;"
                            ),
                            spinner(DT::DTOutput("tb_3_4_1")))),
        #Card
        quanta_card_resize("Figures", "qc-right",
                    # Title with the IS selected in 3.1 (server: title_3_5)
                    section(tagList(textOutput("title_3_5", inline = TRUE), " (3.5)",
                                    info_tip(SECTION_TIPS[["3.5"]])),
                            spinner(plotlyOutput("ggplot3_1_3", height = "500px")),
                            spinner(uiOutput("remove_failedIS_outlier"))),
                    div(
                      h3(tip_title("Volumetric internal standards", "3.6"), style = "margin: 0 0 8px 0;"),
                      div(
                        style = "display: flex; gap: 20px; flex-wrap: wrap; align-items: flex-end;",
                        selectInput("vis_colour", "Colour",
                                    choices = c("Sample group" = "group", "Batch" = "batch"),
                                    width = "200px"),
                        # Calibration samples have much higher values than the samples: hidden by default
                        checkboxInput("vis_show_calibrants", "Show calibration samples", value = FALSE)
                      ),
                      spinner(uiOutput("vis_or_text_3_2_1_1"))
                    ),
                    div(
                      h3(tip_title("Internal standards over the sequence", "3.7"), style = "margin: 16px 0 4px 0;"),
                      p("Two independent views: choose the colouring and the values of each, e.g. raw and",
                        "normalized, or the same data in two colourings. Failed IS and outlier samples are",
                        "left out."),
                      uiOutput("best_norm_note_3"),
                      # For both panels; calibration samples have much higher values than the samples
                      checkboxInput("isbar_show_calibrants", "Show calibration samples", value = FALSE),
                      # Two panels side by side; below each other in a narrow window
                      div(
                        style = "display: flex; flex-wrap: wrap; gap: 16px;",
                        lapply(c("a", "b"), function(k) {
                          div(
                            style = "flex: 1 1 420px; min-width: 0;",
                            div(
                              style = "display: flex; gap: 12px; flex-wrap: wrap;",
                              selectInput(paste0("isbar_colour_", k), "Colour",
                                          choices = c("Sample group" = "group", "Batch" = "batch",
                                                      "Internal standard" = "is"),
                                          width = "200px"),
                              selectInput(paste0("isbar_values_", k), "Values", choices = c(Raw = "raw"),
                                          width = "200px")
                            ),
                            spinner(plotlyOutput(paste0("ggplot3_2_1_2", k), height = "500px"))
                          )
                        })
                      ),
                      h3(
                        tip_title("Comparison of normalization methods", "3.8"),
                        style = "margin: 0;"
                      ),
                      spinner(plotlyOutput("ggplot3_2_3", height = "550px")),
                      h3(
                        tip_title("RSD% values", "3.9"),
                        style = "margin: 0;"
                      ),
                      spinner(DT::DTOutput("tb_3_2_3")),
                      # Features removed by blank filtering or the QC RSD% filter
                      h3(
                        tip_title("Removed features", "3.10"),
                        style = "margin: 0;"
                      ),
                      shiny::selectInput(
                        inputId = "removed_method",
                        label = "Normalization method",
                        choices = NORM_METHOD_LABELS,   # Updated to the computed ones
                        width = "260px"
                      ),
                      shiny::uiOutput("removed_note"),
                      spinner(DT::DTOutput("tb_3_2_4")),
                      # QC/QA: one feature in the blanks compared to the other sample groups
                      h3(tip_title("Blanks compared to samples", "3.11"), style = "margin: 16px 0 4px 0;"),
                      p("One feature at a time: the (method) blanks in red next to the QCs, study",
                        "samples and calibrants, with the threshold of the blank filter. The values are",
                        "normalized but before blank filtering and blank subtraction. Type in the feature",
                        "box to search; in the Feature, Normalization and View boxes, the arrow keys up /",
                        "down switch to the previous / next choice.",
                        "Internal standards are not blank-filtered (see 3.1 and 3.5)."),
                      div(
                        class = "q-controls-row",   # Labels at the same height (www/quanta.css)
                        radioButtons("blank_type", "Features",
                                     choices = c(Targeted = "targeted", "Non-targeted" = "nontargeted",
                                                 All = "all"),
                                     selected = "targeted", inline = TRUE),
                        selectizeInput("blank_feature", "Feature", choices = NULL, width = "250px",
                                       options = list(
                                         placeholder = "Type to search…",
                                         openOnFocus = FALSE,   # Opens on a click, not when the focus is given back
                                         onInitialize = ARROW_KEYS_SELECTIZE   # Arrow keys switch the feature
                                       )),
                        selectizeInput("blank_values", "Normalization", choices = c(Raw = "raw"), width = "170px",
                                       options = list(openOnFocus = FALSE, onInitialize = ARROW_KEYS_SELECTIZE)),
                        selectizeInput("blank_view", "View",
                                       choices = c("Groups" = "groups", "Per sample" = "samples"),
                                       selected = "groups", width = "140px",
                                       options = list(openOnFocus = FALSE, onInitialize = ARROW_KEYS_SELECTIZE))
                      ),
                      div(
                        style = "display: flex; gap: 20px; flex-wrap: wrap; align-items: center;",
                        checkboxInput("blank_removed_only", "Only features removed by the blank filter",
                                      value = FALSE),
                        checkboxInput("blank_show_calibrants", "Show calibration samples", value = FALSE)
                      ),
                      # How many features of the chosen kind the blank filter removed
                      uiOutput("blank_removed_count"),
                      spinner(plotlyOutput("ggplot3_3", height = "500px"))
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
            sizes: [33, 67],   // Tables 1/3, figures 2/3
            minSize: 250,
            gutterSize: 8,
            onDragEnd: function() { window.dispatchEvent(new Event('resize')); }
          });
        });
      "))
    ),
    nav_panel(
      title = quanta_step(4, "Calibration\nand quantification"),
      value = "calibration",
      div(
        id = "cal-split",
        class = "split-pane html-fill-item",   # Resizable, as pages 2 and 3
        quanta_card_resize("Tables", "cal-left",
                    uiOutput("best_norm_note_4"),
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
                        tip_title("Calibration curve samples", "4.1"),
                        style = "margin: 0;"
                      ),
                      spinner(DT::DTOutput("tb_4_1")),
                    ),
                    div(
                      style = "padding: 0;",
                      h3(
                        tip_title("Calibration curve limits", "4.2"),
                        style = "margin: 0;"
                      ),
                      spinner(DT::DTOutput("tb_4_2")),
                    ),
                    div(
                      style = "padding: 0;",
                      h3(
                        tip_title("Removal of chemicals with no measurement samples", "4.3"),
                        style = "margin: 0;"
                      ),
                      spinner( DT::DTOutput("tb_4_5")),
                    )),
        quanta_card_resize("Figures", "cal-right",
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
                        tip_title("Calibration curves per chemical", "4.4"),
                        style = "margin: 0;"
                      ),
                      div(
                        style = "width: 100%;", # aspect-ratio: 4 / 3;",   # ✅ shorter height
                        spinner(plotlyOutput("ggplot4_3")),
                      )
                    ),
                    div(
                      h3(
                        tip_title("Calibration curves of all measurement samples", "4.5"),
                        style = "margin: 0;"
                      ),
                      
                      
                      div(
                        style = "width: 100%;",
                        spinner(plotlyOutput("ggplot4_4_2")),
                      )
                    ),
                    div(
                      h3(
                        tip_title("Concentration", "4.6"),
                        style = "margin: 0;"
                      ),
                      div(
                        style = "width: 100%;",
                        # As high as the plot (460 px), so that it does not cover the title of 4.7
                        spinner(plotlyOutput("ggplot5_1", height = "470px")),
                      )
                    ),
                    div(
                      h3(
                        tip_title("QC samples in acquisition order", "4.7"),
                        style = "margin: 0;"
                      ),
                      p("The chemical selected in 4.2, in the QC samples of all batches (each with its",
                        "own calibration), along the injection order."),
                      div(
                        style = "width: 100%;",
                        spinner(plotlyOutput("ggplot4_7", height = "460px")),
                      )
                    )
                  )
      ),
      tags$script(HTML("
        $(document).on('shiny:sessioninitialized', function() {
          if (typeof Split === 'undefined') return;
          Split(['#cal-left', '#cal-right'], {
            sizes: [33, 67],   // Tables 1/3, figures 2/3
            minSize: 250,
            gutterSize: 8,
            onDragEnd: function() { window.dispatchEvent(new Event('resize')); }
          });
        });
      "))
    ),
    nav_panel(
      title = quanta_step(5, "Report\nexport", last = TRUE),
      value = "reports",
      # Final normalization method: used by both reports and the exported tables. Nothing is
      # pre-selected; the generate / export buttons stay disabled until a method is chosen.
      div(
        style = "width: 40%; min-width: 320px;",
        quanta_card("Final normalization method",
                    icon_name = "sliders",
                    subtitle = "Used for both reports and the exported tables.",
                    spinner(uiOutput("final_norm_ui")))
      ),
      layout_columns(
        col_widths = c(9, 3),
        quanta_card(
          "Report",
          icon_name = "file-lines",
          subtitle = "Generate the internal (HTML) and external (PDF) reports.",
          genReportUI("report"),
          # Above the report: the rendered report is long and scrolls on its own
          div(
            class = "d-flex justify-content-center my-2",
            downloadButton(
              outputId = "download_report",
              label    = "Download HTML Report",
              class    = "btn btn-sm btn-primary",
              style    = "font-size:18px; padding:12px 24px;"
            )
          ),
          spinner(uiOutput("the_report"))
        ),
        quanta_card("Export datasets",
                    icon_name = "table",
                    subtitle = "Export the tables of the final normalization method.",
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

  # Message when a file above SciLifeLab Serve's upload limit is chosen
  uploadLimitServer(input, session)

  # ---- 1. Private workspace of this session (deleted when the session ends) ----
  ws <- create_session_workspace(session)
  
  # ---- Removing all results (read data, processing, tables, reports; not the uploaded file) ----
  # E.g. when a parameters file is loaded: the results no longer match the settings. The
  # reactives that load the results depend on `results_reset()` (server_setup.R), so that all
  # tables and figures are emptied.
  results_reset <- shiny::reactiveVal(0)
  remove_all_results <- function() {
    unlink(list.files(ws$intermediate, full.names = TRUE), recursive = TRUE)
    unlink(list.files(ws$tables, full.names = TRUE), recursive = TRUE)
    unlink(list.files(ws$report, full.names = TRUE), recursive = TRUE)
    results_reset(shiny::isolate(results_reset()) + 1)
    report_obj$reset()   # Defined below; called later only
  }

  # ---- 2. Parameters: come from the inputs; a loaded file only pre-fills them ----
  data_info <- msdialInfoServer(
    "data_info",
    ws = ws,
    processing_params = shiny::reactive(collect_processing_params(input)),
    on_params_loaded = remove_all_results
  )
  # Settings of a loaded parameters file that were not valid (set to their default): shown when
  # "Process data" is clicked
  param_notes <- shiny::reactiveVal(NULL)
  shiny::observeEvent(data_info$loaded_processing, {
    notes <- apply_processing_params(session, data_info$loaded_processing)
    param_notes(if (length(notes) > 0) notes else NULL)
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
  # Which normalization was best, in QuaNTA blue (pages 3 and 4; one output per place)
  best_norm_note <- shiny::reactive({
    tr <- shiny::req(to_report_r())
    rsd <- tr[["median RSD% per normalization"]]
    basis <- tr[["median RSD% basis"]]
    best <- best_norm_r()
    lab <- names(NORM_METHOD_LABELS)[match(best, NORM_METHOD_LABELS)]
    txt <- if (is.null(rsd) || is.null(basis) || is.na(basis) || is.na(rsd[best])) {
      paste0("Best normalization: not determined (no replicates); ", lab, " is used by default.")
    } else {
      paste0("Best normalization: ", lab, " (lowest median RSD% of the ",
             if (basis == "QC") "QC" else "calibration", " replicates: ", round(rsd[best], 1), "%).")
    }
    shiny::div(class = "q-best-norm", txt)
  })
  output$best_norm_note_3 <- shiny::renderUI(best_norm_note())
  output$best_norm_note_4 <- shiny::renderUI(best_norm_note())
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
  clear_log <- shiny::reactive(input$clear_log)   # "Clear log" empties both logs
  read_done <- runReadMsdialServer("read_msdial", data_info = data_info, ws = ws, clear = clear_log)
  # `data_caps_r()` is defined below (reactives are evaluated lazily)
  proc_done <- runProcessServer("proc", data_info = data_info, ws = ws, clear = clear_log,
                                data_caps = shiny::reactive(data_caps_r()), param_notes = param_notes)
  # Show the log of the step that is started
  shiny::observeEvent(input[["read_msdial-run_button"]], {
    bslib::nav_select("log_tabs", "Data reading log")
  }, ignoreInit = TRUE)
  shiny::observeEvent(input[["proc-run_button"]], {
    bslib::nav_select("log_tabs", "Processing log")
  }, ignoreInit = TRUE)
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
    norm_method = final_norm_method_r,
    # No PDF report without calibration (non-target mode): the reason instead (`is_target_mode()`
    # and `no_calib_msg_r()` are defined below)
    no_external_reason = shiny::reactive(if (is_target_mode()) NULL else no_calib_msg_r())
  )

  output$the_report <- shiny::renderUI({
    # Re-reading / a new upload / loading parameters deletes the report (see below / msdialInfo.R):
    # hide it then
    input[["read_msdial-run_button"]]
    input[["data_info-input_file_upload"]]
    results_reset()
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
    # The name of the generated file, with the time of generation
    filename = function() basename(report_obj$report_path()),
    content = function(file) {
      path <- report_obj$report_path()
      if (!nzchar(path) || !file.exists(path)) stop("Generate the report first.")
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
    # Also the earlier read data: after a read with input errors, nothing can be processed
    if (!is.null(f) && nzchar(f)) {
      unlink(c(rds_file(""), rds_file("proc"), rds_file("to_report"), rds_file("app")))
      unlink(Sys.glob(sub("\\.rds$", "*.rds", rds_file("blank_"))))   # The values of 3.11 per method
    }
    unlink(file.path(ws$intermediate, "input_errors.txt"))
    unlink(list.files(ws$tables, full.names = TRUE), recursive = TRUE)
    unlink(list.files(ws$report, full.names = TRUE), recursive = TRUE)
  }, priority = 100)

  # ---- Input errors found by "Read" (read-msdial.R): a popup asking for a corrected file ----
  shiny::observeEvent(read_done(), {
    path <- file.path(ws$intermediate, "input_errors.txt")
    if (!file.exists(path)) return()
    errs <- readLines(path, warn = FALSE)
    shiny::showModal(shiny::modalDialog(
      title = "The file needs to be corrected",
      shiny::p(paste0("The data could not be read, because the file has the following problem",
                      if (length(errs) > 1) "s", ":")),
      shiny::tags$ul(lapply(errs, shiny::tags$li)),
      shiny::p("Please correct the file and upload it again. The data cannot be processed until then."),
      easyClose = TRUE,
      footer = shiny::modalButton("OK")
    ))
  })

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

  # Why there is no calibration (non-target mode), shown by the empty calibration tables and figures
  no_calib_msg_r <- reactive({
    se <- raw_se_r()
    has_q <- any(util$is_targeted_feature(se))
    has_cc <- any(util$ctrl_smpl_cat(se) == "CalCurve")
    missing <- c(if (!has_q) "targeted (Quant) features", if (!has_cc) "calibration curve (CalCurve) samples")
    paste0("No calibration for this dataset: it has no ", paste(missing, collapse = " and no "),
           ", so it was processed without calibration (non-target mode). The normalized data are on",
           " pages 2 and 3 and in the exported tables.")
  })
  # What the read data allows, for the processing options (greyed out when not feasible) and the
  # popup when "Process data" is clicked; `NULL` before a read
  data_caps_r <- reactive({
    se <- tryCatch(raw_se_r(), error = function(e) NULL)
    if (is.null(se)) return(NULL)
    cd <- SumExp::col_df(se)
    cc <- util$ctrl_smpl_cat(se) == "CalCurve"
    conc <- util$spiked_conc_pts(se)   # NULL in non-target mode
    b <- as.character(cd$batch_id)
    # Per batch with calibration samples, and all batches together (the global curve)
    check <- function(keep) {
      x <- if (is.null(conc)) numeric(0) else conc[keep]
      n_pt <- table(x[x != 0])
      c(cal0 = sum(x == 0) >= 2, reps = length(n_pt) > 0 && all(n_pt >= 2))
    }
    batches <- unique(b[cc])
    per_batch <- matrix(vapply(batches, \(bb) check(cc & b == bb), logical(2)), nrow = 2,
                        dimnames = list(c("cal0", "reps"), batches))
    all_b <- check(cc)
    list(
      has_targeted = any(util$is_targeted_feature(se)),
      has_calib = length(batches) > 0 && !is.null(conc),   # Calibration samples with concentrations
      has_vis = any(util$std_type(se) %in% "vIS"),
      cal0_ok = any(per_batch["cal0", ]) || all_b[["cal0"]],
      cal0_fail_batches = batches[!per_batch["cal0", ]],
      reps_ok = any(per_batch["reps", ]) || all_b[["reps"]],
      reps_fail_batches = batches[!per_batch["reps", ]],
      multi_batch = length(batches) > 1,
      comment_notes = SumExp::metadata(se)$comment_notes,   # NULL for data read by older versions
      column_notes = SumExp::metadata(se)$column_notes,     # Columns similar to a required one
      missing_values = SumExp::metadata(se)$missing_values
    )
  })
  # Stops a calibration table or figure with that message in non-target mode
  need_calibration <- function() shiny::validate(shiny::need(is_target_mode(), no_calib_msg_r()))
  MSG_SELECT_CHEMICAL <- "Select a chemical in table 4.2 (left) to show it here."
  MSG_SELECT_IS <- "Select an internal standard in table 3.1 (left) to show it here."

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
      # Every class of every category; the calibration samples as one total (their levels are in 2.2)
      dplyr::mutate(Class = ifelse(Category == "CalCurve", "All levels", Class)) |>
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
      selection = "none",   # Clicking a row has no effect
      options = list(
        scrollX = TRUE,
        lengthChange = FALSE,
        searching = FALSE,
        paging = FALSE, # remove Previous/Next
        scrollY = "400px", scrollCollapse = TRUE,   # Scrolls (shorter when it fits)
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
  ### This is table 2.2: calibration curve samples per concentration, per batch and in total (the
  ### same table as in the internal report)

  output$tb_2_1_2_2 <- DT::renderDataTable({
    se <- raw_se_r()   # Not inside an S4 method argument (see 4.4)
    df_conc_summary <- as.data.frame(show$calib_samples_per_batch(se), check.names = FALSE)
    shiny::validate(shiny::need(
      nrow(df_conc_summary) > 0,
      if (!any(util$ctrl_smpl_cat(se) == "CalCurve")) {
        "No calibration curve (CalCurve) samples in this dataset."
      } else {
        paste("The calibration curve samples have no concentrations: without targeted (Quant) features",
              "they are not read (non-target mode, no calibration).")
      }
    ))

    DT::datatable(
      df_conc_summary,
      selection = "none",   # Clicking a row has no effect
      options = list(
        scrollX = TRUE,
        lengthChange = FALSE,
        searching = FALSE,
        paging = FALSE, # remove Previous/Next
        scrollY = "400px", scrollCollapse = TRUE,   # Scrolls (shorter when it fits)
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
  ### This is table 2.3


  # A DataTable in the same format as 2.1 and 2.2
  output$tb_2_2 <- DT::renderDataTable({
    v <- util$std_type(raw_se_r())
    v <- replace(v, v == "", "Not targeted")
    tb <- janitor::tabyl(v) |> janitor::adorn_pct_formatting() |> as.data.frame()
    colnames(tb) <- c("Standard Type", "Number of Features", "Percent")
    DT::datatable(
      tb,
      selection = "none",   # Clicking a row has no effect
      options = list(
        scrollX = TRUE, lengthChange = FALSE, searching = FALSE, paging = FALSE, info = FALSE, dom = "t",
        scrollY = "400px", scrollCollapse = TRUE,
        headerCallback = JS(
          "function(thead){",
          "$(thead).find('th').css({'color': 'green', 'font-weight': 'bold'});", "}"
        ),
        columnDefs = list(list(className = "dt-center", targets = "_all"))
      )
    ) |>
      DT::formatStyle(columns = names(tb), color = "#000000", backgroundColor = "#FFFFFF", target = "cell") |>
      DT::formatStyle(names(tb), backgroundColor = "#F8FAFF", target = "row")
  })

  ################################################################################################################
  ### This is figure 2.4

  # m/z vs retention time map, the same as in the internal report. Before processing, features
  # are coloured by type; after processing, targeted compounds by their outcome (for the final
  # normalization method if chosen, otherwise the best one)
  output$ggplot1_2_2 <- plotly::renderPlotly({
    se <- shiny::req(raw_se_r())   # Not inside `row_df()`: S4 dispatch turns `req()` into an error
    rd <- SumExp::row_df(se)
    quiet <- function(expr) tryCatch(expr, error = function(e) NULL)   # Not processed (yet)
    tr <- quiet(to_report_r())
    calib_rows <- removed <- NULL
    m <- if (isTRUE(quiet(final_norm_chosen_r()))) quiet(final_norm_method_r()) else quiet(best_norm_r())
    if (!is.null(tr) && length(m) == 1) {
      blk <- util$mat_id_of_blank_subtracted(m)
      lst <- quiet(proc_r()[[blk]])
      calib_rows <- dplyr::bind_rows(lapply(Filter(\(x) !is.null(x$concn), lst), function(x) {
        tibble::as_tibble(SumExp::row_df(x$concn), rownames = "feature_id") |>
          dplyr::select(feature_id, to_export, has_proper_range)
      }))
      rem <- tr[["removed features"]]
      if (!is.null(rem)) removed <- rem[rem$mat_id == m & rem$targeted, ]
    }
    show$plotly_mz_rt_map(show$mz_rt_outcome(rd, calib_rows, removed), height = 520)
  })


  ################################################################################################################
  ### This is figure 2.5: number of features per m/z (bins of 0.5), the same as in the internal report

  output$ggplot2_2_2 <- plotly::renderPlotly({
    se <- shiny::req(raw_se_r())   # Not inside `row_df()`: S4 dispatch turns the message into an error
    df1 <- SumExp::row_df(se)
    show$plotly_binned_hist(df1$mz, binwidth = 0.5, x_title = show$label_if_has(df1$mz, "m/z"))
  })


  ################################################################################################################
  ### This is figure 2.6: all raw values, linear and log scale, the same as in the internal report

  output$ggplot2_3 <- plotly::renderPlotly({
    se <- shiny::req(raw_se_r())
    # Counted when the file was read (read-msdial.R); counted here for data read by older versions
    show$plotly_value_distribution(SumExp::metadata(se)$value_bins %||% se[["raw"]])
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
        .rt = round(.rt, 2) |> labelled::copy_labels_from(.rt),   # RT with 2 decimals
        mz = round(mz, 5) |> labelled::copy_labels_from(mz),       # m/z with 5 decimals
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
      # The first IS is shown at once; a click on the selected row keeps it (see the header script)
      selection = list(mode = "single", selected = 1L),
      rownames = FALSE,

      options = list(
        scrollX = TRUE,
        lengthChange = FALSE,
        searching = FALSE,
        paging = FALSE, # remove Previous/Next
        scrollY = "400px", scrollCollapse = TRUE,   # Scrolls (shorter when it fits)
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

  # Title of 3.5 with the name of the IS selected in table 3.1
  output$title_3_5 <- shiny::renderText({
    ii <- input$tb_3_1_rows_selected
    nm <- tryCatch(SumExp::row_df(internal_std_se_r())$feature_name[ii[1]], error = function(e) NULL)
    if (length(nm) == 1 && !is.na(nm)) paste("Distribution of intensities of", nm) else
      "Distribution of intensities of the selected IS feature"
  })

  # The IS selected in table 3.1: box plot per sample group and scatter plot along the injection
  # order, the same as in the internal report (all samples; outliers drawn as crosses)
  output$ggplot3_1_3 <- plotly::renderPlotly({
    se <- internal_std_se_r()
    shiny::validate(shiny::need(input$tb_3_1_rows_selected, MSG_SELECT_IS))   # Before a row is selected
    ii <- input$tb_3_1_rows_selected
    cd <- SumExp::col_df(se)
    smp <- tibble::as_tibble(cd) |> dplyr::mutate(group = show$sample_group(cd))
    is_out <- to_report_r()[["is outlier sample"]]
    show$plotly_is_distribution(
      y = unname(se[["raw"]][ii, ]),
      smp = smp,
      name = SumExp::row_df(se)$feature_name[ii],
      outlier = if (is.null(is_out)) rep(FALSE, ncol(se)) else unname(is_out[colnames(se)] %in% TRUE),
      failed = isTRUE(to_report_r()[["is failed internal std."]][[ii]]),
      height = 480
    )
  })


  ################################################################################################################
  ### This is text 3.2

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
        .rt = round(.rt, 2) |> labelled::copy_labels_from(.rt),   # RT with 2 decimals
        mz = round(mz, 5) |> labelled::copy_labels_from(mz),       # m/z with 5 decimals
        num_zeros = ifelse(num_zeros == 0, "", num_zeros) |>
          labelled::copy_labels_from(num_zeros)
      )

    df <- data.frame(df,
      check.names = FALSE
    )

    is_failed_istd <- to_report_r()[["is failed internal std."]]


    df$is_failed_istd <- is_failed_istd # ensure it's inside render

    # The failed IS (also coloured in 3.1), as in the internal report
    if (!any(is_failed_istd)) {
      return(shiny::div(class = "q-note-line", "No failed internal standards (none with zeros in more than 10% of the samples)."))
    }
    n <- ncol(internal_std_se_r())
    zeros <- count_zeros_per_feature(internal_std_se_r()[["raw"]])
    failed <- data.frame(
      `Internal standard` = df$feature_name, RT = df$.rt, `m/z` = df$mz,
      `Zero values` = paste0(zeros, " of ", n, " (", round(100 * zeros / n, 1), "%)"),
      check.names = FALSE
    )[is_failed_istd, , drop = FALSE]
    shiny::tagList(
      shiny::div(class = "q-note-line", sprintf("%d failed internal standard(s), removed before normalization:", nrow(failed))),
      DT::datatable(failed, rownames = FALSE, selection = "none",
                    options = list(dom = "t", paging = FALSE, scrollX = TRUE, scrollY = "300px",
                                   scrollCollapse = TRUE, searching = FALSE, info = FALSE))
    )
  })


  ################################################################################################################
  ### something for 3.3 outlier removal


  output$wrap_outlier <- renderUI({
    is_outlier <- to_report_r()[["is outlier sample"]]
    # The following is added only when the outlier removal step has been set to perform

    if (!is.null(is_outlier)) {
      # Number of outlying internal standard features per sample
      n_out_istd <- to_report_r()[["number of outlier internal std. per sample"]]

      if (all(!is_outlier)) {
        text_3_1_2 <- paste0("No outliers were identified.\n")

        output$text_3_1_2 <- renderUI({
          shiny::div(class = "q-note-line", text_3_1_2)
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
          shiny::div(class = "q-note-line", text_3_1_2)
        })

        output$tb_3_1_2 <- DT::renderDataTable({
          DT::datatable(
            show_tb,
            selection = "none",   # Clicking a row has no effect
            options = list(
              scrollX = TRUE,
              lengthChange = FALSE,
              searching = FALSE,
              paging = FALSE, # remove Previous/Next
              scrollY = "400px", scrollCollapse = TRUE,   # Scrolls (shorter when it fits)
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
  ### This is for table 3.4


  output$tb_3_4_1 <- DT::renderDataTable({
    # RSD% of each IS per QC group and sample class (prepared by process.R; computed here for data
    # processed by older versions)
    prep <- app_data_r()[["is rsd"]]
    if (is.null(prep)) prep <- show$is_rsd_by_group(shiny::req(internal_std_se_r()))
    m_rsd <- prep$rsd
    n_smpl <- prep$n

    shiny::validate(shiny::need(input$tb_3_1_rows_selected, MSG_SELECT_IS))
    selected <- input$tb_3_1_rows_selected

    m_rsd <- m_rsd[, selected, drop = FALSE]
    # A fixed column name (the IS is in the caption): a table drawn for another IS would otherwise
    # ask for a column that no longer exists when the selection changes quickly
    is_name <- colnames(m_rsd)[1]
    colnames(m_rsd) <- "RSD%"
    # Why a group has no RSD%: it needs at least two samples (or values that are not all 0)
    rsd_v <- m_rsd[, 1]
    note <- dplyr::case_when(
      n_smpl < 2 ~ paste0("Too few samples (n = ", n_smpl, "): RSD% needs at least 2"),
      !is.finite(rsd_v) ~ "Not computable: all values 0 or missing",
      TRUE ~ ""
    )
    m_rsd <- data.frame(m_rsd, n = n_smpl, check.names = FALSE)
    if (any(nzchar(note))) m_rsd$Note <- note

    DT::datatable(
      m_rsd,
      selection = "none",   # Clicking a row has no effect
      caption = is_name,
      options = list(
        scrollX = TRUE,
        lengthChange = FALSE,
        searching = FALSE,
        paging = FALSE, # remove Previous/Next
        scrollY = "400px", scrollCollapse = TRUE,   # Scrolls (shorter when it fits)
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
  ### This is for Figure 3.6: one tab per volumetric internal standard (vIS), the same
  ### plot as in the internal report (raw peak areas of all samples)

  output$vis_or_text_3_2_1_1 <- renderUI({
    se <- shiny::req(raw_se_r())
    vis <- which(util$std_type(se) %in% "vIS")
    if (length(vis) == 0) {
      return(HTML("<h4 style='color:orange;'>No volumetric internal standards found.</h4>"))
    }
    cd <- SumExp::col_df(se)
    smp <- tibble::as_tibble(cd) |> dplyr::mutate(group = show$sample_group(cd))
    # The plots without the calibration samples unless asked for (the RSD% table has all samples)
    keep <- isTRUE(input$vis_show_calibrants) | cd$.ctrl_cat != "CalCurve"
    plots <- lapply(vis, function(i) {
      nm <- SumExp::row_df(se)$feature_name[i]
      list(name = nm, plot = show$plotly_vis_bars(unname(se[["raw"]][i, keep]), smp[keep, ], nm,
                                                  colour_by = input$vis_colour %||% "group"))
    })
    # RSD% of the vIS within groups of samples, as in the internal report
    rsd <- show$vis_rsd_table(se[["raw"]][vis, , drop = FALSE], SumExp::row_df(se)$feature_name[vis], cd) |>
      dplyr::mutate(Grouping = factor(Grouping), Group = factor(Group))
    rsd_tbl <- DT::datatable(rsd, rownames = FALSE, filter = "top", selection = "none",
                             options = list(paging = FALSE, scrollY = "400px", scrollCollapse = TRUE, scrollX = TRUE,
                                            dom = "rti"))   # Column filters only; scrolls
    # (Before processing there is nothing to report)
    not_corrected <- tryCatch(to_report_r()[["samples without vIS value"]], error = function(e) NULL)
    shiny::tagList(
      if (length(plots) == 1) plots[[1]]$plot else
        do.call(shiny::tabsetPanel, lapply(plots, \(x) shiny::tabPanel(x$name, x$plot))),
      shiny::h5("RSD% of the volumetric internal standards", style = "margin-top: 1.2em;"),
      shiny::helpText("Raw peak areas, all samples (also outliers), within groups of samples."),
      rsd_tbl,
      # The samples the vIS pre-normalization left uncorrected (no vIS value: 0 or missing)
      lapply(names(not_corrected), \(nm)
        shiny::helpText(paste0("Not corrected by ", nm, " (no vIS value): ",
                               paste(not_corrected[[nm]], collapse = ", "), "."),
                        style = "color: #b35900;"))
    )
  })


  ################################################################################################################
  ### Page 1, log card, "Processing history": what happened to the features and samples (the event
  ### log of the processing, projlib/history.R), the full tables in a popup

  history_tr <- reactive(tryCatch(to_report_r(), error = function(e) NULL))

  output$history_ui <- renderUI({
    tr <- history_tr()
    if (is.null(tr) || is.null(tr[["feature history"]])) {
      return(helpText("Available after the processing: what each step did with the features and samples,",
                      "where a fallback was needed and what was removed."))
    }
    versions <- names(tr[["feature history"]])
    choices <- stats::setNames(versions, dplyr::coalesce(unname(history$version_labels[versions]), versions))
    tagList(
      selectInput("history_version", "Data version", choices = choices,
                  selected = intersect(tr[["best normalization"]], versions)[1], width = "100%"),
      helpText("Where the features ended: from their type (left) to their status (right). Hover over",
               "a band for the number of features."),
      plotly::plotlyOutput("history_flow", height = "340px"),
      helpText("Fallbacks (the data did not allow the usual way) and removals, per step. The full tables",
               "are also exported with the tables (….history.xlsx)."),
      DT::dataTableOutput("history_summary"),
      div(class = "d-flex gap-2 mt-2",
          actionButton("history_features", "Feature history", icon = icon("table"), class = "btn-sm-quanta"),
          actionButton("history_samples", "Sample history", icon = icon("table"), class = "btn-sm-quanta"),
          actionButton("history_events", "All events", icon = icon("list"), class = "btn-sm-quanta"))
    )
  })

  # The events of the selected data version (and those of all versions)
  history_events <- reactive({
    tr <- req(history_tr())
    m <- req(input$history_version)
    ev <- tr[["events"]]
    ev[is.na(ev$mat_id) | ev$mat_id == m, ]
  })

  history_dt <- function(df, status_col = NULL, bad = character()) {
    out <- DT::datatable(df, rownames = FALSE, filter = "top", selection = "none",
                         options = list(paging = FALSE, scrollX = TRUE, scrollY = "60vh", scrollCollapse = TRUE,
                                        dom = "rti"))
    if (!is.null(status_col)) {
      out <- DT::formatStyle(out, status_col, backgroundColor = DT::styleEqual(bad, rep("#F8D0D0", length(bad))))
    }
    out
  }

  output$history_flow <- plotly::renderPlotly({
    fh <- req(history_tr())[["feature history"]][[req(input$history_version)]]
    history$plotly_feature_flow(req(fh), height = 340)
  })

  output$history_summary <- DT::renderDataTable({
    ev <- history_events()
    ev <- ev[ev$kind %in% c("removed", "fallback"), ]
    df <- ev |>
      dplyr::count(Step = step, Kind = kind, Event = event, name = "Count")
    DT::datatable(df, rownames = FALSE, selection = "none",
                  options = list(paging = FALSE, dom = "t", scrollY = "300px", scrollCollapse = TRUE,
                                 language = list(emptyTable = "Nothing removed, no fallback needed."))) |>
      DT::formatStyle("Kind", backgroundColor = DT::styleEqual(c("removed", "fallback"), c("#F8D0D0", "#FFE3B3")))
  })

  history_modal <- function(title, table_id) {
    showModal(modalDialog(title = title, size = "xl", easyClose = TRUE, footer = modalButton("Close"),
                          DT::dataTableOutput(table_id)))
  }
  observeEvent(input$history_features, history_modal(
    paste0("Feature history (", dplyr::coalesce(unname(history$version_labels[input$history_version]), input$history_version), ")"),
    "history_features_tbl"))
  observeEvent(input$history_samples, history_modal("Sample history", "history_samples_tbl"))
  observeEvent(input$history_events, history_modal(
    paste0("All events (", dplyr::coalesce(unname(history$version_labels[input$history_version]), input$history_version), ")"),
    "history_events_tbl"))

  output$history_features_tbl <- DT::renderDataTable({
    fh <- req(history_tr())[["feature history"]][[req(input$history_version)]]
    history_dt(dplyr::mutate(fh, dplyr::across(c(Type, Status), factor)), "Status",
               c("Removed (failed IS)", "Removed (blank filter)", "Removed (QC RSD% filter)", "Not quantified"))
  })
  output$history_samples_tbl <- DT::renderDataTable({
    sh <- req(history_tr())[["sample history"]]
    history_dt(dplyr::mutate(sh, dplyr::across(c(`Sample type`, Status, Batch), factor)), "Status", "Removed (outlier)")
  })
  output$history_events_tbl <- DT::renderDataTable({
    ev <- history_events()
    df <- dplyr::transmute(ev, Step = step, Kind = factor(kind), `Applies to` = factor(scope), Name = name,
                           Batch = batch_id, Event = factor(event), Detail = detail)
    history_dt(df, "Kind", "removed")
  })


  ################################################################################################################
  ### This is for Figure 3.7: two independent stacked bar plots of the internal standards,
  ### the same as the internal report's "Internal standards over the sequence"

  # Internal standards after the QC steps (failed IS and outlier samples removed)
  # (and the calibration samples unless asked for)
  is_bars_se_r <- shiny::reactive({
    norm_se <- to_report_r()[["normalized"]]
    keep <- isTRUE(input$isbar_show_calibrants) | util$ctrl_smpl_cat(norm_se) != "CalCurve"
    norm_se[util$is_internal_std(norm_se), keep]
  })
  # Values to choose from: raw and the normalization methods in the data
  shiny::observe({
    labs <- c(raw = "Raw", closest_norm = "Closest RT", loess_norm = "LOESS", mvis_norm = "Multivariate (PCA)")
    avail <- labs[names(labs) %in% names(is_bars_se_r())]
    choices <- stats::setNames(names(avail), avail)
    best <- tryCatch(best_norm_r(), error = function(e) NULL)
    defaults <- c(a = "raw", b = if (!is.null(best) && best %in% choices) best else choices[[min(2, length(choices))]])
    for (k in c("a", "b")) {
      cur <- shiny::isolate(input[[paste0("isbar_values_", k)]])
      shiny::updateSelectInput(session, paste0("isbar_values_", k), choices = choices,
                               selected = if (!is.null(cur) && cur %in% choices && cur != "raw") cur else defaults[[k]])
    }
  })
  for (k in c("a", "b")) {
    local({
      k <- k
      output[[paste0("ggplot3_2_1_2", k)]] <- plotly::renderPlotly({
        se <- is_bars_se_r()
        m <- shiny::req(input[[paste0("isbar_values_", k)]])
        shiny::req(m %in% names(se))
        labs <- c(raw = "Raw", closest_norm = "Closest RT", loess_norm = "LOESS", mvis_norm = "Multivariate (PCA)")
        show$plotly_is_bars(se, m, colour_by = shiny::req(input[[paste0("isbar_colour_", k)]]),
                            title = paste("Internal standards —", labs[[m]]), height = 480)
      })
    })
  }

  ################################################################################################################
  ### This is for Figure 3.11 (QC/QA): one feature in the blanks compared to the other sample groups,
  ### with the threshold of the blank filter. The data before blank filtering (all features).

  # The samples, features and methods of 3.11. Prepared by process.R (the values of a method are
  # in a file of their own, read when that method is shown); for data processed by older versions
  # from the normalized data
  blank_cmp_r <- shiny::reactive({
    prep <- app_data_r()[["blank"]]
    if (!is.null(prep)) {
      shiny::req(any(prep$col_df$.ctrl_cat == "Blank"))
      return(prep)
    }
    se <- to_report_r()[["normalized"]]
    shiny::req(se, any(util$ctrl_smpl_cat(se) == "Blank"))
    list(col_df = SumExp::col_df(se), row_df = SumExp::row_df(se), methods = names(se), se = se)
  })
  # Values (features x samples) of the method chosen in 3.11
  blank_values_r <- shiny::reactive({
    b <- blank_cmp_r()
    m <- shiny::req(input$blank_values)
    shiny::req(m %in% b$methods)
    if (!is.null(b$se)) return(b$se[[m]])
    f <- rds_file(paste0("blank_", m))
    shiny::validate(shiny::need(file.exists(f), MSG_NOT_PROCESSED))
    readRDS(f)
  })
  # Feature type and label of each feature: names, or "Unk_<m/z>_<RT>" for non-targeted features
  blank_features_r <- shiny::reactive({
    rd <- blank_cmp_r()$row_df
    std <- rd$.std_type
    type <- dplyr::case_when(
      std %in% "vIS" ~ "is",                 # Volumetric IS with the internal standards
      util$is_internal_std(std) ~ "is",
      util$is_targeted_feature(std) ~ "targeted",
      TRUE ~ "nontargeted"
    )
    # Internal standards (also the vIS) are not blank-filtered: not offered here (see 3.1, 3.5)
    tibble::tibble(feature_id = rownames(rd), type = type,
                   label = ifelse(type == "nontargeted", sprintf("Unk_%.4f_%.2f", rd$mz, rd$.rt),
                                  rd$feature_name))[type != "is", ]
  })
  # Number of features of the chosen kind removed by the blank filter (for the chosen values)
  output$blank_removed_count <- shiny::renderUI({
    f <- tryCatch(blank_features_r(), error = function(e) NULL)
    shiny::req(f, input$blank_values)
    p <- to_report_r()[["params"]]
    if (!isTRUE(p$blank_filter)) return(shiny::div(class = "q-note-line", "Blank filter not used in this processing."))
    kind <- input$blank_type
    in_kind <- if (kind == "all") rep(TRUE, nrow(f)) else f$type == kind
    n_rem <- sum(f$feature_id[in_kind] %in% blank_removed_r())
    what <- switch(kind, targeted = "targeted compounds", nontargeted = "non-targeted features", "features")
    labs <- c(raw = "raw", closest_norm = "Closest RT", loess_norm = "LOESS", mvis_norm = "Multivariate (PCA)")
    shiny::div(class = "q-note-line",
               sprintf("%d of %d %s removed by the blank filter (%s values).", n_rem, sum(in_kind), what,
                       labs[[input$blank_values]] %||% input$blank_values))
  })
  # Features removed by the blank filter for the chosen values (matrix)
  blank_removed_r <- shiny::reactive({
    rem <- to_report_r()[["removed features"]]
    m <- shiny::req(input$blank_values)
    if (is.null(rem)) return(character(0))
    rem$feature_id[rem$mat_id == m & rem$reason == "Blank filter"]
  })
  # Values to choose from: raw and the normalization methods in the data
  shiny::observe({
    labs <- c(raw = "Raw", closest_norm = "Closest RT", loess_norm = "LOESS", mvis_norm = "Multivariate (PCA)")
    avail <- labs[names(labs) %in% blank_cmp_r()$methods]
    cur <- shiny::isolate(input$blank_values)
    shiny::updateSelectInput(session, "blank_values", choices = stats::setNames(names(avail), avail),
                             selected = if (!is.null(cur) && cur %in% names(avail)) cur else "raw")
  })
  # Features to choose from (server-side search in the dropdown)
  shiny::observe({
    f <- blank_features_r()
    keep <- if (input$blank_type == "all") TRUE else f$type == input$blank_type
    if (isTRUE(input$blank_removed_only)) keep <- keep & f$feature_id %in% blank_removed_r()
    f <- f[keep, ]
    f <- f[order(f$label), ]
    if (nrow(f) == 0) {   # E.g. no non-targeted features in the data: say so in the box
      shiny::updateSelectizeInput(session, "blank_feature", choices = character(0), selected = character(0),
                                  options = list(placeholder = "No features of this kind"))
      return()
    }
    cur <- shiny::isolate(input$blank_feature)
    shiny::updateSelectizeInput(session, "blank_feature", server = TRUE,
                                choices = stats::setNames(f$feature_id, f$label),
                                selected = if (!is.null(cur) && cur %in% f$feature_id) cur else f$feature_id[1],
                                options = list(placeholder = "Type to search…"))
  })
  output$ggplot3_3 <- plotly::renderPlotly({
    b <- blank_cmp_r()
    shiny::validate(shiny::need(isTruthy(input$blank_feature), "No features of this kind in the data."))
    fid <- input$blank_feature
    m <- shiny::req(input$blank_values)
    vals <- blank_values_r()
    shiny::req(fid %in% rownames(vals))
    cd <- b$col_df
    keep <- isTRUE(input$blank_show_calibrants) | cd$.ctrl_cat != "CalCurve"
    smp <- tibble::as_tibble(cd[keep, ]) |> dplyr::mutate(group = show$sample_group(cd[keep, ]))
    p <- to_report_r()[["params"]]
    f <- blank_features_r()
    removed <- fid %in% blank_removed_r()
    subtitle <- if (!isTRUE(p$blank_filter)) {
      "Blank filter not used in this processing"
    } else if (removed) {
      "<span style='color:#c23636'><b>Removed by the blank filter</b></span>"
    } else {
      "<span style='color:#2E9E5B'><b>Kept by the blank filter</b></span>"
    }
    labs <- c(raw = "Raw peak area", closest_norm = "Closest RT normalized",
              loess_norm = "LOESS normalized", mvis_norm = "Multivariate (PCA) normalized")
    show$plotly_blank_comparison(
      y = unname(vals[fid, keep]), smp = smp, name = f$label[match(fid, f$feature_id)],
      factor = p$blank_filter_factor %||% 5, view = input$blank_view, subtitle = subtitle,
      y_title = labs[[m]],
      # The Linear / Log choice of the user is kept for the next feature, method or view
      log = isTRUE(shiny::isolate(input$blank_log))
    ) |>
      htmlwidgets::onRender("function (el) {
        el.on('plotly_buttonclicked', function (d) {
          Shiny.setInputValue('blank_log', d.button.label === 'Log');
        });
      }")
  })

  ################################################################################################################
  ### This is for figure 3.8

  # RSD% of the targeted compounds per QC group and method (3.8, 3.9): prepared by process.R,
  # computed here for data processed by older versions
  qc_rsd_r <- shiny::reactive({
    prep <- app_data_r()[["qc rsd"]]
    if (!is.null(prep)) prep else show$qc_rsd_by_method(to_report_r()[["normalized"]])
  })


  # One line per targeted chemical and QC class (panels): its RSD% among the QC replicates under
  # each method (x-axis), colored by the normalization method with the lowest RSD%. Interactive:
  # hover shows the chemical and all its RSD% values, the legend filters, drag to zoom.
  output$ggplot3_2_3 <- plotly::renderPlotly({
    norm_se <- to_report_r()[["normalized"]]
    shiny::validate(shiny::need(show$has_enough_qc(norm_se), show$not_enough_qc_msg))
    # The comparison uses the targeted compounds
    shiny::validate(shiny::need(any(util$is_targeted_feature(norm_se)), "No targeted (Quant) compounds in this dataset: this comparison uses the targeted compounds."))
    prep <- qc_rsd_r()
    labs <- prep$labs
    df_to_show <- prep$df |>
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
  ### This is for table 3.9


  output$tb_3_2_3 <- DT::renderDataTable({
    norm_se <- to_report_r()[["normalized"]]
    shiny::validate(shiny::need(show$has_enough_qc(norm_se), show$not_enough_qc_msg))
    # The comparison uses the targeted compounds
    shiny::validate(shiny::need(any(util$is_targeted_feature(norm_se)), "No targeted (Quant) compounds in this dataset: this comparison uses the targeted compounds."))
    prep <- qc_rsd_r()
    labs <- prep$labs
    df_to_show <- prep$df

    df_to_show <- df_to_show |>
      dplyr::left_join(feature_id_name_tbl(), by = "feature_id") |> # Add feature names
      dplyr::select(QC, feature_name, dplyr::all_of(unname(labs))) |>
      dplyr::rename(all_of(labs), `Feature` = feature_name)

    df_to_show <- data.frame(df_to_show,
      check.names = FALSE
    )
    df_to_show$QC <- factor(df_to_show$QC)   # A dropdown filter of the QC groups
    # Scrolls within a box; filters on the QC group and the feature (search) columns only
    DT::datatable(
      df_to_show,
      selection = "none",   # Clicking a row has no effect
      rownames = FALSE,
      filter = "top",
      extensions = "Scroller",   # Only the rows in view are fetched and drawn (many rows)
      options = list(
        scrollX = TRUE,
        scrollY = "400px",
        scrollCollapse = TRUE,
        deferRender = TRUE,
        scroller = TRUE,
        lengthChange = FALSE,
        info = FALSE, # remove "Showing X to Y of Z entries"
        dom = "t",
        headerCallback = JS(
          "function(thead){",
          "$(thead).find('th').css({'color': 'green',
                                            'font-weight': 'bold'});", "}"
        ),
        columnDefs = list(
          list(className = "dt-center", targets = "_all"), # <--- centers all text
          list(searchable = FALSE, targets = seq_len(ncol(df_to_show) - 2) + 1)   # Values: no filter
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
  ### This is for table 3.10

  # ---- Removed features (3.10): blank filtering and QC RSD% filter ----
  shiny::observeEvent(to_report_r(), {
    ids <- available_norm_r()
    current <- shiny::isolate(input$removed_method)
    shiny::updateSelectInput(
      session, "removed_method",
      choices = NORM_METHOD_LABELS[NORM_METHOD_LABELS %in% ids],
      selected = if (!is.null(current) && current %in% ids) current else best_norm_r()
    )
  })

  # Explanation of the filters; red when the QC RSD% filter was skipped
  output$removed_note <- shiny::renderUI({
    # Nothing before processing: the table under it already says why (one message is enough)
    tr <- shiny::req(tryCatch(to_report_r(), error = function(e) NULL))
    rf <- tr[["qc rsd filter"]]
    blank_txt <- if (isTRUE(tr[["blank_filter"]])) {
      paste0("Blank filter: a feature is removed unless its maximum or average signal in the ",
             "samples is > ", tr[["blank_filter_factor"]], " × the average blank signal.")
    } else {
      "Blank filter: not used."
    }
    rsd_txt <- if (is.null(rf) || !isTRUE(rf$enabled)) {
      "QC RSD% filter: not used."
    } else {
      limits <- c(
        if (!is.na(rf$max_targeted)) paste0("targeted > ", rf$max_targeted, "%"),
        if (!is.na(rf$max_nontargeted)) paste0("non-targeted > ", rf$max_nontargeted, "%")
      )
      paste0("QC RSD% filter (after normalization and blank subtraction): removed if ",
             paste(limits, collapse = ", "), ".")
    }
    shiny::tagList(
      shiny::tags$div(style = "color: #333;", blank_txt, shiny::br(), rsd_txt),
      if (isTRUE(rf$skipped)) {
        shiny::tags$div(style = "color: #C62828; font-weight: 600;",
                        paste("The QC RSD% filter was requested but skipped.", rf$skip_reason))
      }
    )
  })

  output$tb_3_2_4 <- DT::renderDataTable({
    tr <- to_report_r()
    rem <- tr[["removed features"]]
    shiny::validate(shiny::need(!is.null(rem),
                                "Not available for data processed by an older version; process again."))
    rf <- tr[["qc rsd filter"]]
    rem <- rem[rem$mat_id == shiny::req(input$removed_method), ]
    shiny::validate(shiny::need(nrow(rem) > 0, "No features were removed for this normalization method."))
    # The volumetric IS has its own type (it is not a targeted feature, but no unknown either)
    std <- SumExp::row_df(raw_se_r())$.std_type[match(rem$feature_id, rownames(raw_se_r()))]
    show_df <- data.frame(
      Feature = rem$feature_name,
      Type = dplyr::case_when(rem$targeted ~ "Targeted", std %in% "vIS" ~ "Volumetric IS", TRUE ~ "Non-targeted"),
      `Removed by` = ifelse(
        rem$reason == "QC RSD%",
        paste0("QC RSD% > ", ifelse(rem$targeted, rf$max_targeted, rf$max_nontargeted), "%"),
        paste0("Blank filter (≤ ", tr[["blank_filter_factor"]], "× blank)")
      ),
      `QC RSD%` = round(rem$qc_rsd, 1),
      check.names = FALSE
    )
    show_df <- show_df[order(show_df$Type != "Targeted", show_df$Feature), ]
    DT::datatable(
      show_df,
      selection = "none",   # Clicking a row has no effect
      rownames = FALSE,
      caption = paste0(nrow(show_df), " removed features (",
                       sum(show_df$Type == "Targeted"), " targeted, ",
                       sum(show_df$Type == "Non-targeted"), " non-targeted",
                       if (any(show_df$Type == "Volumetric IS")) ", the volumetric IS", ")"),
      # At most 300 px high; shorter with few removed features. Only the rows in view are fetched
      # and drawn (Scroller): with many features, thousands of rows would block the browser
      extensions = "Scroller",
      options = list(scrollX = TRUE, scrollY = "300px", scrollCollapse = TRUE, deferRender = TRUE,
                     scroller = TRUE, info = FALSE, dom = "t")
    )
  })

  # ---- Calibration and Quantification: batch selection ----
  # Calibrated data of the selected normalization method: batch ID -> list(calcurve, concn)
  calib_lst_proc_r <- shiny::reactive({
    need_calibration()
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
    shiny::validate(shiny::need(input$tb_4_2_rows_selected, MSG_SELECT_CHEMICAL))
    id <- rownames(calib_batch_proc_r()$calcurve)[input$tb_4_2_rows_selected[1]]
    shiny::validate(shiny::need(!is.na(id), MSG_SELECT_CHEMICAL))
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
    # Silent when there is nothing to show: the tables and figures say why
    shiny::req(tryCatch({ to_report_r(); is_target_mode() }, error = function(e) FALSE))
    batch <- input$calib_batch
    used <- calib_used_curve_r()
    # Why the batch uses the global curve (saved by process.R), e.g. no blank samples
    concn_se <- calib_batch_proc_r()$concn   # Evaluated before the S4 method (see 4.3)
    reason <- SumExp::metadata(concn_se)$calibration_note
    reason_tag <- if (isTRUE(used == "Global") && length(reason) == 1 && !is.na(reason)) {
      shiny::tags$div(style = "color: #333; font-style: italic;",
                      paste0("Reason for the global curve in batch ", batch, ": ", reason, "."))
    }
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
      return(shiny::tagList(
        shiny::tags$div(
          style = "color: #C62828; font-weight: 600;",
          paste0("Only batch ", batch, "'s calibration points are shown. The concentrations were ",
                 "calculated with the global calibration curve, fitted on the calibration points of ",
                 "all batches.")
        ),
        reason_tag
      ))
    }
    shiny::tagList(shiny::tags$div(style = "color: #333;", do.call(paste0, txt)), reason_tag)
  })

  output$tb_4_1 <- DT::renderDataTable({
    ###################################################
    ### There are two issues.
    ## (1) One is the reactivity of params_yml$norm_method
    ## (2) One is reactivity to the data

    to_report_r(); need_calibration()   # Why it is empty: not processed / no calibration
    if (is_target_mode()) {
      # Number of calibration curve samples per concentration in the selected batch
      se <- calib_batch_proc_r()$calcurve
      v <- util$spiked_conc_pts(se)
      v <- as.data.frame(table(v[!is.na(v)]), check.names = FALSE)
      colnames(v) <- c("Calibration curve samples", "Number of Samples")
      DT::datatable(
        v,
        selection = "none",   # Clicking a row has no effect
        options = list(
          scrollX = TRUE,
          lengthChange = FALSE,
          searching = FALSE,
          paging = FALSE, # remove Previous/Next
          scrollY = "400px", scrollCollapse = TRUE,   # Scrolls (shorter when it fits)
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
    to_report_r(); need_calibration()   # Why it is empty: not processed / no calibration

    if (is_target_mode()) {
      # --------------------------------------------------------
      # Prepare data of the selected batch. Rows are in the order of `rownames(calcurve)`, which
      # the figures use to find the selected chemical.
      se <- calib_batch_proc_r()$calcurve
      batch <- shiny::isolate(input$calib_batch)
      rd <- SumExp::row_df(se)
      limit_df <- rd |>
        dplyr::select(
          min_c_conc,
          max_c_conc,
          lod,
          lloq,
          dplyr::any_of("uloq"),   # Not in data processed by older versions
          has_proper_range,
          src_calcurve
        ) |>
        tibble::as_tibble(rownames = "feature_id") |>
        dplyr::mutate(
          batch = batch,
          is_kept = rd$kept_calcurve %in% TRUE
        )
      # Detected (signal at or above the LOD signal) / quantified (exported: a value at or above
      # the LLOQ) in any study sample of the batch
      concn <- calib_batch_proc_r()$concn
      rd_q <- SumExp::row_df(concn)[limit_df$feature_id, , drop = FALSE]
      study <- util$extract_ctrl_smpl_cat(concn, "")[limit_df$feature_id, ]
      sig <- study[[util$src_mat_id_for_conc(concn)]]
      if (isTRUE(to_report_r()[["params"]][["log_calibration"]])) sig <- log1p(sig)   # As the limits
      detected <- rowSums(sig >= rd_q$lod_signal, na.rm = TRUE) > 0
      limit_df$detection <- dplyr::case_when(
        !(rd_q$has_proper_range %in% TRUE) ~ "",   # No curve: the reason says why
        !detected ~ "Not detected in any study sample",
        !(rd_q$to_export %in% TRUE) ~ "Detected but not quantified (no study sample ≥ LLOQ; not exported)",
        TRUE ~ ""
      )
      # The notes as icons by category (why the curve is not valid, kept or replaced by the global
      # one, its linear range, the detection in the samples); the full note on hover
      limit_df$Notes <- show$calib_notes_html(util$calib_notes(rd, batch_view = TRUE, detection = limit_df$detection))
      # Select the same chemical as before the batch / normalization method changed
      prev_id <- shiny::isolate(calib_feature_id())   # NULL until a chemical has been selected
      # The first chemical when none was selected before (the figures show it at once)
      preselect <- if (is.null(prev_id)) 1L else match(prev_id, limit_df$feature_id)
      preselect <- if (is.na(preselect)) 1L else preselect

      # --------------------------------------------------------
      show_df <- limit_df |>
        dplyr::rename(min_c = min_c_conc, max_c = max_c_conc) |>
        dplyr::mutate(
          lod = ifelse(is.na(lod), "N.A.", as.character(lod)),
          min_c = dplyr::case_when(
            is.na(min_c) ~ "No valid pts.",
            # A curve kept although not valid can start below its LLOQ, or have none
            is_kept & is.na(lloq) ~ paste0("None (curve from ", min_c, ")"),
            is_kept & lloq != min_c ~ paste0(lloq, " (curve from ", min_c, ")"),
            TRUE ~ as.character(min_c)
          ),

          # ✅ flags for coloring
          is_bad = !has_proper_range,
          is_global = src_calcurve == "Global",
          # Calibration curve used for quantification: "Batch", "Global" or "None"
          calibration = dplyr::coalesce(src_calcurve, "None"),
          max_c = dplyr::case_when(
            max_c == -9 ~ "No valid max",
            max_c == 0 | is.na(max_c) ~ "",
            TRUE ~ as.character(max_c)
          ),
          # Empty for a quadratic curve without log calibration (no ULOQ)
          uloq = if ("uloq" %in% names(limit_df)) ifelse(is.na(uloq) | !has_proper_range, "", as.character(signif(uloq, 4))) else ""
        ) |>
        dplyr::left_join(feature_id_name_tbl(), by = "feature_id") |>
        dplyr::mutate(is_removed = FALSE) |>
        dplyr::select(
          "Batch" = batch,
          "Chemical Name" = feature_name,
          "Calibration" = calibration,
          "LOD" = lod,
          "Minimum conc. (LLOQ)" = min_c,
          "Maximum conc." = max_c,
          "ULOQ" = uloq,
          Notes,
          is_global,
          is_bad,
          is_removed,
          is_kept
        )

      # Targeted compounds removed before calibration (blank filter / QC RSD% filter) are listed
      # at the end (after the rows of the calibrated chemicals, which the figures index by row)
      rem <- to_report_r()[["removed features"]]
      if (!is.null(rem)) {
        rf <- to_report_r()[["qc rsd filter"]]
        rem <- rem[rem$mat_id == input$norm_method & rem$targeted, ]
        if (nrow(rem) > 0) {
          show_df <- dplyr::bind_rows(show_df, tibble::tibble(
            "Batch" = shiny::isolate(input$calib_batch),
            "Chemical Name" = rem$feature_name,
            "Calibration" = "None",
            "LOD" = "", "Minimum conc. (LLOQ)" = "", "Maximum conc." = "", "ULOQ" = "",
            Notes = show$calib_notes_html(tibble::tibble(removed = ifelse(
              rem$reason == "QC RSD%",
              paste0("Removed: QC RSD% ", round(rem$qc_rsd, 1), "% > ", rf$max_targeted, "%"),
              "Removed: blank filter"
            ))),
            is_global = FALSE, is_bad = FALSE, is_removed = TRUE, is_kept = FALSE
          ))
        }
      }

      show_df <- as.data.frame(show_df, check.names = FALSE)
      # Positions of the helper columns in the DataTable (0-based, no row name column)
      i_flag <- match(c("is_global", "is_bad", "is_removed", "is_kept"), names(show_df)) - 1L
      i_name <- match("Chemical Name", names(show_df)) - 1L

      # --------------------------------------------------------
      # All chemicals in a scrolling box (the selected one is scrolled into view, see the header
      # script), searchable by the chemical name only. The figures use the row index of the
      # selection in the full table (not affected by searching).
      DT::datatable(
        show_df,
        rownames = FALSE,
        escape = setdiff(names(show_df), "Notes"),   # The note icons are HTML
        caption = htmltools::HTML(show$calib_notes_legend_html()),
        selection = list(mode = "single", selected = preselect),
        options = list(
          scrollX = TRUE,
          paging = FALSE,
          scrollY = "420px",
          scrollCollapse = TRUE,
          searching = TRUE,
          info = TRUE,
          dom = "fti",
          language = list(search = "Search chemical:"),

          # Hide the helper columns; search in the chemical name only
          columnDefs = list(
            list(targets = i_flag, visible = FALSE),
            list(targets = setdiff(seq_along(show_df) - 1L, i_name), searchable = FALSE)
          ),

          # Row colors: red = no proper calibration range, orange = curve kept although not
          # valid, blue = global calibration curve, grey = removed before calibration
          rowCallback = JS(sprintf("
          function(row, data) {
            var is_global = data[%d], is_bad = data[%d], is_removed = data[%d], is_kept = data[%d];
            var color = is_removed ? '#D9D9D9' : (is_bad ? 'lightcoral' : (is_kept ? '#FFD9A0' : (is_global ? 'lightblue' : null)));
            if (color) {
              $('td', row).each(function() {
                this.style.setProperty('background-color', color, 'important');
                this.style.setProperty('color', 'black', 'important');
              });
            }
          }
        ", i_flag[1], i_flag[2], i_flag[3], i_flag[4]))
        )
      )
    }
  })


  # Colour of the title of 4.4 / 4.5 by the state of the curve, as the rows of table 4.2: red not
  # valid, orange kept although not valid, blue the global curve, grey valid
  calib_title_fill <- function(rd) {
    if (!isTRUE(rd$has_proper_range[1])) "lightcoral" else if (isTRUE(rd$kept_calcurve[1])) "#FFD9A0" else
      if (isTRUE(rd$src_calcurve[1] == "Global")) "lightblue" else "lightgray"
  }

  ################################################################################################################
  ### This is for Figure 4.4
  output$ggplot4_3 <- renderPlotly({
    to_report_r(); need_calibration()   # Why it is empty: not processed / no calibration
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
        # The ULOQ (none for a quadratic curve, or in data processed by older versions)
        dplyr::mutate(.uloq = if ("uloq" %in% names(calcurve_to_show_df)) dplyr::coalesce(uloq, Inf) else Inf) |>
        dplyr::mutate(
          c_type = dplyr::case_when(
            # The LOD level is also the LLOQ (the start of the curve): its own colour, as it
            # would otherwise be one of the points within the limits
            .cc_pt == lod & .cc_pt == lloq & .cc_pt >= min_c_conc & .cc_pt <= max_c_conc ~ "LOD = LLOQ",
            # In the curve but above the top of its linear range (curve not cut at the ULOQ)
            .cc_pt >= min_c_conc & .cc_pt <= max_c_conc & .cc_pt > .uloq ~ "Above the ULOQ",
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
      sel_row_df <- SumExp::row_df(calcurve_lst[[1]])[calib_selected_feature_r(), , drop = FALSE]

      ############################################
      # Drawn with plotly directly (converting a ggplot took seconds per chemical, in the app's
      # process shared by all users). The title is the name only; its colour tells the state of
      # the curve, as the rows of 4.2 (which give the reason).
      y <- input$norm_method
      show$plotly_calcurve_points(
        dfm, y = y, title = dfm$feature_name[1], title_fill = calib_title_fill(sel_row_df),
        log_scale = isTRUE(log_scale),
        x_title = show$label_if_has(dfm$.cc_pt, "Calibrant Concentration"),
        y_title = show$label_if_has(dfm[[y]], "Signal"),
        # The limits of the curve chosen in 4.2 (the batch's own, or the global one it uses)
        lod = sel_row_df$lod[1], lloq = sel_row_df$lloq[1],
        uloq = if (isTRUE(sel_row_df$has_proper_range)) sel_row_df$uloq[1] else NA
      )
    }
  })

  ################################################################################################################
  ### This is for Figure 4.5

  output$ggplot4_4_2 <- renderPlotly({
    to_report_r(); need_calibration()   # Why it is empty: not processed / no calibration
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

      # Why the curve is not valid, kept or replaced by the global one ("" if used as is), and
      # whether the full range is shown as no sample is quantifiable
      reason <- show$calib_issue_text(SumExp::row_df(se_selected))
      if (!isTRUE(SumExp::row_df(se_selected)$has_proper_range)) {


        ############################################
        # ✅ Output

        plotly::plotly_empty(type = "scatter", mode = "markers") |>
          plotly::layout(
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE),
            annotations = list(
              list(
                text = paste0("No valid calibration curve<br><span style='font-size:16px'>",
                              htmltools::htmlEscape(sub("^Not valid: ", "", reason)), "</span>"),
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
        # Drawn with plotly directly (as 4.4). The title is the name only, coloured by the state
        # of the curve (as in 4.4 and 4.2)
        show$plotly_calcurve_samples(
          se_selected, calib_se, MAT_ID_IN_CALIB, colors_of_classes(), log_scale = isTRUE(log_scale),
          title_fill = calib_title_fill(SumExp::row_df(se_selected))
        )
      }
    }
  })

  ################################################################################################################
  ### This is for Figure 4.6

  output$ggplot5_1 <- renderPlotly({
    to_report_r(); need_calibration()   # Why it is empty: not processed / no calibration

    if (shiny::req(is_target_mode())) {
      log_scale <- isTRUE(to_report_r()[["params"]][["log_calibration"]])

      # ------------------------------------------------------------
      # Prepare data of the selected batch and chemical (by feature ID, not by name)
      feature_id <- calib_selected_feature_r()
      concn_se <- calib_batch_proc_r()$concn   # Evaluated before the S4 method (see 4.3)
      shiny::req(feature_id %in% rownames(concn_se))
      df1 <- SumExp::as_tibble(concn_se[feature_id, ])   # The selected chemical only (fast)

      # ------------------------------------------------------------
      # No concentrations: a message instead of an empty plot
      # Values below the LOD / LLOQ without a value ("n.d." / "n.q."), when not replaced
      flag <- if ("conc_flag" %in% names(df1)) df1$conc_flag else NULL
      if (is.null(df1) || nrow(df1) == 0 || all(is.na(df1$conc))) {
        n_flag <- sum(!is.na(flag))
        plotly::plotly_empty(type = "scatter", mode = "markers") |>
          plotly::layout(
            xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
            annotations = list(list(text = if (n_flag > 0) {
                                      "All values below the LOD / LLOQ (n.d. / n.q.)"
                                    } else {
                                      "Not detected / quantified in samples"
                                    }, x = 0.5, y = 0.5,
                                    xref = "paper", yref = "paper", showarrow = FALSE,
                                    font = list(size = 24, color = "gray")))
          )
      } else {
        # ------------------------------------------------------------
        # Box per class; points of the values replaced by the LOD/LLOQ rules coloured as in
        # the exported tables. Hover for the sample; Linear/Log buttons
        hover <- paste0("<b>", df1$sample_name, "</b><br>", df1$Class, " · batch ", df1$batch_id,
                        " · inj. ", df1$injection_order, "<br>conc. ", show$num_txt(df1$conc),
                        " (", show$imputed_kind(df1$conc, df1$lloq), ")")
        show$plotly_conc_by_class(
          conc = df1$conc, lloq = df1$lloq, cats = as.character(df1$Class),
          cat_colors = colors_of_classes(), hover = hover, name = df1$feature_name[1],
          y_title = labelled::get_label_attribute(df1$conc) %||% "Concentration", log = log_scale,
          flag = flag
        )
      }
    }
  })
  ################################################################################################################
  ### This is for Figure 4.7: the selected chemical in the QC samples of all batches along the
  ### injection order (each batch with its own calibration), the same as in the internal report

  output$ggplot4_7 <- renderPlotly({
    to_report_r(); need_calibration()   # Why it is empty: not processed / no calibration
    feature_id <- calib_selected_feature_r()
    lst_proc <- calib_lst_proc_r()   # Evaluated before the S4 methods (see 4.4)
    d <- dplyr::bind_rows(lapply(Filter(\(x) !is.null(x$concn), lst_proc), function(x) {
      se <- util$extract_ctrl_smpl_cat(x$concn, "QC")
      if (!feature_id %in% rownames(se) || ncol(se) == 0) return(NULL)
      se <- se[rownames(se) == feature_id, ]
      flag <- if ("conc_flag" %in% names(se)) as.character(se[["conc_flag"]][1, ]) else NA_character_
      cd <- SumExp::col_df(se)
      tibble::tibble(sample_name = cd$sample_name, Class = as.character(cd$Class),
                     batch_id = as.character(cd$batch_id), injection_order = cd$injection_order,
                     conc = unname(se[["conc"]][1, ]), lloq = SumExp::row_df(se)$lloq, flag = flag,
                     name = SumExp::row_df(se)$feature_name,
                     unit = labelled::get_label_attribute(se[["conc"]]) %||% "Concentration")
    }))
    if (nrow(d) == 0 || all(is.na(d$conc) & is.na(d$flag))) {
      return(plotly::plotly_empty(type = "scatter", mode = "markers") |>
        plotly::layout(
          xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
          annotations = list(list(text = if (nrow(d) == 0) "No QC samples" else "No concentrations in the QC samples",
                                  x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE,
                                  font = list(size = 24, color = "gray")))
        ))
    }
    d$kind <- show$imputed_kind(d$conc, d$lloq)
    show$plotly_qc_conc(d, name = d$name[1], y_title = d$unit[1])
  })

  ################################################################################################################
  ### This is for table 4.3


  output$tb_4_5 <- DT::renderDataTable({
    to_report_r(); need_calibration()   # Why it is empty: not processed / no calibration
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
        selection = "none",   # Clicking a row has no effect
        options = list(
          scrollX = TRUE,
          lengthChange = FALSE,
          searching = FALSE,
          paging = FALSE, # remove Previous/Next
          scrollY = "400px", scrollCollapse = TRUE,   # Scrolls (shorter when it fits)
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
