# ------------------------------------------------------------------------------------------- #
# Help popup: a question-mark icon in the top-right corner of the navbar that opens a popup in
# the middle of the screen. The popup closes when clicking outside of it (or pressing Esc).
# ------------------------------------------------------------------------------------------- #

#' Content of the help popup
#'
#' >>> EDIT THE CONTENT OF THE HELP POPUP HERE <<<
#'
#' Any Shiny/HTML tags can be used, e.g. `shiny::h4()`, `shiny::p()`, `shiny::tags$ul()`.
#' Links: use `help_link()` below so that they open in a new browser tab and the app stays open.
#'
#' @param on_serve Whether the app runs on SciLifeLab Serve (shows the hosting acknowledgement)
#' @export
help_popup_content <- function(on_serve = FALSE) {
  ref <- function(text, doi) shiny::tags$li(text, " ", help_link(paste0("doi:", doi), paste0("https://doi.org/", doi)))
  shiny::tagList(
    shiny::h4("About QuaNTA"),
    shiny::p(
      "Version 0.1.0", br(),
      help_link("QuaNTA tutorial", "https://tutorial.com"), br(),
      help_link("QuaNTA github repo", "https://github.com/Exposomics-SciLifeLab/QuaNTA"), br(),
      help_link("QuaNTA example data download",
                "https://github.com/Exposomics-SciLifeLab/QuaNTA/tree/main/example_data"), br(),
      help_link("QuaNTA preprint", "https://www.preprint.com")
    ),
    if (on_serve) {
      # SciLifeLab Serve's suggested acknowledgement; the app is cited by its Serve DOI
      shiny::p(
        style = "font-size: .85em;",
        "Technical infrastructure for hosting the app 'QuaNTA' was provided by SciLifeLab Serve (",
        help_link("https://serve.scilifelab.se", "https://serve.scilifelab.se"),
        "; RRID:SCR_027820), a platform developed and supported by SciLifeLab Data Centre.",
        "To cite the app, use its DOI on SciLifeLab Serve rather than its address."
      )
    },
    # The methods QuaNTA uses (the same references as the reports, reports/bibliography.bib)
    shiny::tags$details(
      style = "font-size: .85em;",
      shiny::tags$summary("Methods and references"),
      shiny::tags$ul(
        ref("Input: MS-DIAL. Tsugawa et al. (2015), Nat Methods 12:523–526.", "10.1038/nmeth.3393"),
        ref("LOESS normalization: Cleveland & Devlin (1988), J Am Stat Assoc 83:596–610.",
            "10.1080/01621459.1988.10478639"),
        ref("Multivariate (PCA) normalization: Surowiec et al. (2017), Metabolomics 13:114.",
            "10.1007/s11306-017-1248-1"),
        ref("Relative log abundance plots: De Livera et al. (2012), Anal Chem 84:10768–10776.",
            "10.1021/ac302748b"),
        ref("QC samples, blank and QC RSD% filters: Broadhurst et al. (2018), Metabolomics 14:72.",
            "10.1007/s11306-018-1367-3"),
        shiny::tags$li("LOD/LLOQ (3× and 10× the blank signal; RSD% ≤ 20% at the LLOQ): ICH Q2(R2) (2023)",
                       "and ICH M10 (2022) guidelines."),
        ref("Weighted calibration (1/x, 1/x²): Almeida et al. (2002), J Chromatogr B 774:215–222.",
            "10.1016/S1570-0232(02)00244-1"),
        ref("Group comparison: Wilcoxon (1945); false discovery rate: Benjamini & Hochberg (1995), J R Stat Soc B 57:289–300.",
            "10.1111/j.2517-6161.1995.tb02031.x"),
        ref("Report layout: MeTaQuaC, Kuhring et al. (2020), Anal Chem 92:10241–10245.",
            "10.1021/acs.analchem.0c00136"),
        shiny::tags$li("QuaNTA rules (no literature reference): values with a signal below the LOD signal",
                       "are replaced by LLOQ/4, below the mean signal of the LLOQ level by LLOQ/2 (or,",
                       "optionally, reported as n.d. / n.q.), above it but below the LLOQ set to the LLOQ;",
                       "outlier samples have more than 20% of the internal",
                       "standards beyond 3 SD (log scale, of their non-zero values) or missing (zero).")
      )
    )
  )
}

#' A link that opens in a new browser tab
#'
#' @param text The text shown for the link.
#' @param url The address of the link.
#' @export
help_link <- function(text, url) {
  shiny::tags$a(text, href = url, target = "_blank", rel = "noopener noreferrer")
}

#' A Shiny UI module of the help icon, to be placed in the navbar with `bslib::nav_item()`
#'
#' @param uiId A string that identifies the UI module.
#' @export
helpPopupUI <- function(uiId) {
  ns <- shiny::NS(uiId)
  shiny::tagList(
    shiny::actionLink(
      inputId = ns("open"),
      label = shiny::tagList("About", shiny::tags$span(shiny::icon("circle-question"), style = "margin-left: 6px;")),
      title = "Help",
      style = "font-size: 1.5rem; padding: 0 12px;"
    ),
    shiny::tags$style(shiny::HTML("
      /* Vertically center the popup (Bootstrap's `modal-dialog-centered`) */
      #shiny-modal .modal-dialog {
        display: flex;
        align-items: center;
        min-height: calc(100% - 3.5rem);
      }
      /* The About popup: ~40% of the window width (at least 360px, at most 90% on small screens) */
      #shiny-modal .modal-dialog:has(.quanta-help) {
        max-width: none;
        width: max(40vw, min(360px, 90vw));
      }
      /* Text and links scale with the window width */
      .quanta-help {
        font-size: clamp(1rem, 1.15vw, 1.5rem);
        line-height: 1.6;
        padding: 0.5em 0.75em;
      }
      .quanta-help h4 {
        font-size: 1.6em;
        margin-bottom: 0.6em;
      }
    "))
  )
}

#' A Shiny server module of the help popup
#'
#' @param serverId A string that identifies the server module. It should match the UI module's ID.
#' @export
helpPopupServer <- function(serverId) {
  shiny::moduleServer(serverId, function(input, output, session) {
    shiny::observeEvent(input$open, {
      shiny::showModal(shiny::modalDialog(
        shiny::div(class = "quanta-help", help_popup_content(on_serve = is_on_serve(session))),   # Styled in helpPopupUI()
        title = NULL,
        footer = NULL,
        size = "m",
        easyClose = TRUE    # Close by clicking outside of the popup or pressing Esc
      ))
    })
  })
}
