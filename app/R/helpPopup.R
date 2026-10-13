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
#' @export
help_popup_content <- function() {
  shiny::tagList(
    shiny::h4("About QuaNTA"),
    shiny::p(
      "Version 0.1.0", br(),
      help_link("QuaNTA tutorial", "https://tutorial.com"), br(),
      help_link("QuaNTA github repo", "https://github.com/Exposomics-SciLifeLab/QuaNTA"), br(),
      help_link("QuaNTA example data download",
                "https://github.com/Exposomics-SciLifeLab/QuaNTA/tree/main/example_data"), br(),
      help_link("QuaNTA preprint", "https://www.preprint.com")
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
        shiny::div(class = "quanta-help", help_popup_content()),   # Styled in helpPopupUI()
        title = NULL,
        footer = NULL,
        size = "m",
        easyClose = TRUE    # Close by clicking outside of the popup or pressing Esc
      ))
    })
  })
}
