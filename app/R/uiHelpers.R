# ------------------------------------------------------------------------------------------- #
# Small UI building blocks of the QuaNTA design (styles in www/quanta.css)
# ------------------------------------------------------------------------------------------- #

#' Card header: an icon, a title and an optional subtitle; optional elements on the right
#'
#' @param title The card title.
#' @param icon_name A Font Awesome icon name, e.g. "folder-open".
#' @param subtitle An optional short description below the title.
#' @param actions Optional UI on the right of the header, e.g. a button.
#' @param below Optional UI in a row of its own under the title and subtitle (part of the header:
#'   it stays visible while the card body scrolls), e.g. a button.
#' @export
quanta_card_header <- function(title, icon_name = NULL, subtitle = NULL, actions = NULL, below = NULL) {
  bslib::card_header(
    shiny::div(
      class = "q-card-head",
      if (!is.null(icon_name)) shiny::span(class = "q-card-icon", shiny::icon(icon_name)),
      shiny::div(
        class = "q-card-titles",
        shiny::h2(class = "q-card-title", title),
        if (!is.null(subtitle)) shiny::p(class = "q-card-sub", subtitle)
      ),
      if (!is.null(actions)) shiny::div(class = "q-card-actions", actions)
    ),
    if (!is.null(below)) shiny::div(class = "q-card-below", below)
  )
}

#' A group of settings with a light-blue heading (e.g. "Quality control")
#'
#' @param title The heading.
#' @param icon_name A Font Awesome icon name.
#' @param ... The settings.
#' @export
settings_group <- function(title, icon_name, ...) {
  # One element: bslib's card body puts a gap between its children
  shiny::div(
    class = "q-group",
    shiny::div(class = "q-group-head", shiny::icon(icon_name), title),
    shiny::div(class = "q-group-body", ...)
  )
}

#' A small heading of settings on one theme within a [settings_group()] (style `.q-subhead`)
#'
#' @param title The heading.
#' @export
settings_subhead <- function(title) shiny::div(class = "q-subhead", title)

#' A short note under a setting, e.g. why it is greyed out (style `.q-hint`)
#'
#' @param text The note.
#' @export
settings_hint <- function(text) shiny::div(class = "q-hint", shiny::icon("circle-info"), text)

#' A setting with its suboptions in a lightly tinted box (style `.q-option-group`)
#'
#' @param ... The setting (e.g. a checkbox) and its suboptions (e.g. a `conditionalPanel()`).
#' @export
option_group <- function(...) shiny::div(class = "q-option-group", ...)

#' An info icon that shows a short explanation on hover
#'
#' @param text The explanation.
#' @export
info_tip <- function(text) {
  # 20 px from the icon (Bootstrap: 6), so that the mouse pointer (a question mark) does not cover the text
  bslib::tooltip(shiny::span(class = "q-info", shiny::icon("circle-info")), text, placement = "right",
                 options = list(offset = c(0, 20)))
}

#' A label followed by an info icon
#'
#' @param label The label text (or tags).
#' @param tip The explanation shown on hover.
#' @export
label_tip <- function(label, tip) shiny::tagList(label, info_tip(tip))

#' Title of a step in the navigation bar: a numbered circle and a label
#'
#' @param n The step number.
#' @param label The label; a line break can be given as `"\n"`.
#' @param last Whether this is the last step (no connector line after it).
#' @export
quanta_step <- function(n, label, last = FALSE) {
  parts <- strsplit(label, "\n", fixed = TRUE)[[1]]
  lab <- list()
  for (i in seq_along(parts)) {
    lab <- c(lab, list(parts[i]), if (i < length(parts)) list(shiny::br()))
  }
  lab <- do.call(shiny::tagList, lab)
  shiny::span(
    class = paste("q-step", if (last) "q-step-last"),
    shiny::span(class = "q-step-num", n),
    shiny::span(class = "q-step-label", lab)
  )
}
