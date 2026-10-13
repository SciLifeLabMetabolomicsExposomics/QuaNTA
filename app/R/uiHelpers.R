# ------------------------------------------------------------------------------------------- #
# Small UI building blocks of the QuaNTA design (styles in www/quanta.css)
# ------------------------------------------------------------------------------------------- #

#' Card header: an icon, a title and an optional subtitle; optional elements on the right
#'
#' @param title The card title.
#' @param icon_name A Font Awesome icon name, e.g. "folder-open".
#' @param subtitle An optional short description below the title.
#' @param actions Optional UI on the right of the header, e.g. a button.
#' @export
quanta_card_header <- function(title, icon_name = NULL, subtitle = NULL, actions = NULL) {
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
    )
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

#' An info icon that shows a short explanation on hover
#'
#' @param text The explanation.
#' @export
info_tip <- function(text) {
  bslib::tooltip(shiny::span(class = "q-info", shiny::icon("circle-info")), text, placement = "right")
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
