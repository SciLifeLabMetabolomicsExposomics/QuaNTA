# ------------------------------------------------------------------------------------------- #
# GDPR notice before uploading data on SciLifeLab Serve
#
# On Serve (see R/uploadLimit.R for how that is detected), choosing an MS-DIAL file first shows a
# popup about personal data under the GDPR. The file dialog opens only
# after "Yes". Run locally, nothing is shown: the data then stay on the user's own computer.
# Everything happens in the browser: the popup cannot be skipped by a slow server round trip.
# ------------------------------------------------------------------------------------------- #

# The file inputs with data (module namespace of app.R: msdialInfoUI("data_info"))
.gdpr_guarded_inputs <- "data_info-input_file_upload"

#' Content of the GDPR popup
#'
#' >>> EDIT THE TEXT OF THE GDPR POPUP HERE <<<
#'
#' @returns A character string of HTML (the body of the popup; the question "Do you want to
#'   proceed?" and the Yes / No buttons are added below it)
#' @export
gdpr_notice_html <- function() {
  as.character(shiny::tagList(
    shiny::p(shiny::strong("This app runs on a web server (SciLifeLab Serve)."),
             "Do not upload personal data that is sensitive under the EU General Data Protection",
             "Regulation (GDPR)."),
    shiny::tags$ul(
      shiny::tags$li("Measurements of human samples (e.g. blood, plasma, urine or tissue) that can be",
                     "linked to a person are personal data, and usually", shiny::strong("data concerning health"),
                     "— a special category of personal data (GDPR Art. 9)."),
      shiny::tags$li(shiny::strong("Pseudonymised data are still personal data"), "(GDPR Recital 26): e.g. samples",
                     "with codes or IDs while a key to the persons exists, even if it is kept elsewhere."),
      shiny::tags$li("Only anonymised data, where the persons cannot be identified by any means reasonably",
                     "likely to be used, are outside the GDPR.")
    ),
    shiny::p("Upload only data you are allowed to process on this server, e.g. anonymised, non-human or",
             "synthetic data such as the QuaNTA example dataset. Otherwise, run QuaNTA on your own computer,",
             "where the data do not leave it. If in doubt, ask your data protection officer.")
  ))
}

#' JavaScript of the GDPR popup (only active on SciLifeLab Serve)
#'
#' A capture listener on the document stops the click (and drag-and-drop) on the guarded file
#' inputs before the browser opens its file dialog, and shows a Bootstrap modal. "Yes" opens the
#' file dialog (a click within the user's own click on "Yes", so browsers allow it); "No" closes
#' the popup.
#' @export
gdpr_notice_script <- function() {
  shiny::tags$script(shiny::HTML(sprintf("
    (function() {
      if (!new RegExp(%s, 'i').test(window.location.hostname)) return;
      var GUARDED = %s, BODY = %s, allowNext = null, modal = null, pending = null;

      // The guarded file input of an event target: the input itself or its button (`within`:
      // '.btn-file, .input-group-btn' for clicks; the whole input group for dropped files)
      function guardedInput(target, within) {
        if (!target || !target.closest) return null;
        var box = target.matches('input[type=file]') ? null : target.closest(within);
        var inp = box ? box.querySelector('input[type=file]') : (target.matches('input[type=file]') ? target : null);
        return inp && GUARDED.indexOf(inp.id) >= 0 ? inp : null;
      }
      function ensureModal() {
        if (modal) return modal;
        var el = document.createElement('div');
        el.className = 'modal fade'; el.id = 'quanta-gdpr'; el.tabIndex = -1;
        el.setAttribute('data-bs-backdrop', 'static'); el.setAttribute('data-bs-keyboard', 'false');
        el.innerHTML =
          '<div class=\"modal-dialog modal-dialog-centered modal-lg\"><div class=\"modal-content\">' +
          '<div class=\"modal-header\"><h4 class=\"modal-title\">Personal data and the GDPR</h4></div>' +
          '<div class=\"modal-body\">' + BODY + '</div>' +
          '<div class=\"modal-footer\" style=\"justify-content: space-between;\">' +
          '<strong>Do you want to proceed?</strong><span>' +
          '<button type=\"button\" class=\"btn btn-secondary\" id=\"quanta-gdpr-no\">No</button> ' +
          '<button type=\"button\" class=\"btn btn-primary\" id=\"quanta-gdpr-yes\">Yes</button></span></div>' +
          '</div></div>';
        document.body.appendChild(el);
        modal = new bootstrap.Modal(el);
        el.querySelector('#quanta-gdpr-no').addEventListener('click', function() { pending = null; modal.hide(); });
        el.querySelector('#quanta-gdpr-yes').addEventListener('click', function() {
          var inp = pending; pending = null; modal.hide();
          if (inp) { allowNext = inp; inp.click(); }   // Opens the file dialog
        });
        return modal;
      }
      function ask(e, inp) {
        e.preventDefault(); e.stopPropagation(); e.stopImmediatePropagation();
        pending = inp; ensureModal().show();
      }
      document.addEventListener('click', function(e) {
        var inp = guardedInput(e.target, '.btn-file, .input-group-btn');
        if (!inp) return;
        if (allowNext === inp) { allowNext = null; return; }   // The click from \"Yes\"
        ask(e, inp);
      }, true);
      // Files dropped on the input's text field: ask too, then open the file dialog
      document.addEventListener('drop', function(e) {
        var inp = guardedInput(e.target, '.input-group');
        if (inp) ask(e, inp);
      }, true);
    })();
  ", jsonlite::toJSON(.serve_host_regex, auto_unbox = TRUE),
     jsonlite::toJSON(.gdpr_guarded_inputs),
     jsonlite::toJSON(gdpr_notice_html(), auto_unbox = TRUE))))
}
