// ------------------------------------------------------------------------------------------- //
// Two helpers for all tables and figures of the app:
//
// 1. The eye button next to the title of every table and figure (`tip_title()` in app.R): a click
//    hides everything under the title up to the next title (only the title stays), a second click
//    shows it again. Open eye: shown; closed eye: hidden.
// 2. Tables with a scroll bar can be made taller or shorter by dragging the bar under them.
// ------------------------------------------------------------------------------------------- //
(function () {
  "use strict";

  // ---- 1. Hide / show a table or figure ----
  var TITLE = "h1, h2, h3, h4, .q-section-title";
  // The elements of a title: its next siblings up to the next title (or an element with one)
  function partsOf(title) {
    var out = [], n = title.nextElementSibling;
    while (n && !n.matches(TITLE) && !n.querySelector(TITLE)) {
      out.push(n);
      n = n.nextElementSibling;
    }
    return out;
  }
  document.addEventListener("click", function (e) {
    var eye = e.target.closest ? e.target.closest(".q-eye") : null;
    if (!eye) return;
    e.preventDefault();
    e.stopPropagation();
    var title = eye.closest(TITLE);
    if (!title) return;
    var hide = !title.classList.contains("q-collapsed");
    title.classList.toggle("q-collapsed", hide);
    eye.setAttribute("title", hide ? "Show" : "Hide");
    eye.setAttribute("aria-pressed", hide ? "true" : "false");
    partsOf(title).forEach(function (n) { n.classList.toggle("q-eye-hidden", hide); });
    // Plots and tables drawn while hidden: fit them to their width again
    if (!hide) {
      setTimeout(function () {
        window.dispatchEvent(new Event("resize"));
        if (window.jQuery && jQuery.fn.dataTable) jQuery.fn.dataTable.tables({ visible: true, api: true }).columns.adjust();
      }, 30);
    }
  }, true);
  // The eye must not open the tooltip of the info icon or select text
  document.addEventListener("mousedown", function (e) {
    if (e.target.closest && e.target.closest(".q-eye")) e.preventDefault();
  }, true);

  // ---- Batch steppers (`batch_stepper()` in app.R): the arrows choose the previous / next option
  // of their select (the select itself opens its list when clicked) ----
  window.quantaStepSelect = function (sel, d) {
    var i = sel.selectedIndex + d;
    if (i < 0 || i >= sel.options.length) return;
    sel.selectedIndex = i;
    if (window.jQuery) jQuery(sel).trigger("change"); else sel.dispatchEvent(new Event("change", { bubbles: true }));
  };
  document.addEventListener("click", function (e) {
    var b = e.target.closest ? e.target.closest(".q-bstep") : null;
    if (!b) return;
    var sel = document.getElementById(b.getAttribute("data-target"));
    if (sel) window.quantaStepSelect(sel, b.classList.contains("q-bstep-next") ? 1 : -1);
  });

  // ---- Key N on page 4: the next normalization method (radio buttons `norm_method`, in turn) ----
  document.addEventListener("keydown", function (e) {
    if (e.key !== "n" && e.key !== "N") return;
    if (e.ctrlKey || e.altKey || e.metaKey) return;
    var t = e.target;
    if (t && t.matches && t.matches("input:not([type=radio]), textarea, select, [contenteditable]")) return;
    var box = document.getElementById("norm_method");
    if (!box || box.offsetParent === null) return;   // Only when page 4 is shown
    var r = Array.prototype.filter.call(box.querySelectorAll("input[type=radio]"), function (x) { return !x.disabled; });
    if (r.length < 2) return;
    var i = r.indexOf(r.filter(function (x) { return x.checked; })[0]);
    var next = r[(i + 1) % r.length];
    next.checked = true;
    if (window.jQuery) jQuery(next).trigger("change"); else next.dispatchEvent(new Event("change", { bubbles: true }));
    e.preventDefault();
  });

  // ---- 2. Resizable tables ----
  // A bar under the scrolling body of a table (DataTables with `scrollY`), or under any other
  // scrolling box of class `q-resizable` (e.g. table 3.2): dragging it sets the height of the
  // body (at least 80 px)
  function addHandle(wrapper) {
    var own = wrapper.classList.contains("q-resizable");
    var body = own ? wrapper : wrapper.querySelector(".dataTables_scrollBody");
    if (!body) return;
    if (own ? (wrapper.nextElementSibling && wrapper.nextElementSibling.classList.contains("q-table-resize"))
            : wrapper.querySelector(":scope > .q-table-resize")) return;
    var h = document.createElement("div");
    h.className = "q-table-resize";
    h.title = "Drag to make the table taller or shorter";
    if (own) wrapper.parentNode.insertBefore(h, wrapper.nextSibling); else wrapper.appendChild(h);
    h.addEventListener("mousedown", function (e) {
      e.preventDefault();
      var y0 = e.clientY, h0 = body.getBoundingClientRect().height;
      document.body.classList.add("q-resizing");
      function move(ev) {
        var hh = Math.max(80, h0 + ev.clientY - y0);
        body.style.maxHeight = hh + "px";
        body.style.height = hh + "px";
        // A box drawn again (e.g. table 3.2 for another IS) keeps this height
        if (own && wrapper.parentNode) wrapper.parentNode._qHeight = hh;
      }
      function up() {
        document.removeEventListener("mousemove", move);
        document.removeEventListener("mouseup", up);
        document.body.classList.remove("q-resizing");
        if (window.jQuery && jQuery.fn.dataTable) jQuery.fn.dataTable.tables({ visible: true, api: true }).columns.adjust();
      }
      document.addEventListener("mousemove", move);
      document.addEventListener("mouseup", up);
    });
  }
  function scan() {
    document.querySelectorAll(".dataTables_wrapper, .q-resizable").forEach(addHandle);
  }
  // For boxes drawn by other scripts (www/quanta_figs.js calls it after drawing table 3.2)
  window.QuantaUI = { scan: scan };
  if (window.jQuery) {
    jQuery(document).on("init.dt draw.dt", function () { setTimeout(scan, 0); });
  }
  document.addEventListener("DOMContentLoaded", scan);
})();
