// ------------------------------------------------------------------------------------------- //
// The calibration curve of one compound (QuaNTA 4.4: app, internal report and tutorial). One plot
// with two modes:
// - "Calibration inspection": all calibration points (also Cal0, out of range, above the ULOQ),
//   coloured by their role, one symbol per batch;
// - "Final calibration model": the fitted curve, the calibration points it was fitted on, and the
//   QCs and study samples on it (QCs in one colour, study samples by class; values replaced by the
//   LOD/LLOQ rules as open circles).
// Both with the LOD / LLOQ (grey) and ULOQ (olive) areas, the labels of the limits and the
// parameters of the curve (chips). The data of all compounds and curves ("views": a batch, the
// global curve, ...) is given once (`calib_viewer_data()` in projlib/show.R); switching compounds,
// curves, modes or log axes is done here, without the server.
//
//   var v = QuantaCalViewer(element, data, options);
//   v.show(i); v.setMode("inspect" | "final"); v.setLog(true | false); v.setData(data); v.redraw();
//
// Options: `nav` (a search box, the compound list and previous / next buttons: the internal report
// and the tutorial; in the app, table 4.2 chooses the compound), `keys` (left / right: the mode;
// with `nav` also up / down: the compound), `height` (of the plot, in pixels).
// ------------------------------------------------------------------------------------------- //
(function (global) {
  "use strict";

  var OKABE = { orange: "#E69F00", green: "#009E73", blue: "#0072B2", vermillion: "#D55E00" };
  // The role of a calibration point (inspection mode), in the order of the legend
  var TYPE_COLORS = {
    "Within limits": OKABE.green, "Above the ULOQ": "#999933", "LOD = LLOQ": OKABE.orange,
    "Out-of-range": "gray", "Cal0": OKABE.vermillion, "LOD": OKABE.blue
  };
  // The labels and lines of the limits
  var LIMIT = { lod: "#0072B2", lloq: "#AA3377", lod_lloq: "#E69F00", uloq: "#7A7A1F" };
  var SYMBOLS = ["circle", "triangle-up", "square", "diamond", "star", "cross", "x", "hexagon",
                 "triangle-down", "pentagon", "hourglass", "bowtie"];
  var MODES = { inspect: "Calibration inspection", final: "Final calibration model" };

  // The look of the controls, the same in the app, the report and the tutorial
  var CSS = [
    ".qcv { font-family: Lato, 'Segoe UI', sans-serif; }",
    ".qcv-nav { display: flex; gap: 6px; align-items: center; flex-wrap: wrap; margin: 0 0 8px; }",
    ".qcv-nav input, .qcv-nav select, .qcv-view select { font: inherit; padding: 3px 6px; border: 1px solid #D0D5DD; border-radius: 6px; background: #fff; }",
    ".qcv-nav select { max-width: 360px; }",
    ".qcv-nav button { font: inherit; border: 1px solid #D0D5DD; background: #fff; border-radius: 6px; padding: 2px 8px; cursor: pointer; }",
    ".qcv-bar { display: flex; gap: 14px; flex-wrap: wrap; align-items: center; margin-bottom: 4px; }",
    ".qcv-group { display: inline-flex; border: 1px solid #D0D5DD; border-radius: 8px; overflow: hidden; }",
    ".qcv-group button { font: inherit; font-size: .9rem; border: 0; background: #fff; padding: 5px 12px; cursor: pointer; color: #1F2A44; }",
    ".qcv-group button + button { border-left: 1px solid #D0D5DD; }",
    ".qcv-group button.on { background: #1A0DF2; color: #fff; }",
    ".qcv-log { cursor: pointer; user-select: none; margin: 0; font-weight: normal; } .qcv-log input { vertical-align: middle; margin: 0 4px 0 0; }",
    ".qcv-view { margin: 0; font-weight: normal; }",
    ".qcv-chips { margin: 8px 0 0; font-size: .9rem; min-height: 1.6em; }",
    ".qcv-chip { display: inline-block; background: #EEF4FE; border-radius: 4px; padding: 1px 7px; margin: 0 6px 3px 0; }",
    ".qcv-chip.warn { background: #FDECEA; color: #c23636; font-weight: 600; }",
    ".qcv-chip.kept { background: #FFF1D6; color: #8A5300; font-weight: 600; }",
    ".qcv-msg { color: #667085; padding: 40px 0; text-align: center; font-size: 1.1rem; }"
  ].join("\n");
  function addStyle() {
    if (document.getElementById("qcv-style")) return;
    var s = document.createElement("style");
    s.id = "qcv-style";
    s.textContent = CSS;
    document.head.appendChild(s);
  }

  function num(x) {
    if (x === null || x === undefined || !isFinite(x)) return "–";
    var a = Math.abs(x);
    return (a !== 0 && (a >= 1e5 || a < 1e-3)) ? x.toExponential(3).replace("e+", "E+").replace("e-", "E-")
                                              : String(+x.toPrecision(4));
  }
  function finite(x) { return x !== null && x !== undefined && isFinite(x); }
  function idx(n) { var o = []; for (var i = 0; i < n; i++) o.push(i); return o; }
  function pick(arr, k) { return k.map(function (j) { return arr[j]; }); }
  function esc(s) { return String(s).replace(/[&<>"]/g, function (c) { return { "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c]; }); }

  // Grey areas below the LLOQ and below the LOD (darker). Shapes are placed by the concentration
  // also on log axes (plotly.js 2.11; log10 values are misplaced there)
  function lloxShapes(lod, lloq, xs, log) {
    var pos = xs.filter(function (x) { return finite(x) && x > 0; });
    var from = log ? Math.min.apply(null, pos.concat([lod, lloq].filter(finite))) / 10 : 0;
    var shade = function (x1) {
      return { type: "rect", xref: "x", yref: "paper", x0: from, x1: x1, y0: 0, y1: 1, layer: "below",
               fillcolor: "rgba(64,64,64,0.25)", line: { width: 0 } };
    };
    var out = [];
    if (finite(lloq) && isFinite(from)) out.push(shade(lloq));
    if (finite(lod) && isFinite(from)) out.push(shade(lod));
    return out;
  }
  // The olive area above the ULOQ, to the highest concentration shown, and a dashed line at it
  function uloqShapes(uloq, xs) {
    if (!finite(uloq)) return [];
    var xmax = Math.max.apply(null, xs.filter(finite));
    var out = [];
    if (isFinite(xmax) && xmax > uloq) {
      out.push({ type: "rect", xref: "x", yref: "paper", x0: uloq, x1: xmax, y0: 0, y1: 1, layer: "below",
                 fillcolor: "rgba(153,153,51,0.15)", line: { width: 0 } });
    }
    out.push({ type: "line", xref: "x", yref: "paper", x0: uloq, x1: uloq, y0: 0, y1: 1,
               line: { color: "#999933", width: 1.5, dash: "dash" } });
    return out;
  }
  // Dotted lines and coloured labels of the LOD and LLOQ (one when they are the same level) and
  // the label of the ULOQ. Annotations are placed by the log10 of the concentration on log axes
  function limitMarks(lod, lloq, uloq, log) {
    var ok = function (x) { return finite(x) && (!log || x > 0); };
    var shapes = [], ann = [];
    var mark = function (x, text, col, right, y, line) {
      if (line) shapes.push({ type: "line", xref: "x", yref: "paper", x0: x, x1: x, y0: 0, y1: 1,
                              line: { color: col, width: 1.2, dash: "dot" } });
      ann.push({ x: log ? Math.log10(x) : x, y: y, xref: "x", yref: "paper", text: "<b>" + text + "</b>",
                 showarrow: false, xanchor: right ? "left" : "right", yanchor: "top", xshift: right ? 3 : -3,
                 font: { color: col, size: 12 }, bgcolor: "rgba(255,255,255,0.75)", borderpad: 1 });
    };
    if (ok(lod) && ok(lloq) && lod === lloq) {
      mark(lloq, "LOD = LLOQ", LIMIT.lod_lloq, false, 0.99, true);
    } else {
      if (ok(lloq)) mark(lloq, "LLOQ", LIMIT.lloq, false, 0.99, true);
      if (ok(lod)) mark(lod, "LOD", LIMIT.lod, false, 0.91, true);
    }
    if (ok(uloq)) mark(uloq, "ULOQ", LIMIT.uloq, true, 0.99, false);
    return { shapes: shapes, annotations: ann };
  }
  // The name of the compound as a band over the plot, coloured by the state of its curve
  function titleBand(title, fill) {
    return {
      shapes: [{ type: "rect", xref: "paper", yref: "paper", x0: 0, x1: 1, y0: 1.01, y1: 1.09,
                 fillcolor: fill, line: { width: 0 } }],
      annotations: [{ text: esc(title), xref: "paper", yref: "paper", x: 0.5, y: 1.05, xanchor: "center",
                      yanchor: "middle", showarrow: false, font: { size: 14 } }]
    };
  }
  // The fitted curve from its coefficients (signal = b0 + b1 x (+ b2 x²); in log scale of
  // log(conc) and log(signal + 1)), from the start of the curve (LLOQ) to its top
  function curvePoints(e, log) {
    var cf = e.coef, from = finite(e.lloq) ? e.lloq : e.min, to = e.top;
    if (!cf || !cf.length || !finite(from) || !finite(to) || from <= 0 && log || to <= from) return null;
    var xs = [], n = 120, i;
    if (log) {
      for (i = 0; i < n; i++) xs.push(Math.exp(Math.log(from) + (Math.log(to) - Math.log(from)) * i / (n - 1)));
    } else {
      // Evenly spaced, and more points at the start (the bend of a quadratic curve near the LLOQ)
      for (i = 0; i < 100; i++) xs.push(from + (to - from) * i / 99);
      var to2 = Math.min(from * 10, to);
      for (i = 1; i < 39; i++) xs.push(from + (to2 - from) * i / 39);
      xs.sort(function (a, b) { return a - b; });
    }
    var ys = xs.map(function (x) {
      var xx = log ? Math.log(x) : x;
      var y = cf[0] + cf[1] * xx + (cf.length > 2 ? cf[2] * xx * xx : 0);
      return log ? Math.exp(y) - 1 : y;
    });
    return { x: xs, y: ys };
  }

  function QuantaCalViewer(el, data, opts) {
    opts = opts || {};
    addStyle();
    var state = { i: 0, mode: opts.mode || "inspect", log: !!data.log_scale, view: null, viewPref: null };
    el.innerHTML = "";
    el.classList.add("qcv");
    var bar = document.createElement("div");
    bar.className = "qcv-bar";
    var chips = document.createElement("div");
    chips.className = "qcv-chips";
    var plot = document.createElement("div");
    plot.className = "qcv-plot";
    el.appendChild(bar);
    el.appendChild(chips);
    el.appendChild(plot);

    // The modes as a button group, the curve (when there are several) and log axes
    var modeGroup = document.createElement("div");
    modeGroup.className = "qcv-group";
    Object.keys(MODES).forEach(function (k) {
      var b = document.createElement("button");
      b.type = "button";
      b.textContent = MODES[k];
      b.dataset.key = k;
      b.title = k === "inspect" ? "Calibration inspection (←)" : "Final calibration model (→)";
      b.addEventListener("click", function () { api.setMode(k); });
      modeGroup.appendChild(b);
    });
    var viewLabel = document.createElement("label");
    viewLabel.className = "qcv-view";
    viewLabel.innerHTML = "Curve <select></select>";
    var viewSel = viewLabel.querySelector("select");
    // The curve chosen by the user is kept for the next compounds that have it; otherwise each
    // compound starts with its first curve (in the app: the one used for its concentrations)
    viewSel.addEventListener("change", function () { state.view = state.viewPref = viewSel.value; draw(); });
    var logLabel = document.createElement("label");
    logLabel.className = "qcv-log";
    logLabel.innerHTML = '<input type="checkbox">Log';
    var logBox = logLabel.querySelector("input");
    logBox.addEventListener("change", function () { api.setLog(logBox.checked); });
    bar.appendChild(modeGroup);
    bar.appendChild(viewLabel);
    bar.appendChild(logLabel);

    // Navigation (`opts.nav`): a search box keeping the compounds whose name contains the text,
    // the compound list, and the previous / next compound
    var nav = null, sel = null, search = null;
    function fill(q) {
      var cur = sel.value;
      sel.innerHTML = "";
      data.compounds.forEach(function (c, i) {
        if (q && c.name.toLowerCase().indexOf(q) < 0) return;
        var o = document.createElement("option");
        o.value = i;
        o.textContent = c.name;
        sel.appendChild(o);
      });
      if (Array.prototype.some.call(sel.options, function (o) { return o.value === cur; })) sel.value = cur;
    }
    if (opts.nav) {
      nav = document.createElement("div");
      nav.className = "qcv-nav";
      nav.innerHTML = '<input type="search" placeholder="Search…" size="16">' +
        '<button type="button" title="Previous (↑)">&#9664;</button><select></select>' +
        '<button type="button" title="Next (↓)">&#9654;</button>';
      el.insertBefore(nav, bar);
      search = nav.querySelector("input");
      sel = nav.querySelector("select");
      var btns = nav.querySelectorAll("button");
      search.addEventListener("input", function () {
        fill(search.value.trim().toLowerCase());
        if (sel.options.length && +sel.value !== state.i) api.show(+sel.value);
      });
      sel.addEventListener("change", function () { api.show(+sel.value); });
      btns[0].addEventListener("click", function () { api.step(-1); });
      btns[1].addEventListener("click", function () { api.step(1); });
      fill("");
    }

    // Keys (`opts.keys`): left / right the mode; with `nav` up / down the compound. Not while
    // typing in a field (but up / down in the search box go through its matches), nor when the
    // viewer is hidden (e.g. on another page of the app)
    if (opts.keys) {
      document.addEventListener("keydown", function (e) {
        if (!document.body.contains(el) || el.offsetParent === null) return;
        if (e.ctrlKey || e.altKey || e.metaKey) return;
        var t = e.target, typing = t && t.matches && t.matches("input, textarea, select, [contenteditable]");
        var inSearch = search && t === search;
        if ((e.key === "ArrowLeft" || e.key === "ArrowRight") && !typing) {
          e.preventDefault();
          api.setMode(e.key === "ArrowLeft" ? "inspect" : "final");
        } else if ((e.key === "ArrowUp" || e.key === "ArrowDown") && nav && (!typing || inSearch)) {
          e.preventDefault();
          api.step(e.key === "ArrowDown" ? 1 : -1);
        }
      });
    }

    var unit = function () { return data.unit ? " [" + data.unit + "]" : ""; };

    // The curves (views) with data for compound `id`, and the entry of a view
    function entryOf(view, id) {
      for (var p = 0; p < view.parts.length; p++) {
        var e = view.parts[p].c[id];
        if (e) return { e: e, part: view.parts[p] };
      }
      return null;
    }
    function viewsOf(id) {
      return data.views.filter(function (v) { return entryOf(v, id) !== null; });
    }

    // The calibration points and samples of an entry, with the information shared by the compounds
    // of the same part of a view
    function calOf(x) {
      var ci = x.part.cal_info, c = x.e.cal;
      return { x: ci.x, y: c.y, used: c.used, batch: ci.batch, inj: ci.inj, name: ci.name,
               type: c.type.map(function (t) { return t === null ? null : data.types[t]; }) };
    }
    function smpOf(x) {
      var si = x.part.smp_info, s = x.e.smp;
      if (!si || !s) return null;
      return { x: s.x, y: s.y, kind: s.kind, x0: s.x0, grp: si.grp, cls: si.cls, batch: si.batch, inj: si.inj, name: si.name };
    }

    function inspectTraces(x) {
      var cal = calOf(x), traces = [];
      var batches = [];
      cal.batch.forEach(function (b) { if (batches.indexOf(b) < 0) batches.push(b); });
      batches.sort(function (a, b) { return String(a).localeCompare(String(b), undefined, { numeric: true }); });
      var sym = {};
      batches.forEach(function (b, i) { sym[b] = SYMBOLS[i % SYMBOLS.length]; });
      // On log axes, a concentration of 0 (Cal0) has no place: a short horizontal line at the left
      // edge, at its signal
      var pos = cal.x.filter(function (v, j) { return v > 0 && finite(cal.y[j]); });
      var cal0X = pos.length ? Math.min.apply(null, pos) / 4 : null;
      Object.keys(TYPE_COLORS).forEach(function (ty) {
        var atEdge = state.log && ty === "Cal0";
        var k = idx(cal.x.length).filter(function (j) {
          return cal.type[j] === ty && finite(cal.y[j]) &&
                 (!state.log || (cal.y[j] > 0 && (cal.x[j] > 0 || (atEdge && cal0X !== null))));
        });
        if (!k.length) return;
        traces.push({
          type: "scatter", mode: "markers", name: atEdge ? "Cal0 (conc. 0, at the left edge)" : ty, legendgroup: ty,
          x: k.map(function (j) { return atEdge ? cal0X : cal.x[j]; }), y: pick(cal.y, k),
          marker: atEdge ? { color: TYPE_COLORS[ty], size: 16, symbol: "line-ew-open", line: { width: 3, color: TYPE_COLORS[ty] } }
                         : { color: TYPE_COLORS[ty], size: 8, symbol: k.map(function (j) { return sym[cal.batch[j]]; }) },
          text: k.map(function (j) {
            return "<b>" + esc(cal.name[j]) + "</b><br>batch " + esc(cal.batch[j]) + " · inj. " + cal.inj[j] +
                   "<br>conc: " + num(cal.x[j]) + (atEdge ? " (drawn at the left edge)" : "") + "<br>signal: " + num(cal.y[j]);
          }),
          hoverinfo: "text"
        });
      });
      // The batches (symbols) in the legend, without points of their own
      if (batches.length > 1) {
        batches.forEach(function (b) {
          traces.push({ type: "scatter", mode: "markers", x: [null], y: [null], name: "Batch " + b, legendgroup: "batch",
                        marker: { color: "black", size: 8, symbol: sym[b] }, hoverinfo: "skip" });
        });
      }
      return traces;
    }
    function finalTraces(x) {
      var traces = [];
      var cv = curvePoints(x.e, state.log);
      if (cv) {
        traces.push({ type: "scatter", mode: "lines", name: "Calibration curve", x: cv.x, y: cv.y,
                      hoverinfo: "skip", line: { color: "rgba(95,158,160,0.8)", width: 2 } });
      }
      var cal = calOf(x);
      var k = idx(cal.x.length).filter(function (j) { return cal.used[j] && finite(cal.y[j]); });
      traces.push({
        type: "scatter", mode: "markers", name: "Calibration points", x: pick(cal.x, k), y: pick(cal.y, k),
        marker: { color: "black", size: 6 },
        text: k.map(function (j) {
          return "<b>" + esc(cal.name[j]) + "</b><br>batch " + esc(cal.batch[j]) + " · inj. " + cal.inj[j] +
                 "<br>conc: " + num(cal.x[j]) + "<br>signal: " + num(cal.y[j]);
        }),
        hoverinfo: "text"
      });
      var s = smpOf(x);
      if (s) {
        // QCs in one colour (the QC group in the hover text), study samples by class. Values
        // replaced by the LOD/LLOQ rules (or without a value) as open circles
        var groups = Object.keys(data.group_colors).filter(function (g) { return s.grp.indexOf(g) >= 0; });
        s.grp.forEach(function (g) { if (groups.indexOf(g) < 0) groups.push(g); });
        var anyReplaced = false;
        groups.forEach(function (g) {
          var col = data.group_colors[g] || "grey";
          var has = idx(s.x.length).some(function (j) { return s.grp[j] === g && finite(s.x[j]) && finite(s.y[j]); });
          if (!has) return;
          // One legend entry per group, always with the filled symbol; a click on it hides or
          // shows all points of the group (`legendgroup`)
          traces.push({ type: "scatter", mode: "markers", x: [null], y: [null], name: g, legendgroup: g,
                        marker: { color: col, size: 7, opacity: 0.85 }, hoverinfo: "skip" });
          [false, true].forEach(function (replaced) {
            var kk = idx(s.x.length).filter(function (j) {
              return s.grp[j] === g && finite(s.x[j]) && finite(s.y[j]) && (s.kind[j] > 0) === replaced;
            });
            if (!kk.length) return;
            if (replaced) anyReplaced = true;
            traces.push({
              type: "scatter", mode: "markers", name: g, legendgroup: g, showlegend: false,
              x: pick(s.x, kk), y: pick(s.y, kk),
              // Open symbols take the marker colour as their line colour
              marker: replaced ? { color: col, size: 8, symbol: "circle-open", line: { width: 1.5 } }
                               : { color: col, size: 7, opacity: 0.85 },
              text: kk.map(function (j) {
                var kind = data.kinds[s.kind[j]] || "";
                var noValue = /no value/.test(kind);
                return "<b>" + esc(s.name[j]) + "</b><br>" + (g === "QC" ? "QC group: " : "class: ") + esc(s.cls[j]) +
                       " · batch " + esc(s.batch[j]) + " · inj. " + s.inj[j] +
                       "<br>conc: " + (noValue ? kind.replace(" (no value)", "") + " (no value; calculated " + num(s.x[j]) + ")" : num(s.x[j])) +
                       (replaced && !noValue ? "<br>replaced by the LOD/LLOQ rules: " + kind + " (calculated " + num(s.x0[j]) + ")" : "") +
                       "<br>signal: " + num(s.y[j]);
              }),
              hoverinfo: "text"
            });
          });
        });
        // What the open circles mean, in the legend
        if (anyReplaced) {
          traces.push({ type: "scatter", mode: "markers", x: [null], y: [null], name: "Replaced (LOD/LLOQ rules)",
                        marker: { color: "#555", size: 8, symbol: "circle-open", line: { width: 1.5 } }, hoverinfo: "skip" });
        }
      }
      return traces;
    }

    // The parameters of the curve as chips, and its notes
    function drawChips(x, view) {
      var e = x.e, u = data.unit ? " " + esc(data.unit) : "";
      var chip = function (label, value, cls) {
        return '<span class="qcv-chip' + (cls ? " " + cls : "") + '">' + label + (value === undefined ? "" : " <b>" + value + "</b>") + "</span>";
      };
      var html = [];
      if (e.why) html.push(chip(esc(e.why), undefined, e.valid ? "kept" : "warn"));
      if (e.valid && e.src === "Global" && view.info) html.push(chip(esc(view.info), undefined, view.info_warn ? "warn" : undefined));
      if (e.nq) html.push(chip(esc(e.nq), undefined, "kept"));
      if (e.model) html.push(chip("Model", esc(e.model)));
      if (finite(e.r2)) html.push(chip("R²", (+e.r2).toFixed(3)));
      if (finite(e.lod) || finite(e.lloq)) {
        html.push(chip("LOD", finite(e.lod) ? num(e.lod) + u : "–"));
        html.push(chip("LLOQ", finite(e.lloq) ? num(e.lloq) + u : "–"));
      }
      if (e.valid) {
        html.push(chip("ULOQ", finite(e.uloq) ? num(e.uloq) + u : "–"));
        html.push(chip("Highest cal. conc.", num(e.top) + u));
      }
      if (e.src && view.parts.length) html.push(chip("Curve", esc(e.src)));
      chips.innerHTML = html.join("");
    }
    function message(text) {
      chips.innerHTML = "";
      if (typeof Plotly !== "undefined") Plotly.purge(plot);
      plot.innerHTML = '<div class="qcv-msg">' + esc(text) + "</div>";
    }

    function markControls(views) {
      modeGroup.querySelectorAll("button").forEach(function (b) { b.classList.toggle("on", b.dataset.key === state.mode); });
      logBox.checked = state.log;
      viewSel.innerHTML = "";
      views.forEach(function (v) {
        var o = document.createElement("option");
        o.value = v.label;
        o.textContent = v.label;
        viewSel.appendChild(o);
      });
      viewSel.value = state.view;
      viewLabel.style.display = views.length > 1 ? "" : "none";
    }

    var waiting = false;
    function draw() {
      // plotly.js can load after the data arrives (in the app): draw when it is there
      if (typeof Plotly === "undefined") {
        if (!waiting) {
          waiting = true;
          var t = setInterval(function () {
            if (typeof Plotly !== "undefined") { clearInterval(t); waiting = false; draw(); }
          }, 100);
        }
        return;
      }
      var c = data.compounds[state.i];
      if (!c) { message("No compound selected."); return; }
      var views = viewsOf(c.id);
      if (!views.length) { message("No calibration data for " + c.name + "."); return; }
      // The curve chosen by the user, when this compound has it; otherwise its first one
      var has = function (l) { return views.some(function (v) { return v.label === l; }); };
      state.view = state.viewPref && has(state.viewPref) ? state.viewPref : views[0].label;
      var view = views.filter(function (v) { return v.label === state.view; })[0];
      var x = entryOf(view, c.id), e = x.e;
      markControls(views);
      if (plot.querySelector(".qcv-msg")) plot.innerHTML = "";
      drawChips(x, view);
      var band = titleBand(c.name, e.fill);
      var log = state.log, axisType = log ? "log" : "linear";
      var layout = {
        font: { family: "Lato" }, hovermode: "closest", height: opts.height || 480,
        legend: { orientation: "v", x: 1.02, y: 1, xanchor: "left", tracegroupgap: 10 },
        margin: { t: 60, b: 60, l: 70, r: 170 },
        xaxis: { title: { text: (state.mode === "inspect" ? "Calibrant concentration" : "Concentration") + unit() },
                 type: axisType, exponentformat: "E" },
        yaxis: { title: { text: data.y_title }, type: axisType, exponentformat: "E" }
      };
      var config = { displaylogo: false, responsive: true };
      // No valid curve: the calibration points can still be inspected, but there is no model
      if (state.mode === "final" && !e.valid) {
        layout.xaxis.visible = false;
        layout.yaxis.visible = false;
        layout.shapes = band.shapes;
        layout.annotations = band.annotations.concat([{
          text: "No valid calibration curve" + (e.why ? "<br><span style='font-size:15px'>" + esc(e.why) + "</span>" : ""),
          x: 0.5, y: 0.5, xref: "paper", yref: "paper", showarrow: false, font: { size: 20, color: "gray" } }]);
        Plotly.react(plot, [], layout, config);
        return;
      }
      var traces = state.mode === "inspect" ? inspectTraces(x) : finalTraces(x);
      var xs = [];
      traces.forEach(function (t) { xs = xs.concat(t.x.filter(finite)); });
      var marks = limitMarks(e.lod, e.lloq, e.valid ? e.uloq : null, log);
      layout.shapes = lloxShapes(e.lod, e.lloq, xs, log).concat(e.valid ? uloqShapes(e.uloq, xs) : [], marks.shapes, band.shapes);
      layout.annotations = marks.annotations.concat(band.annotations);
      Plotly.react(plot, traces, layout, config);
    }

    var api = {
      show: function (i) {
        if (i >= 0 && i < data.compounds.length) {
          state.i = i;
          if (sel && sel.value !== String(i)) {
            // A compound outside the search: clear the search
            if (!Array.prototype.some.call(sel.options, function (o) { return o.value === String(i); })) { search.value = ""; fill(""); }
            sel.value = String(i);
          }
          draw();
        } else {
          state.i = i;
          message(i >= data.compounds.length ? "No calibration curve: the compound was not calibrated." : "No compound selected.");
        }
      },
      // The previous / next compound (among those the search keeps)
      step: function (d) {
        if (!sel) { api.show(state.i + d); return; }
        var k = sel.selectedIndex + d;
        if (k >= 0 && k < sel.options.length) api.show(+sel.options[k].value);
      },
      setMode: function (m) { if (MODES[m] && m !== state.mode) { state.mode = m; draw(); } },
      setLog: function (on) { state.log = !!on; draw(); },
      // New data (e.g. another batch or normalization method): the compound, mode and axes stay
      setData: function (d) {
        data = d;
        if (sel) fill(search.value.trim().toLowerCase());
        api.show(state.i);
      },
      redraw: function () { if (plot.data && typeof Plotly !== "undefined") Plotly.Plots.resize(plot); },
      state: function () { return { i: state.i, mode: state.mode, log: state.log, view: state.view }; }
    };
    api.show(0);
    return api;
  }

  global.QuantaCalViewer = QuantaCalViewer;
})(window);
