// ------------------------------------------------------------------------------------------- //
// Figures of the app drawn in the browser from data sent once (as the calibration curve viewer,
// calcurve_viewer.js), so that choosing a row in a table shows its figure at once:
// - 3.5: the peak areas of an internal standard, by sample group and along the injection order
//   (`is_distribution_data()` in projlib/show.R);
// - 4.5: the concentrations of a compound in the QCs and study samples of a batch, by class;
// - 4.6: the concentrations of a compound in the QCs of all batches, along the injection order
//   (both `conc_figs_data()`).
//
//   QuantaFigs.isDistribution(element, data, i);    // i: the row of table 3.1 (null: none)
//   QuantaFigs.isRsdTable(element, data, i);        // table 3.4 of the same IS
//   QuantaFigs.concByClass(element, data, id, log); // id: the compound (null: none)
//   QuantaFigs.qcConc(element, data, id);
//   QuantaFigs.message(element, text);
//
// Each plot keeps its Log / Linear choice when another row is shown.
// ------------------------------------------------------------------------------------------- //
(function (global) {
  "use strict";

  function finite(x) { return x !== null && x !== undefined && isFinite(x); }
  function idx(n) { var o = []; for (var i = 0; i < n; i++) o.push(i); return o; }
  function esc(s) { return String(s).replace(/[&<>"]/g, function (c) { return { "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c]; }); }
  // As `num_txt()` / `sci_txt()` of projlib/show.R
  function sci(x) { return finite(x) ? x.toExponential(3).toUpperCase().replace(/E([+-])(\d)$/, "E$10$2") : "NA"; }
  function num(x) { return !finite(x) ? "NA" : Math.abs(x) >= 1000 ? sci(x) : String(+x.toPrecision(4)); }
  function rgba(hex, a) {
    var m = /^#?([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})$/i.exec(hex || "");
    return m ? "rgba(" + parseInt(m[1], 16) + "," + parseInt(m[2], 16) + "," + parseInt(m[3], 16) + "," + a + ")" : hex;
  }

  // plotly.js can load after the data arrives: draw when it is there
  function whenPlotly(fn) {
    if (typeof Plotly !== "undefined") { fn(); return; }
    var t = setInterval(function () { if (typeof Plotly !== "undefined") { clearInterval(t); fn(); } }, 100);
  }
  var CONFIG = { displaylogo: false, responsive: true };

  // The Log / Linear buttons; the choice is kept per element (`el._qfLog`)
  function logButtons(el, log, x) {
    var b = function (label, type) { return { label: label, method: "relayout", args: [{ "yaxis.type": type }] }; };
    return [{ type: "buttons", direction: "right", x: x || 0.55, y: 1.02, xanchor: "center", yanchor: "bottom",
              pad: { t: 0, r: 0 }, active: log ? 1 : 0, buttons: [b("Linear", "linear"), b("Log", "log")] }];
  }
  function react(el, traces, layout) {
    var msg = el.querySelector(".qf-msg");
    if (msg) el.innerHTML = "";
    Plotly.react(el, traces, layout, CONFIG).then(function () {
      if (el._qfBound) return;
      el._qfBound = true;
      el.on("plotly_relayout", function (ev) {
        if (ev && ev["yaxis.type"]) el._qfLog = ev["yaxis.type"] === "log";
      });
    });
  }
  // A message in place of the plot (as `plotly_empty()` with a grey text)
  function message(el, text, height) {
    whenPlotly(function () {
      react(el, [], {
        height: height || el._qfHeight || 460, font: { family: "Lato" },
        xaxis: { visible: false }, yaxis: { visible: false },
        annotations: [{ text: esc(text), x: 0.5, y: 0.5, xref: "paper", yref: "paper", showarrow: false,
                        font: { size: 20, color: "gray" } }]
      });
    });
  }

  // 3.5: box plot per sample group (left) and the samples along the injection order (right),
  // sharing the y axis (log at the start); outlier samples as crosses
  function isDistribution(el, d, i, height) {
    el._qfHeight = height || 480;
    if (!d || i === null || i === undefined || !d.y[i]) { message(el, "Select an internal standard in table 3.1 (left) to show it here."); return; }
    whenPlotly(function () {
      var y = d.y[i], s = d.smp, groups = [];
      s.grp.forEach(function (g) { if (groups.indexOf(g) < 0) groups.push(g); });
      var traces = [];
      groups.forEach(function (g) {
        var k = idx(y.length).filter(function (j) { return s.grp[j] === g; });
        var col = d.colors[g] || "#999999";
        traces.push({ type: "box", y: k.map(function (j) { return y[j]; }), name: g, legendgroup: g,
                      marker: { color: col }, line: { color: col }, fillcolor: col, opacity: 0.6, boxpoints: false,
                      showlegend: false, hoverinfo: "y", xaxis: "x", yaxis: "y" });
        traces.push({ type: "scatter", mode: "markers", x: k.map(function (j) { return s.inj[j]; }),
                      y: k.map(function (j) { return y[j]; }), name: g, legendgroup: g, hoverinfo: "text",
                      text: k.map(function (j) {
                        return "<b>" + esc(s.name[j]) + "</b><br>" + esc(s.cls[j]) + " · batch " + esc(s.batch[j]) +
                               " · inj. " + s.inj[j] + "<br>peak area " + sci(y[j]) + (s.out[j] ? "<br><b>outlier sample</b>" : "");
                      }),
                      marker: { color: col, size: 6, symbol: k.map(function (j) { return s.out[j] ? "x" : "circle"; }) },
                      xaxis: "x2", yaxis: "y" });
      });
      var log = el._qfLog === undefined ? true : el._qfLog;
      var name = esc(d.names[i]);
      react(el, traces, {
        height: el._qfHeight, font: { family: "Lato" }, hovermode: "closest",
        title: { text: d.failed[i] ? name + " <span style='color:#c23636'>(failed: removed before normalization)</span>" : name, x: 0.02 },
        margin: { t: 70, r: 20, b: 60, l: 80 },
        // The groups panel keeps its x axis when zooming (only the injection order zooms)
        xaxis: { domain: [0, 0.27], tickangle: -30, fixedrange: true },
        xaxis2: { domain: [0.32, 1], title: { text: "Injection order" } },
        yaxis: { title: { text: "Peak area" }, type: log ? "log" : "linear", exponentformat: "E", hoverformat: ".3e" },
        legend: { orientation: "v", x: 1.02, y: 1 },
        updatemenus: logButtons(el, log)
      });
    });
  }

  // Table 3.4: the RSD% of the IS of row `i` of table 3.1 in each QC group and sample class, with
  // the number of samples and why an RSD% is missing. The table is made once; another IS only
  // changes its caption and cells (no redraw of the whole table, so it does not jump)
  function isRsdTable(el, d, i) {
    var r = d && d.rsd;
    if (!r || i === null || i === undefined || !r.v[i]) {
      el.innerHTML = '<div class="qf-msg" style="color:#667085;padding:12px 0;">' +
        (r ? "Select an internal standard in table 3.1 (left) to show it here." : "No RSD% of the internal standards.") + "</div>";
      el._qfTable = null;
      return;
    }
    var v = r.v[i];
    var notes = r.groups.map(function (g, k) {
      return r.n[k] < 2 ? "Too few samples (n = " + r.n[k] + "): RSD% needs at least 2" :
             !finite(v[k]) ? "Not computable: all values 0 or missing" : "";
    });
    var hasNote = notes.some(function (t) { return t !== ""; });
    var t = el._qfTable;
    // Made again only when the rows or the Note column change
    if (!t || t.rows !== r.groups.length || t.note !== hasNote) {
      var th = function (s) { return '<th style="color:green;font-weight:bold;text-align:center;">' + s + "</th>"; };
      el.innerHTML = '<div style="max-height:400px;overflow:auto;">' +
        '<table class="display dataTable no-footer" style="width:100%;">' +
        '<caption style="caption-side:top;text-align:left;color:#667085;"></caption>' +
        "<thead><tr>" + th("") + th("RSD%") + th("n") + (hasNote ? th("Note") : "") + "</tr></thead><tbody>" +
        r.groups.map(function (g) {
          return "<tr><td>" + esc(g) + '</td><td style="text-align:center;"></td><td style="text-align:center;"></td>' +
                 (hasNote ? '<td style="text-align:center;"></td>' : "") + "</tr>";
        }).join("") + "</tbody></table></div>";
      t = el._qfTable = { rows: r.groups.length, note: hasNote, caption: el.querySelector("caption"),
                          cells: Array.prototype.map.call(el.querySelectorAll("tbody tr"), function (tr) { return tr.children; }) };
    }
    t.caption.textContent = d.names[i];
    t.cells.forEach(function (c, k) {
      c[1].textContent = finite(v[k]) ? (+v[k]).toFixed(2) : "";
      c[2].textContent = r.n[k];
      if (hasNote) c[3].textContent = notes[k];
    });
  }

  // The values of a compound in a part of `conc_figs_data()`: shown ones and the number of those
  // without a value per flag ("n.d." / "n.q.")
  function valuesOf(d, part, id) {
    var e = part.c[id];
    if (!e) return null;
    var out = [], noValue = {};
    e.y.forEach(function (y, j) {
      var kind = d.kinds[e.kind[j]] || "";
      if (/no value/.test(kind)) { var f = kind.replace(" (no value)", ""); noValue[f] = (noValue[f] || 0) + 1; return; }
      if (!finite(y)) return;
      out.push({ y: y, kind: kind, name: part.info.name[j], cls: part.info.cls[j], batch: part.info.batch[j], inj: part.info.inj[j] });
    });
    return { v: out, noValue: noValue };
  }
  function noValueNote(nv) {
    var k = Object.keys(nv);
    return k.length ? "Not shown (no value): " + k.map(function (f) { return f + " " + nv[f]; }).join(", ") : "";
  }

  // 4.5: a box per class with the samples as points, coloured by measured / replaced by the
  // LOD/LLOQ rules (as in the exported tables)
  function concByClass(el, d, id, log) {
    el._qfHeight = 460;
    if (!d || id === null || id === undefined) { message(el, "Select a chemical in table 4.2 (left) to show it here."); return; }
    var r = valuesOf(d, d.samples, id);
    if (!r) { message(el, "No concentrations: the chemical was not calibrated."); return; }
    if (!r.v.length) {
      message(el, Object.keys(r.noValue).length ? "All values below the LOD / LLOQ (n.d. / n.q.)" : "Not detected / quantified in samples");
      return;
    }
    whenPlotly(function () {
      var levs = Object.keys(d.class_colors).filter(function (c) { return r.v.some(function (v) { return v.cls === c; }); });
      r.v.map(function (v) { return v.cls; }).sort().forEach(function (c) { if (levs.indexOf(c) < 0) levs.push(c); });
      var traces = [];
      levs.forEach(function (c, i) {
        var ys = r.v.filter(function (v) { return v.cls === c; }).map(function (v) { return v.y; });
        var col = d.class_colors[c] || "#999999";
        traces.push({ type: "box", x: ys.map(function () { return i + 1; }), y: ys, name: c, boxpoints: false,
                      fillcolor: rgba(col, 0.45), line: { color: "#4D4D4D", width: 1 }, width: 0.6,
                      hoverinfo: "skip", showlegend: false });
      });
      // A fixed jitter per sample (the same at every redraw)
      var jit = function (j) { return ((j * 0.618034) % 1 - 0.5) * 0.44; };
      Object.keys(d.kind_colors).forEach(function (kd) {
        var k = idx(r.v.length).filter(function (j) { return r.v[j].kind === kd; });
        if (!k.length) return;
        traces.push({
          type: "scatter", mode: "markers", name: kd, hoverinfo: "text",
          x: k.map(function (j) { return levs.indexOf(r.v[j].cls) + 1 + jit(j); }), y: k.map(function (j) { return r.v[j].y; }),
          text: k.map(function (j) {
            var v = r.v[j];
            return "<b>" + esc(v.name) + "</b><br>" + esc(v.cls) + " · batch " + esc(v.batch) + " · inj. " + v.inj +
                   "<br>conc. " + num(v.y) + " (" + esc(v.kind) + ")";
          }),
          marker: { color: d.kind_colors[kd], size: 7, opacity: 0.85, line: { color: "white", width: 0.5 } }
        });
      });
      var isLog = el._qfLog === undefined ? !!log : el._qfLog;
      var note = noValueNote(r.noValue);
      react(el, traces, {
        height: el._qfHeight, font: { family: "Lato" }, hovermode: "closest", title: { text: esc(nameOf(d, id)), x: 0.02 },
        xaxis: { title: { text: "" }, tickvals: levs.map(function (c, i) { return i + 1; }), ticktext: levs, tickangle: -45,
                 range: [0.4, levs.length + 0.6], zeroline: false },
        yaxis: { title: { text: d.y_title }, type: isLog ? "log" : "linear", exponentformat: "E" },
        legend: { orientation: "v", x: 1.02, y: 1, title: { text: "Value" } },
        annotations: note ? [{ text: esc(note), x: 0, y: 1.02, xref: "paper", yref: "paper", xanchor: "left", yanchor: "bottom",
                               showarrow: false, font: { size: 12, color: "#666666" } }] : [],
        margin: { t: 70 },
        updatemenus: logButtons(el, isLog)
      });
    });
  }

  // 4.6: the QCs of all batches (each with its own calibration), in two views (buttons above the
  // plot; the choice is kept per element, `el._qfMode`):
  // - "Control chart": the % deviation of each QC from the median of its QC group, along the
  //   injection order, with a band at ±15% and lines at ±30%, the batches shaded, a trend (moving
  //   mean of the deviations within each batch) and the QCs beyond ±30% ringed in red;
  // - "Per batch": the QC values of each batch and QC group, with mean ± SD, the CV% above
  //   (red above 15%), and the mean of all batches as a dashed line (Log / Linear buttons).
  // Chips above: QCs beyond ±15% / ±30%, and per QC group the CV% per batch and between batches.
  // Only measured values count for the median, the means, SD and CV%; values replaced by the
  // LOD/LLOQ rules are shown open, values without one ("n.d." / "n.q.") are counted in the title.
  var QC_BAND = 15, QC_LIMIT = 30;
  var QF_CSS = [
    ".qf-bar { display: flex; gap: 12px; align-items: center; flex-wrap: wrap; margin: 4px 0 6px; }",
    ".qf-group { display: inline-flex; border: 1px solid #D0D5DD; border-radius: 8px; overflow: hidden; }",
    ".qf-group button { font: inherit; font-size: .9rem; border: 0; background: #fff; padding: 5px 12px; cursor: pointer; color: #1F2A44; }",
    ".qf-group button + button { border-left: 1px solid #D0D5DD; }",
    ".qf-group button.on { background: #1A0DF2; color: #fff; }",
    ".qf-chips { font-size: .9rem; }",
    ".qf-chip { display: inline-block; background: #EEF4FE; border-radius: 4px; padding: 1px 7px; margin: 0 6px 3px 0; }",
    ".qf-chip.warn { background: #FDECEA; color: #c23636; }"
  ].join("\n");
  function addStyle() {
    if (document.getElementById("qf-style")) return;
    var s = document.createElement("style");
    s.id = "qf-style";
    s.textContent = QF_CSS;
    document.head.appendChild(s);
  }
  var QC_MODES = { control: "Control chart", batch: "Per batch" };
  var QC_SYMBOLS = ["circle", "square", "diamond", "triangle-up", "triangle-down", "cross", "star", "hexagon"];

  function mean(a) { return a.reduce(function (s, x) { return s + x; }, 0) / a.length; }
  function sd(a) { if (a.length < 2) return NaN; var m = mean(a); return Math.sqrt(a.reduce(function (s, x) { return s + (x - m) * (x - m); }, 0) / (a.length - 1)); }
  function median(a) { var s = a.slice().sort(function (x, y) { return x - y; }), n = s.length; return n ? (n % 2 ? s[(n - 1) / 2] : (s[n / 2 - 1] + s[n / 2]) / 2) : NaN; }
  function pct(x) { return finite(x) ? x.toFixed(1) + "%" : "–"; }
  function byNum(a, b) { return String(a).localeCompare(String(b), undefined, { numeric: true }); }

  // The parts of the 4.6 element: the mode buttons, the chips and the plot (made once)
  function qcParts(el) {
    if (el._qfParts) return el._qfParts;
    addStyle();
    el.innerHTML = "";
    var bar = document.createElement("div"), chips = document.createElement("div"), plot = document.createElement("div");
    bar.className = "qf-bar"; chips.className = "qf-chips"; plot.className = "qf-plot";
    var grp = document.createElement("div");
    grp.className = "qf-group";
    Object.keys(QC_MODES).forEach(function (k) {
      var b = document.createElement("button");
      b.type = "button";
      b.textContent = QC_MODES[k];
      b.dataset.key = k;
      b.addEventListener("click", function () { el._qfMode = k; if (el._qfRedraw) el._qfRedraw(); });
      grp.appendChild(b);
    });
    bar.appendChild(grp);
    bar.appendChild(chips);
    el.appendChild(bar);
    el.appendChild(plot);
    el._qfParts = { bar: bar, chips: chips, plot: plot, buttons: grp.querySelectorAll("button") };
    return el._qfParts;
  }
  function qcMessage(el, text) {
    var p = qcParts(el);
    p.bar.style.visibility = "hidden";
    el._qfRedraw = null;
    message(p.plot, text, 440);
  }

  function qcConc(el, d, id) {
    if (!d || id === null || id === undefined) { qcMessage(el, "Select a chemical in table 4.2 (left) to show it here."); return; }
    var v = [], nv = {}, any = false;
    (d.qc || []).forEach(function (part) {
      var r = valuesOf(d, part, id);
      if (!r) return;
      any = true;
      v = v.concat(r.v);
      Object.keys(r.noValue).forEach(function (f) { nv[f] = (nv[f] || 0) + r.noValue[f]; });
    });
    if (!any) {
      qcMessage(el, d.samples && !d.samples.c[id] ? "No concentrations: the chemical was not calibrated." : "No QC samples");
      return;
    }
    if (!v.length && !Object.keys(nv).length) { qcMessage(el, "No concentrations in the QC samples"); return; }
    var p = qcParts(el);
    p.bar.style.visibility = "";
    el._qfRedraw = function () { qcDraw(el, d, id, v, nv); };
    whenPlotly(el._qfRedraw);
  }

  function qcDraw(el, d, id, v, nv) {
    var p = qcParts(el), mode = el._qfMode || "control";
    p.buttons.forEach(function (b) { b.classList.toggle("on", b.dataset.key === mode); });
    v = v.slice().sort(function (a, b) { return a.inj - b.inj; });
    var groups = Object.keys(d.qc_colors).filter(function (g) { return v.some(function (x) { return x.cls === g; }); });
    v.forEach(function (x) { if (groups.indexOf(x.cls) < 0) groups.push(x.cls); });
    var batches = [];
    v.forEach(function (x) { if (batches.indexOf(x.batch) < 0) batches.push(x.batch); });
    batches.sort(byNum);
    var measured = function (x) { return x.kind === "Measured"; };
    // The median of each QC group (measured values), and the deviation of each QC from it
    var med = {};
    groups.forEach(function (g) { med[g] = median(v.filter(function (x) { return x.cls === g && measured(x); }).map(function (x) { return x.y; })); });
    v.forEach(function (x) { x.dev = finite(med[x.cls]) && med[x.cls] > 0 ? 100 * (x.y / med[x.cls] - 1) : null; });

    // Chips: QCs beyond the limits; per QC group the CV% per batch and between the batch means
    var m = v.filter(function (x) { return measured(x) && finite(x.dev); });
    var n15 = m.filter(function (x) { return Math.abs(x.dev) > QC_BAND; }).length;
    var n30 = m.filter(function (x) { return Math.abs(x.dev) > QC_LIMIT; }).length;
    var chip = function (html, warn) { return '<span class="qf-chip' + (warn ? " warn" : "") + '">' + html + "</span>"; };
    var html = [chip("Beyond ±" + QC_BAND + "%: <b>" + n15 + " / " + m.length + "</b>", false),
                chip("Beyond ±" + QC_LIMIT + "%: <b>" + n30 + "</b>", n30 > 0)];
    var stats = {}, between = {};
    groups.forEach(function (g) {
      stats[g] = {};
      var means = [];
      batches.forEach(function (b) {
        var ys = v.filter(function (x) { return x.cls === g && x.batch === b && measured(x); }).map(function (x) { return x.y; });
        var s = { n: ys.length, mean: ys.length ? mean(ys) : NaN, sd: sd(ys) };
        s.cv = 100 * s.sd / s.mean;
        stats[g][b] = s;
        if (finite(s.mean)) means.push(s.mean);
      });
      between[g] = means.length > 1 ? 100 * sd(means) / mean(means) : NaN;
    });
    // The CV% per batch and between batches: one chip per QC group, or with more than two groups
    // one chip with their median (each group in the hover text of the chip)
    var cvText = function (cv, bt) {
      return "CV per batch " + batches.map(function (b) { return esc(b) + ": <b>" + pct(cv[b]) + "</b>"; }).join(", ") +
             (batches.length > 1 ? " · between batches <b>" + pct(bt) + "</b>" : "");
    };
    var worstOf = function (vals) { return Math.max.apply(null, vals.filter(finite).concat([0])); };
    var cvOf = function (g) { var o = {}; batches.forEach(function (b) { o[b] = stats[g][b].cv; }); return o; };
    var withCv = groups.filter(function (g) { return batches.some(function (b) { return finite(stats[g][b].cv); }); });
    if (withCv.length <= 2) {
      withCv.forEach(function (g) {
        var cv = cvOf(g);
        html.push(chip((groups.length > 1 ? esc(g) + " · " : "") + cvText(cv, between[g]),
                       worstOf(Object.keys(cv).map(function (b) { return cv[b]; }).concat([between[g]])) > QC_BAND));
      });
    } else {
      var medCv = {};
      batches.forEach(function (b) { medCv[b] = median(withCv.map(function (g) { return stats[g][b].cv; }).filter(finite)); });
      var medBt = median(withCv.map(function (g) { return between[g]; }).filter(finite));
      var tip = withCv.map(function (g) {
        return g + ": " + batches.map(function (b) { return b + " " + pct(stats[g][b].cv); }).join(", ") +
               (batches.length > 1 ? "; between " + pct(between[g]) : "");
      }).join("\n");
      var worst = worstOf(withCv.map(function (g) { return worstOf(batches.map(function (b) { return stats[g][b].cv; }).concat([between[g]])); }));
      html.push('<span class="qf-chip' + (worst > QC_BAND ? " warn" : "") + '" title="' + esc(tip) + '" style="cursor:help;">Median of ' +
                withCv.length + " QC groups · " + cvText(medCv, medBt) + "</span>");
    }
    if (!withCv.length) html.push(chip("CV: too few measured QCs", false));
    p.chips.innerHTML = html.join("");

    var note = noValueNote(nv);
    var title = { text: esc(nameOf(d, id)) + (note ? "<br><sup>" + esc(note) + "</sup>" : ""), x: 0.02 };
    var sym = {};
    groups.forEach(function (g, i) { sym[g] = QC_SYMBOLS[i % QC_SYMBOLS.length]; });
    var hover = function (x) {
      return "<b>" + esc(x.name) + "</b><br>" + esc(x.cls) + " · batch " + esc(x.batch) + " · inj. " + x.inj +
             "<br>conc. " + num(x.y) + " (" + esc(x.kind) + ")" +
             (finite(x.dev) ? "<br>" + (x.dev >= 0 ? "+" : "") + x.dev.toFixed(1) + "% from the " + esc(x.cls) + " median" : "");
    };
    var pointTraces = function (xOf, yOf, keep) {
      var out = [];
      groups.forEach(function (g) {
        var col = d.qc_colors[g] || "#999999";
        [false, true].forEach(function (replaced) {
          var k = v.filter(function (x) { return x.cls === g && keep(x) && (!measured(x)) === replaced; });
          if (!k.length) return;
          out.push({ type: "scatter", mode: "markers", name: g, legendgroup: g, showlegend: !replaced,
                     x: k.map(xOf), y: k.map(yOf), text: k.map(hover), hoverinfo: "text",
                     marker: replaced ? { color: col, size: 9, symbol: sym[g] + "-open", line: { width: 1.6 } }
                                      : { color: col, size: 8, symbol: sym[g], line: { color: "#fff", width: 1 } } });
        });
      });
      if (v.some(function (x) { return !measured(x); })) {
        out.push({ type: "scatter", mode: "markers", x: [null], y: [null], name: "Replaced (LOD/LLOQ rules)",
                   marker: { color: "#555", size: 9, symbol: "circle-open", line: { width: 1.6 } }, hoverinfo: "skip" });
      }
      return out;
    };
    var layout = { height: 440, font: { family: "Lato" }, hovermode: "closest", title: title, margin: { t: 60, r: 20, b: 60, l: 70 },
                   legend: { orientation: "v", x: 1.02, y: 1 } };

    if (mode === "control") {
      // The batches: shaded alternately, from halfway between the last QC of a batch and the first
      // of the next
      var span = {};
      v.forEach(function (x) { var s = span[x.batch] || (span[x.batch] = [x.inj, x.inj]); s[0] = Math.min(s[0], x.inj); s[1] = Math.max(s[1], x.inj); });
      var order = batches.slice().sort(function (a, b) { return span[a][0] - span[b][0]; });
      var edges = order.map(function (b, i) {
        var lo = i ? (span[order[i - 1]][1] + span[b][0]) / 2 : span[b][0] - 2;
        var hi = i < order.length - 1 ? (span[b][1] + span[order[i + 1]][0]) / 2 : span[b][1] + 2;
        return [lo, hi];
      });
      var shapes = [], ann = [];
      order.forEach(function (b, i) {
        if (order.length > 1 && i % 2 === 0) {
          shapes.push({ type: "rect", xref: "x", yref: "paper", x0: edges[i][0], x1: edges[i][1], y0: 0, y1: 1, layer: "below",
                        fillcolor: "rgba(16,24,40,0.05)", line: { width: 0 } });
        }
        if (order.length > 1) ann.push({ x: (edges[i][0] + edges[i][1]) / 2, y: 1, xref: "x", yref: "paper", yanchor: "bottom",
                                         text: "Batch " + esc(b), showarrow: false, font: { size: 11, color: "#667085" } });
      });
      var x0 = edges[0][0], x1 = edges[edges.length - 1][1];
      shapes.push({ type: "rect", xref: "x", yref: "y", x0: x0, x1: x1, y0: -QC_BAND, y1: QC_BAND, layer: "below",
                    fillcolor: "rgba(0,158,115,0.12)", line: { width: 0 } });
      [-QC_LIMIT, QC_LIMIT].forEach(function (y) {
        shapes.push({ type: "line", xref: "x", yref: "y", x0: x0, x1: x1, y0: y, y1: y, line: { color: "#888780", width: 1, dash: "dash" } });
      });
      shapes.push({ type: "line", xref: "x", yref: "y", x0: x0, x1: x1, y0: 0, y1: 0, line: { color: "#52514e", width: 1 } });
      // Trend: the mean of the deviations of the 5 measured QCs before and after, within each batch
      var traces = [];
      var tx = [], ty = [];
      order.forEach(function (b, bi) {
        var mb = m.filter(function (x) { return x.batch === b; });
        mb.forEach(function (x, i) {
          var w = mb.slice(Math.max(0, i - 5), i + 6).map(function (q) { return q.dev; });
          tx.push(x.inj); ty.push(mean(w));
        });
        if (bi < order.length - 1) { tx.push(null); ty.push(null); }   // A gap between the batches
      });
      if (m.length > 2) traces.push({ type: "scatter", mode: "lines", name: "Trend", x: tx, y: ty, hoverinfo: "skip",
                                      line: { color: "#6250d6", width: 2 }, connectgaps: false });
      // The QCs beyond ±30% ringed in red
      var out = m.filter(function (x) { return Math.abs(x.dev) > QC_LIMIT; });
      if (out.length) traces.push({ type: "scatter", mode: "markers", name: "Beyond ±" + QC_LIMIT + "%", x: out.map(function (x) { return x.inj; }),
                                    y: out.map(function (x) { return x.dev; }), hoverinfo: "skip",
                                    // An open symbol takes its line colour from `color`
                                    marker: { color: "#e34948", size: 17, symbol: "circle-open", line: { width: 1.6 } } });
      traces = traces.concat(pointTraces(function (x) { return x.inj; }, function (x) { return x.dev; }, function (x) { return finite(x.dev); }));
      var ext = Math.max(QC_LIMIT + 10, Math.max.apply(null, v.filter(function (x) { return finite(x.dev); }).map(function (x) { return Math.abs(x.dev); })) * 1.08);
      layout.margin.t = order.length > 1 ? 75 : 60;
      layout.xaxis = { title: { text: "Injection order" }, range: [x0, x1], zeroline: false };
      layout.yaxis = { title: { text: "Deviation from the QC median (%)" }, range: [-ext, ext], zeroline: false, ticksuffix: "%" };
      layout.shapes = shapes;
      layout.annotations = ann;
      react(p.plot, traces, layout);
    } else {
      // Per batch: a column per batch, the QC groups side by side in it
      var ng = groups.length, wdt = 0.7 / ng;
      var xpos = function (b, g) { return batches.indexOf(b) + 1 - 0.35 + wdt * (groups.indexOf(g) + 0.5); };
      var jit = function (j) { return ((j * 0.618034) % 1 - 0.5) * wdt * 0.6; };
      var idxOf = {};
      v.forEach(function (x, j) { idxOf[x.name + "|" + x.inj] = j; });
      var traces = pointTraces(function (x) { return xpos(x.batch, x.cls) + jit(idxOf[x.name + "|" + x.inj]); }, function (x) { return x.y; },
                               function () { return true; });
      var shapes = [], ann = [];
      groups.forEach(function (g) {
        var col = d.qc_colors[g] || "#999999";
        var mx = [], my = [], me = [];
        batches.forEach(function (b) {
          var s = stats[g][b];
          if (!finite(s.mean)) return;
          mx.push(xpos(b, g)); my.push(s.mean); me.push(finite(s.sd) ? s.sd : 0);
          if (finite(s.cv)) ann.push({ x: xpos(b, g), y: s.mean + (finite(s.sd) ? s.sd : 0), xref: "x", yref: "y", yanchor: "bottom", yshift: 4,
                                       text: "CV " + pct(s.cv), showarrow: false, font: { size: ng > 3 ? 10 : 11, color: s.cv > QC_BAND ? "#c23636" : "#52514e" } });
        });
        traces.push({ type: "scatter", mode: "markers", name: g + " mean ± SD", legendgroup: g, showlegend: false, x: mx, y: my,
                      error_y: { type: "data", array: me, visible: true, color: col, thickness: 1.5, width: 0 },
                      marker: { color: col, size: Math.max(14, 50 * wdt), symbol: "line-ew-open", line: { color: col, width: 3 } },
                      hovertext: my.map(function (y, i) { return esc(g) + ": mean " + num(y) + " ± " + num(me[i]); }), hoverinfo: "text" });
        var all = v.filter(function (x) { return x.cls === g && measured(x); }).map(function (x) { return x.y; });
        if (all.length) shapes.push({ type: "line", xref: "paper", yref: "y", x0: 0, x1: 1, y0: mean(all), y1: mean(all),
                                      line: { color: col, width: 1, dash: "dash" } });
      });
      // Only this view has the Log / Linear buttons: their last choice (`react()` keeps it in `_qfLog`)
      var log = !!p.plot._qfLog;
      layout.xaxis = { tickvals: batches.map(function (b, i) { return i + 1; }), ticktext: batches.map(function (b) { return "Batch " + esc(b); }),
                       range: [0.5, batches.length + 0.5], zeroline: false };
      layout.yaxis = { title: { text: d.y_title }, type: log ? "log" : "linear", exponentformat: "E" };
      layout.shapes = shapes;
      layout.annotations = ann;
      layout.margin.t = 70;
      layout.updatemenus = logButtons(p.plot, log);
      react(p.plot, traces, layout);
    }
  }

  // The name of a compound (`names` of the data: id -> name)
  function nameOf(d, id) { return (d.names && d.names[id]) || id; }

  global.QuantaFigs = {
    isDistribution: isDistribution, isRsdTable: isRsdTable, concByClass: concByClass, qcConc: qcConc, message: message,
    // Fit a plot to its width (4.6 has its plot inside)
    redraw: function (el) {
      var p = el && (el.querySelector(".qf-plot") || el);
      if (p && p.data && typeof Plotly !== "undefined") Plotly.Plots.resize(p);
    }
  };
})(window);
