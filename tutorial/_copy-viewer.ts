// Before each render of the tutorial (`pre-render` in _quarto.yml): the calibration curve viewer
// (app/www/calcurve_viewer.js) and the figures drawn in the browser (app/www/quanta_figs.js, 4.6)
// of the app copied to assets/, so that the tutorial always shows those of the app, also on pages
// stored in _freeze/ (they link the files, not copies of them).
// TypeScript, run by Quarto itself: works where R is not installed (GitHub).
Deno.mkdirSync("assets", { recursive: true });
Deno.copyFileSync("../app/www/calcurve_viewer.js", "assets/calcurve_viewer.js");
Deno.copyFileSync("../app/www/quanta_figs.js", "assets/quanta_figs.js");
