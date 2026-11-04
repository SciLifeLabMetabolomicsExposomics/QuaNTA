// Before each render of the tutorial (`pre-render` in _quarto.yml): the calibration curve viewer of
// the app (app/www/calcurve_viewer.js) copied to assets/, so that the tutorial always shows the
// viewer of the app, also on pages stored in _freeze/ (they link the file, not a copy of it).
// TypeScript, run by Quarto itself: works where R is not installed (GitHub).
Deno.mkdirSync("assets", { recursive: true });
Deno.copyFileSync("../app/www/calcurve_viewer.js", "assets/calcurve_viewer.js");
