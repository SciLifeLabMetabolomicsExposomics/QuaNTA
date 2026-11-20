source(file.path("..", "..", "scripts", "embed_html_resources.R"), local = TRUE)

test_that("only files inside the report folder are embedded", {
  root <- tempfile("embed")
  on.exit(unlink(root, recursive = TRUE))
  dir.create(file.path(root, "report"), recursive = TRUE)
  dir.create(file.path(root, "inline"))
  writeLines("SECRET", file.path(root, "secret.txt"))
  writeLines("var ok = 1;", file.path(root, "report", "ok.js"))
  writeLines("var big = 1;", file.path(root, "inline", "big.js"))
  html <- file.path(root, "report", "page.html")
  writeLines(c(
    '<script src="ok.js"></script>',
    '<img src="../secret.txt">',
    sprintf('<script data-quanta-inline="%s"></script>', normalizePath(file.path(root, "secret.txt"), "/")),
    sprintf('<script data-quanta-inline="%s"></script>', normalizePath(file.path(root, "inline", "big.js"), "/"))
  ), html)

  embed_html_resources(html, inline_dir = file.path(root, "inline"))
  out <- paste(readLines(html), collapse = "\n")
  expect_false(grepl("SECRET", out) || grepl(jsonlite::base64_enc(charToRaw("SECRET")), out, fixed = TRUE))
  expect_match(out, '<img src="../secret.txt">', fixed = TRUE)     # Left as it was
  expect_match(out, "var big = 1;", fixed = TRUE)
  expect_match(out, "data:application/javascript;base64,", fixed = TRUE)
})
