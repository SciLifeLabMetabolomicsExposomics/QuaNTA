# The calibration rules that decide the reported concentrations: detection (LOD), the LOD/LLOQ
# replacements and the ULOQ. Each case is one that was wrong once.
#
# Run from the project root: Rscript -e "testthat::test_dir('app/tests/testthat')"

options(box.path = normalizePath(file.path(testthat::test_path(), "..", "..")))
box::use(SumExp, proc = projlib/proc, util = projlib/msdial_utils)

# One chemical (LLOQ 1, LOD signal 10, mean signal of the LLOQ level 100) in five samples
one_chemical <- function(signal, conc) {
  se <- SumExp::SumExp(
    conc = matrix(conc, 1, dimnames = list("c1", paste0("s", seq_along(conc)))),
    sig = matrix(signal, 1, dimnames = list("c1", paste0("s", seq_along(conc)))),
    row_df = data.frame(lloq = 1, lod_signal = 10, avg_signal_at_lloq = 100, row.names = "c1"),
    col_df = data.frame(row.names = paste0("s", seq_along(conc)))
  )
  util$save_src_mat_id_for_conc(se, "sig")
}

test_that("a signal of 0 is never detected, also when the LOD signal is 0 (no signal in Cal0)", {
  expect_equal(util$is_detected(c(0, 0.1, 5), 0), c(FALSE, TRUE, TRUE))
  expect_equal(util$is_detected(c(9, 10), 10), c(FALSE, TRUE))   # At the LOD signal: detected
})

test_that("values below the LOD / LLOQ are replaced by LLOQ/4, LLOQ/2 and the LLOQ", {
  # Signals: none, below the LOD, below the LLOQ level, above it with a low concentration, measured
  se <- one_chemical(signal = c(0, 5, 50, 200, 200), conc = c(0.1, 0.2, 0.5, 0.6, 3))
  out <- proc$apply_llox_rules(se, conc_mat_id = "conc")[["conc"]][1, ]
  expect_equal(unname(out), c(0.25, 0.25, 0.5, 1, 3))
})

test_that("log-scale fitting is a straight line in log-log scale, also for small signals", {
  # Normalized signals well below 1 (where log(x + 1) would be about x, not a log)
  cc <- rep(c(0.1, 0.5, 1, 5, 10, 50), each = 2)
  sig <- 0.02 * cc^0.9 * rep(c(0.98, 1.02), 6)
  fit <- proc$fit_and_test_calcurve_model(cc, sig, weight_method = "largestR2", log_log = TRUE)
  expect_equal(fit$best_model_name, "linear-1")   # No quadratic model, weight 1
  expect_gt(fit$R2s[["linear-1"]], 0.999)
  expect_equal(fit$best_model(0.02 * c(0.1, 10, 50)^0.9), c(0.1, 10, 50), tolerance = 0.02)
  expect_equal(fit$best_model(c(0, -1, NA)), c(0, 0, NA))   # No signal: concentration 0
})

# Linear up to 0.2, then the response flattens (two replicates per level)
conc <- rep(c(0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1, 2, 5), each = 2)
signal <- 1e6 * ifelse(conc <= 0.2, conc, 0.2 + (conc - 0.2)^0.5 * 0.15) * rep(c(0.99, 1.01), 9)

test_that("the ULOQ of a linear curve is the top of its linear range, whatever the minimum number of levels", {
  fit <- proc$fit_and_test_calcurve_model(conc, signal, weight_method = "1_div_x", linear_only = TRUE)
  for (min_n in c(3, 6)) {
    lr <- proc$trim_to_linear_range(conc, signal, fit, max_dev = 0.15, min_n = min_n, weight_method = "1_div_x")
    expect_equal(lr$uloq, 0.2)
    # A cut curve keeps the minimum number of levels: it ends above the ULOQ
    expect_equal(lr$top, if (min_n == 6) 0.5 else 0.2)
  }
})

test_that("the ULOQ by SCALiR (log-scale fitting) does not depend on the minimum number of levels", {
  refit <- function(s) proc$fit_and_test_calcurve_model(conc, s, log_log = TRUE)
  for (min_n in c(3, 6)) {
    lr <- proc$linear_range_of_log_curve(conc, signal, signal, refit(signal), min_n = min_n, refit = refit)
    expect_equal(lr$uloq, 0.2)
    expect_equal(lr$top, if (min_n == 6) 0.5 else 0.2)
  }
})
