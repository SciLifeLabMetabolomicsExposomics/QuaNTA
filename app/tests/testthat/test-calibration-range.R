# The rules that decide whether a compound gets a calibration curve and over which range (LOD, LLOQ,
# the levels of the curve), blank subtraction, what counts as quantified, and the cap of quadratic
# curves. Made-up data; each case is one that was wrong once or is easy to break.
#
# Run from the project root: Rscript -e "testthat::test_dir('app/tests/testthat')"

options(box.path = normalizePath(file.path(testthat::test_path(), "..", "..")))
box::use(SumExp, proc = projlib/proc, msdial = projlib/msdial, show = projlib/show)

# Calibration levels (2 replicates each), Cal0 (2 replicates) and study samples, one batch
cal_levels <- c(1, 2, 5, 10, 20)
#' @param cal Per compound: the 10 signals of the levels (2 per level, low to high)
#' @param cal0 Per compound: the 2 signals of Cal0
#' @param samples Per compound: the signals of the study samples
mini_calib <- function(cal, cal0, samples) {
  ids <- names(cal)
  n_s <- length(samples[[1]])
  conc <- c(0, 0, rep(cal_levels, each = 2), rep(NA, n_s))
  cd <- data.frame(.ctrl_cat = c(rep("CalCurve", 12), rep("", n_s)), c_conc = conc, batch_id = "1",
                   Class = c(rep("Cal", 12), rep("S", n_s)), row.names = paste0("i", seq_along(conc)))
  m <- t(vapply(ids, \(id) c(cal0[[id]], cal[[id]], samples[[id]]), numeric(length(conc))))
  dimnames(m) <- list(ids, rownames(cd))
  SumExp::SumExp(sig = m, col_df = cd, row_df = data.frame(feature_name = ids, row.names = ids))
}
# Two replicates per level around the given mean signals (2% apart)
reps <- function(means) rep(means, each = 2) * rep(c(0.98, 1.02), length(means))
limits <- function(se, rsd20 = FALSE, enough = Inf) {
  SumExp::row_df(proc$find_calib_lim_pts_and_llox_from_llox_signal(
    se, "sig", proc$compute_llox_signal_using_mean_times, rsd20, min_n = 3, enough_n = enough))
}

lin <- 100 * cal_levels   # A clean linear response: mean signals 100 ... 2000
se <- mini_calib(
  cal = list(
    A = reps(lin),                              # Clean
    B = reps(lin),                              # Cal0 high: LLOQ at level 10, too few levels above
    C = reps(c(100, 200, 500, 1000, 950)),      # Top level dips (saturation)
    D = replace(reps(lin), 1:2, c(50, 150)),    # Lowest level with an RSD% of about 70%
    E = reps(lin)                               # No sample reaches the LLOQ
  ),
  cal0 = list(A = c(4, 6), B = c(45, 55), C = c(4, 6), D = c(4, 6), E = c(4, 6)),
  samples = list(A = c(150, 450, 700), B = c(1500, 1500, 1500), C = c(150, 450, 700),
                 D = c(150, 450, 700), E = c(20, 30, 40))
)

test_that("the rising-signal check starts at the low end and ignores a top that flattens or dips", {
  expect_equal(proc$find_increasing_start(c(10, 20, 40, 80, 78)), 1)    # Top dips: all levels kept
  expect_equal(proc$find_increasing_start(c(30, 10, 40, 80, 160)), 2)   # Noisy low end: cut
  expect_equal(proc$find_increasing_start(c(10, 20, 40, 80, 160)), 1)
})

test_that("LOD and LLOQ: the lowest levels above 3x and 10x the mean signal of Cal0", {
  rd <- limits(se)
  expect_equal(rd["A", "lod"], 1)
  expect_equal(rd["A", "lloq"], 1)
  # Cal0 mean 50: LOD signal 150 (level 2), LLOQ signal 500 (level 10; level 5 is not above it)
  expect_equal(rd["B", "lod"], 2)
  expect_equal(rd["B", "lloq"], 10)
})

test_that("a top level that dips does not remove the lower levels of the curve", {
  rd <- limits(se)
  expect_equal(rd["C", "lloq"], 1)
  expect_equal(rd["C", "max_c_conc"], 20)   # A valid range over all levels (the ULOQ handles the top)
})

test_that("with the RSD% option, a level with an RSD% above 20% cannot be the LLOQ", {
  expect_equal(limits(se)["D", "lloq"], 1)
  expect_equal(limits(se, rsd20 = TRUE)["D", "lloq"], 2)
})

test_that("a curve needs 3 levels from the LLOQ upwards", {
  rd <- limits(se)
  expect_true(is.na(rd["B", "max_c_conc"]))
  expect_match(rd["B", "range_issue"], "Only 2 calibration level")
})

test_that("the top of the curve: all levels, or with Optimize the level above the highest sample", {
  expect_equal(limits(se)["A", "max_c_conc"], 20)                # Optimize off: the whole range
  expect_equal(limits(se, enough = 3)["A", "max_c_conc"], 10)    # First level above the sample 700
  expect_equal(limits(se, enough = 5)["A", "max_c_conc"], 20)    # Extended to the wished 5 levels
})

test_that("no sample at the LLOQ: the full calibration range is shown", {
  rd <- limits(se, enough = 3)
  expect_true(rd["E", "no_quant_samples"])
  expect_equal(rd["E", "max_c_conc"], 20)
  expect_false(rd["A", "no_quant_samples"])
})

test_that("blank subtraction: the blanks of the batch, all blanks for a batch without, standards unchanged", {
  cd <- data.frame(
    .ctrl_cat = c("Blank", "Blank", "", "", "CalCurve", "", "Blank", ""),
    batch_id = c("1", "1", "1", "1", "1", "2", "3", "3"),
    row.names = paste0("i", 1:8)
  )
  m <- matrix(c(10, 30, 100, 5, 100, 100, 50, 100), nrow = 1, dimnames = list("f", rownames(cd)))
  blk_se <- SumExp::SumExp(x = m, col_df = cd, row_df = data.frame(feature_name = "f", row.names = "f"))
  out <- proc$add_blank_subtracted_sumexp(blk_se, no_change = cd$.ctrl_cat == "CalCurve",
                                          mat_ids = "x", out_mat_ids = "x_blk")
  expect_equal(colnames(out), c("i3", "i4", "i5", "i6", "i8"))   # The blanks are removed
  # Batch 1: blank mean 20; batch 2 (no blanks): mean of all blanks, 30; batch 3: 50; at least 0
  expect_equal(unname(out[["x_blk"]][1, ]), c(80, 0, 100, 70, 50))
  # Option off: nothing subtracted
  off <- proc$add_blank_subtracted_sumexp(blk_se, no_change = cd$.ctrl_cat == "CalCurve",
                                          mat_ids = "x", out_mat_ids = "x_blk", subtract = FALSE)
  expect_equal(unname(off[["x_blk"]][1, ]), c(100, 5, 100, 100, 100))
})

test_that("quantified: a measured concentration at or above the LLOQ, not a value set to it", {
  cd <- data.frame(.ctrl_cat = c("", "", "", "", "QC"), row.names = paste0("s", 1:5))
  conc0 <- matrix(c(3, 0.8, 1, 0.2, 5), nrow = 1, dimnames = list("f", rownames(cd)))
  conc <- matrix(c(3, 1, 1, 0.25, 5), nrow = 1, dimnames = list("f", rownames(cd)))   # After the LOD/LLOQ rules
  q <- msdial$measured_conc(SumExp::SumExp(conc = conc, conc0 = conc0, col_df = cd,
                                           row_df = data.frame(lloq = 1, row.names = "f")))
  # Measured 3 and exactly the LLOQ count; set to the LLOQ and LLOQ/4 do not; the QC is left out
  expect_equal(unname(q[1, ]), c(3, NA, 1, NA))
})

test_that("a quadratic curve that bends down caps the signals above its peak at the peak's concentration", {
  conc <- rep(c(1, 2, 4, 6, 8), each = 2)
  down <- proc$fit_and_test_calcurve_model(conc, -conc^2 + 20 * conc)   # Peak at 10 (signal 100)
  expect_match(down$best_model_name, "^quadratic")
  cap <- proc$quadratic_cap(down)
  expect_equal(cap, 10)
  expect_equal(down$best_model(c(150, 1000)), c(cap, cap))   # Above the peak: all the same
  expect_lt(down$best_model(50), cap)                         # Below the peak: on the rising side
  up <- proc$fit_and_test_calcurve_model(conc, conc^2 + 20 * conc)
  expect_equal(proc$quadratic_cap(up), Inf)                   # Bends up: no cap
  linear <- proc$fit_and_test_calcurve_model(conc, 5 * conc, linear_only = TRUE)
  expect_true(is.na(proc$quadratic_cap(linear)))              # Not quadratic
  # Capped values have their own colour in the app and the reports
  expect_equal(as.character(show$imputed_kind(c(5, cap, cap), lloq = 1, cap = cap)),
               c("Measured", "Capped (peak of the curve)", "Capped (peak of the curve)"))
})
