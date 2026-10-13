# ------------------------------------------------------------------------------------------- #
# Create the synthetic QuaNTA example dataset (MS-DIAL alignment export format)
#
# Calibration standards, QCs and blanks are based on a real ESI+ targeted exposomics run
# (5 batches with one calibration curve each), collapsed into 3 batches:
#   - batch 1 = original batches 2 + 3, batch 2 = original batches 4 + 5 (all real injections)
#   - batch 3 = original batch 1 + a synthetic copy of each of its injections (with noise and
#     outliers)
#   - plus a third, synthetic injection of the calibration curve in every batch (mid-batch)
# Everything else is synthetic:
#   - 120 samples (60 "Healthy", 60 "Sick"; 20 + 20 per batch), no real samples are included
#   - 45 common serum metabolites (targeted, with calibration curves; 2 not detectable)
#   - 1000 non-targeted features ("Unknown")
#   - group differences (20 targeted, 97 non-targeted features), injection-order drift,
#     matrix effects, batch effects (strong for some features), outliers, 4 failed injections
#     (outlier samples), a failing internal standard, blank contaminants, noisy features in QCs
#     and missing values (zeros)
#
# The source file is not part of the repository; the script documents how the example was made.
# Usage (from the project root): Rscript example_data/make_example_data.R
# ------------------------------------------------------------------------------------------- #

set.seed(20261006)
src <- "test_data/Expo00027_ESIpos_TARGETS_no obv outliers_noQCspiked_end cal only_no dupl.txt"
out_dir <- "example_data"
out_name <- "QuaNTA_example_ESIpos"

n_feat_cols <- 11L                      # Alignment ID ... MS/MS spectrum
n_nontarget <- 1000L
n_per_group_batch <- 20L               # Per group and batch
batch_sources <- list("1" = c(2, 3), "2" = c(4, 5), "3" = 1)   # Pairs with the most similar curves

# -----  READ THE SOURCE  ------------------------------------------------------------------- #
lines <- readLines(src, warn = FALSE)
split <- strsplit(lines, "\t", fixed = TRUE)
ncol_all <- length(split[[5]])
split <- lapply(split, function(x) c(x, rep("", ncol_all - length(x))))
hdr <- do.call(rbind, split[1:5])                       # Class, File type, Injection order, Batch ID, names
body <- do.call(rbind, split[-(1:5)])
s_idx <- (n_feat_cols + 1L):ncol_all
feat <- body[, 1:n_feat_cols, drop = FALSE]
colnames(feat) <- hdr[5, 1:n_feat_cols]
x <- matrix(as.numeric(body[, s_idx]), nrow(body))
smp <- data.frame(
  class = hdr[1, s_idx], type = hdr[2, s_idx], io = as.integer(hdr[3, s_idx]),
  batch = as.integer(hdr[4, s_idx]), name = hdr[5, s_idx]
)
smp$cal_level <- ifelse(smp$type == "Standard", sub("^Cal([0-9][0-9-]*).*", "\\1", smp$name), NA)
smp$conc <- as.numeric(sub("-", ".", smp$cal_level))
smp$qc_group <- ifelse(smp$type == "QC", sub("_.*", "", smp$name), NA)              # QCall, QC1, ...
smp$blk_group <- ifelse(smp$type == "Blank", sub("_.*", "", smp$name), NA)          # PROCBLK1, ...

comment <- sub("\\\\.*", "", feat[, "Comment"])
is_is <- comment %in% c("IS", "vIS")
n_t <- nrow(feat)
levels_conc <- sort(unique(smp$conc[!is.na(smp$conc)]))

# -----  HELPERS  --------------------------------------------------------------------------- #
lnorm <- function(n, sd, meanlog = 0) exp(stats::rnorm(n, meanlog, sd))
noisy_copy <- function(m, cv = 0.08) m * matrix(lnorm(length(m), cv), nrow(m))

# -----  CONTROL INJECTIONS PER NEW BATCH (targeted features)  ------------------------------- #
# A list per batch of: columns (data frame), values (targeted matrix), injection "A"/"B" for cals
ctrl <- list()
strong_bt_targets <- sample(which(comment == "Quant"), 8)                 # Strong batch 3 effect
strong_bt_factor <- ifelse(stats::runif(8) < 0.5, stats::runif(8, 0.4, 0.6), stats::runif(8, 1.8, 2.5))
for (b in names(batch_sources)) {
  ob <- batch_sources[[b]]
  cols <- which(smp$batch %in% ob & smp$type != "Sample")
  info <- smp[cols, ]
  info$inj <- ifelse(info$batch == ob[1], "A", "B")
  vals <- x[, cols, drop = FALSE]
  if (length(ob) == 1) {                                     # Synthetic copy of every injection
    copy <- noisy_copy(vals)
    copy[, info$type == "QC"] <- copy[, info$type == "QC"] * lnorm(n_t, 0.05)  # Feature-wise shift
    # Outliers: one calibration level of ~10% of the compounds
    cal_cols <- which(info$type == "Standard")
    for (i in sample(which(comment == "Quant"), round(0.1 * sum(comment == "Quant")))) {
      j <- sample(cal_cols, 1)
      copy[i, j] <- copy[i, j] * sample(c(stats::runif(1, 2.5, 4), 0.25), 1)
    }
    # Extreme values in a few QC injections, and carry-over in one blank
    qc_cols <- which(info$type == "QC")
    k <- sample(length(copy[, qc_cols]), round(0.02 * length(copy[, qc_cols])))
    copy[, qc_cols][k] <- copy[, qc_cols][k] * stats::runif(length(k), 3, 8)
    blk_col <- sample(which(info$type == "Blank"), 1)
    cal_mid <- which(info$type == "Standard" & info$conc == 0.5)[1]
    carry <- sample(which(comment == "Quant"), 5)
    copy[carry, blk_col] <- pmax(copy[carry, blk_col], copy[carry, cal_mid]) * stats::runif(5, 1, 3)
    info2 <- info
    info2$inj <- "B"
    info <- rbind(info, info2)
    vals <- cbind(vals, copy)
    # Strong batch effect (all injections of the batch, so batch-wise calibration corrects it)
    vals[strong_bt_targets, ] <- vals[strong_bt_targets, ] * strong_bt_factor
  }
  # Third injection of the calibration curve ("C", mid-batch): a synthetic replicate of the mean
  # curve of the batch
  cal_i <- which(info$type == "Standard")
  first <- cal_i[!duplicated(info$cal_level[cal_i])]
  base <- sapply(first, function(j) {
    rowMeans(vals[, cal_i[info$cal_level[cal_i] == info$cal_level[j]], drop = FALSE])
  })
  info3 <- info[first, ]
  info3$inj <- "C"
  info <- rbind(info, info3)
  vals <- cbind(vals, noisy_copy(base))
  ctrl[[b]] <- list(info = info, vals = vals)
}

# -----  INJECTION SEQUENCE PER BATCH  ------------------------------------------------------ #
# Blank, calibration curve A, blank, then samples with QCs (and remaining blanks) evenly spread,
# then blank, calibration curve B, blank
seq_all <- list()
io_next <- 1L
healthy_no <- 0L
sick_no <- 0L
for (b in names(ctrl)) {
  info <- ctrl[[b]]$info
  info$src_col <- seq_len(nrow(info))
  cal_a <- info[info$type == "Standard" & info$inj == "A", ]
  cal_b <- info[info$type == "Standard" & info$inj == "B", ]
  cal_c <- info[info$type == "Standard" & info$inj == "C", ]
  cal_a <- cal_a[order(cal_a$conc), ]
  cal_b <- cal_b[order(cal_b$conc), ]
  cal_c <- cal_c[order(cal_c$conc), ]
  qcs <- info[info$type == "QC", ]
  qcs <- qcs[sample(nrow(qcs)), ]
  blks <- info[info$type == "Blank", ]
  blks <- blks[sample(nrow(blks)), ]
  groups <- sample(rep(c("Healthy", "Sick"), each = n_per_group_batch))
  samples <- data.frame(class = groups, type = "Sample", src_col = NA_integer_)
  # Middle part: samples with QCs and blanks between them
  n_mid_blk <- nrow(blks) - 4L
  mid <- list(qcs[1, ])
  n_s <- nrow(samples)
  qc_at <- round(seq(0, n_s, length.out = nrow(qcs)))[-1]
  blk_at <- if (n_mid_blk > 0) round(seq(0, n_s, length.out = n_mid_blk + 2))[-c(1, n_mid_blk + 2)] else integer()
  qi <- 2L
  bi <- 3L
  for (s in seq_len(n_s)) {
    mid[[length(mid) + 1]] <- samples[s, ]
    if (s %in% blk_at && bi <= nrow(blks) - 2L) { mid[[length(mid) + 1]] <- blks[bi, ]; bi <- bi + 1L }
    while (qi <= nrow(qcs) && qi - 1L <= sum(qc_at <= s)) { mid[[length(mid) + 1]] <- qcs[qi, ]; qi <- qi + 1L }
    if (s == n_s %/% 2) mid <- c(mid, split(cal_c, seq_len(nrow(cal_c))))   # Mid-batch curve
  }
  pick <- function(d) d[, c("class", "type", "src_col")]
  ord <- rbind(
    pick(blks[1, ]), pick(cal_a), pick(blks[2, ]),
    do.call(rbind, lapply(mid, pick)),
    pick(blks[nrow(blks) - 1L, ]), pick(cal_b), pick(blks[nrow(blks), ])
  )
  stopifnot(sum(!is.na(ord$src_col)) == nrow(info), !anyDuplicated(stats::na.omit(ord$src_col)))
  ord$batch <- as.integer(b)
  ord$io <- seq(io_next, length.out = nrow(ord))
  io_next <- io_next + nrow(ord)
  # Relative position in the batch (for drift)
  ord$pos <- (seq_len(nrow(ord)) - 1) / (nrow(ord) - 1)
  # Names
  ord$name <- NA_character_
  ord$qc_group <- NA_character_
  for (r in seq_len(nrow(ord))) {
    if (ord$type[r] == "Sample") {
      if (ord$class[r] == "Healthy") { healthy_no <- healthy_no + 1L; id <- sprintf("H%03d", healthy_no) }
      else { sick_no <- sick_no + 1L; id <- sprintf("S%03d", sick_no) }
      ord$name[r] <- sprintf("%s_B%s_%d", id, b, ord$io[r])
    } else {
      i <- info[ord$src_col[r], ]
      ord$name[r] <- switch(
        i$type,
        Standard = sprintf("Cal%s_E_B%s_%d", i$cal_level, b, ord$io[r]),
        QC = sprintf("%s_B%s_%d", if (i$qc_group == "QCall") "QCall" else paste0(i$qc_group, "_QC"), b, ord$io[r]),
        Blank = sprintf("%s_B%s_%d", i$blk_group, b, ord$io[r])
      )
      ord$qc_group[r] <- if (i$type == "QC") i$qc_group else NA
    }
  }
  seq_all[[b]] <- ord
}

# -----  TARGETED FEATURES IN SAMPLES  ------------------------------------------------------ #
n_samples <- sum(sapply(seq_all, function(o) sum(o$type == "Sample")))
quant <- which(comment %in% c("Quant", "R"))
# Lowest calibration level clearly above the zero calibrant (all real calibration injections)
cal_cols <- which(smp$type == "Standard")
c_low <- apply(x[, cal_cols], 1, function(v) {
  resp <- tapply(v, smp$conc[cal_cols], mean)
  r0 <- resp[1]
  s0 <- stats::sd(v[smp$conc[cal_cols] == 0])
  above <- resp > r0 + max(5 * s0, 0.2 * r0, 1000)
  i <- which(above & rev(cumsum(rev(!above))) == 0)[1]     # Above from here on
  if (is.na(i)) Inf else as.numeric(names(resp)[i])
})
# Concentrations: most compounds are found in their quantifiable range, some are rarely detected
rare <- stats::runif(n_t) < 0.2 | c_low > 5
med_conc <- exp(stats::runif(n_t, log(pmin(3 * c_low, 5)), log(20)))
sd_conc <- stats::runif(n_t, 0.4, 0.8)
de_targets <- sample(quant[!rare[quant] & c_low[quant] <= 0.5], 10)
de_targets_fold <- stats::runif(10, 1.5, 3)^sample(c(-1, 1), 10, replace = TRUE)

# Matrix effect (ion suppression) per sample and a common drift over the injection order per batch
matrix_supp <- 0.75
drift_rate <- c("1" = 0.25, "2" = 0.15, "3" = 0.35)

sample_vals_t <- list()
sample_bad <- list()
for (b in names(seq_all)) {
  ord <- seq_all[[b]]
  cv <- ctrl[[b]]$vals
  ci <- ctrl[[b]]$info
  is_smp <- ord$type == "Sample"
  n_s <- sum(is_smp)
  grp <- ord$class[is_smp]
  m_s <- matrix_supp * lnorm(n_s, 0.2) * exp(-drift_rate[[b]] * ord$pos[is_smp])
  v <- matrix(0, n_t, n_s)
  # Mean calibration response per level in this batch
  cal <- ci$type == "Standard"
  for (i in seq_len(n_t)) {
    if (is_is[i]) {                                    # Internal standards: as in the calibrants
      base <- stats::median(cv[i, cal])
      v[i, ] <- base * m_s * lnorm(n_s, 0.08)
      next
    }
    resp <- tapply(cv[i, cal], ci$conc[cal], mean)
    conc <- med_conc[i] * lnorm(n_s, sd_conc[i])
    if (rare[i]) conc <- ifelse(stats::runif(n_s) < 0.8, 0, conc * 0.1)
    k <- match(i, de_targets)
    if (!is.na(k)) conc[grp == "Sick"] <- conc[grp == "Sick"] * de_targets_fold[k]
    conc <- pmin(conc, 40)
    sig <- stats::approx(as.numeric(names(resp)), resp, xout = conc, rule = 2)$y
    v[i, ] <- sig * m_s * lnorm(n_s, 0.1)
  }
  # Outliers
  k <- sample(length(v), round(0.01 * length(v)))
  v[k] <- v[k] * stats::runif(length(k), 5, 15)
  # Failed injections: too low or too high signal of all features (also non-targeted ones, via
  # the IS level below); 4 outlier samples in total
  bad <- list("1" = 0.1, "2" = c(0.08, 8), "3" = 0.12)[[b]]
  j <- sample(n_s, length(bad))
  v[, j] <- v[, j] * rep(bad, each = n_t)
  sample_bad[[b]] <- j
  sample_vals_t[[b]] <- v
}

# -----  NON-TARGETED FEATURES (all injections)  -------------------------------------------- #
existing_ids <- as.integer(feat[, "Alignment ID"])
new_ids <- sample(setdiff(0:90000, existing_ids), n_nontarget + 45L)
nt_ids <- sort(new_ids[seq_len(n_nontarget)])
met_ids <- new_ids[-seq_len(n_nontarget)]
nt_mu <- exp(stats::rnorm(n_nontarget, log(2e5), 1.8))
nt_bio_sd <- stats::runif(n_nontarget, 0.15, 0.5)
nt_qc_cv <- stats::runif(n_nontarget, 0.03, 0.15)
nt_noisy <- sample(n_nontarget, round(0.08 * n_nontarget))
nt_qc_cv[nt_noisy] <- stats::runif(length(nt_noisy), 0.3, 0.7)
nt_contam <- sample(n_nontarget, round(0.06 * n_nontarget))
nt_blank_frac <- stats::runif(n_nontarget, 0.005, 0.03)
nt_blank_frac[nt_contam] <- stats::runif(length(nt_contam), 0.4, 1.2)
# Group differences: 50 moderate and 47 strong ones
nt_de <- sample(n_nontarget, 97)
nt_de_fold <- c(stats::runif(50, 1.5, 3), stats::runif(47, 2, 4))^sample(c(-1, 1), 97, replace = TRUE)
qc_groups <- unique(stats::na.omit(smp$qc_group))
nt_qc_level <- sapply(qc_groups, function(g) {
  nt_mu * lnorm(n_nontarget, if (g == "QCall") 0.1 else 0.3)
})
nt_strong_bt <- sample(n_nontarget, round(0.05 * n_nontarget))
nt_batch <- sapply(names(seq_all), function(b) {
  f <- lnorm(n_nontarget, 0.1)
  f[nt_strong_bt] <- lnorm(length(nt_strong_bt), 0.6)
  f
})
# Feature-specific drift on top of the signal level of each injection (corrected by LOESS)
nt_drift_sens <- stats::runif(n_nontarget, -1, 1)

# Targeted features of all injections
vals_t <- lapply(names(seq_all), function(b) {
  ord <- seq_all[[b]]
  v <- matrix(NA_real_, n_t, nrow(ord))
  v[, ord$type != "Sample"] <- ctrl[[b]]$vals[, ord$src_col[ord$type != "Sample"]]
  v[, ord$type == "Sample"] <- sample_vals_t[[b]]
  v
})
names(vals_t) <- names(seq_all)

# Signal level of each injection (matrix effect, drift, failed injection) from its internal
# standards, relative to the calibrants of all batches, so that IS normalization works for the
# synthetic features too. (Relative to each batch's own calibrants, IS normalization would leave
# a different level per batch.)
is_ref <- apply(do.call(cbind, lapply(names(seq_all), function(b) {
  vals_t[[b]][is_is, seq_all[[b]]$type == "Standard", drop = FALSE]
})), 1, stats::median)
ok_ref <- is_ref > 0
g_all <- lapply(vals_t, function(v) {
  exp(apply(log(pmax(v[is_is, , drop = FALSE][ok_ref, , drop = FALSE], 1) / is_ref[ok_ref]), 2, stats::median))
})

nt_vals <- list()
for (b in names(seq_all)) {
  ord <- seq_all[[b]]
  g <- g_all[[b]]
  v <- matrix(0, n_nontarget, nrow(ord))
  drift <- exp(-0.3 * outer(nt_drift_sens, ord$pos))
  for (r in seq_len(nrow(ord))) {
    t <- ord$type[r]
    if (t == "Sample") {
      lvl <- nt_mu * lnorm(n_nontarget, nt_bio_sd)
      if (ord$class[r] == "Sick") lvl[nt_de] <- lvl[nt_de] * nt_de_fold
      v[, r] <- lvl * g[r] * drift[, r]
    } else if (t == "QC") {
      v[, r] <- nt_qc_level[, ord$qc_group[r]] * lnorm(n_nontarget, nt_qc_cv) * g[r] * drift[, r]
    } else {                                     # Blanks and calibrants (solvent): background
      v[, r] <- nt_mu * nt_blank_frac * lnorm(n_nontarget, 0.3)
    }
  }
  v <- v * nt_batch[, b]
  # Outliers in samples
  sc <- which(ord$type == "Sample")
  k <- sample(length(v[, sc]), round(0.005 * length(v[, sc])))
  v[, sc][k] <- v[, sc][k] * stats::runif(length(k), 5, 20)
  # Missing values: low signals are often not detected
  low <- v < 1500 & matrix(stats::runif(length(v)) < 0.7, nrow(v))
  v[low] <- 0
  nt_vals[[b]] <- v
}

# -----  SERUM METABOLITES (synthetic targeted compounds, all injections)  ------------------- #
# Common serum metabolites measured by LC-HRMS (C18 reversed phase, ESI+), with retention times
# typical of such a method. Their calibration curves, QCs and samples are all synthetic.
mets <- utils::read.csv(text = "
name,formula,rt
Creatinine,C4H7N3O,1.02
Creatine,C4H9N3O2,0.95
L-Carnitine,C7H15NO3,0.91
Acetylcarnitine,C9H17NO4,1.21
Propionylcarnitine,C10H19NO4,2.08
Butyrylcarnitine,C11H21NO4,3.42
Isovalerylcarnitine,C12H23NO4,4.61
Hexanoylcarnitine,C13H25NO4,6.03
Octanoylcarnitine,C15H29NO4,7.42
Decanoylcarnitine,C17H33NO4,8.64
Palmitoylcarnitine,C23H45NO4,11.83
Betaine,C5H11NO2,0.88
Valine,C5H11NO2,1.12
Leucine,C6H13NO2,2.31
Isoleucine,C6H13NO2,2.12
Methionine,C5H11NO2S,1.52
Phenylalanine,C9H11NO2,3.58
Tyrosine,C9H11NO3,1.93
Tryptophan,C11H12N2O2,4.81
Proline,C5H9NO2,0.98
Citrulline,C6H13N3O3,0.94
Asymmetric dimethylarginine,C8H18N4O2,0.96
Kynurenine,C10H12N2O3,3.04
Kynurenic acid,C10H7NO3,5.18
Indole-3-propionic acid,C11H11NO2,8.12
Indole-3-acetic acid,C10H9NO2,7.21
Hippuric acid,C9H9NO3,4.93
Phenylacetylglutamine,C13H16N2O4,5.11
Uric acid,C5H4N4O3,1.08
Hypoxanthine,C5H4N4O,1.41
Xanthine,C5H4N4O2,1.33
Paraxanthine,C7H8N4O2,3.71
Theobromine,C7H8N4O2,3.12
Trigonelline,C7H7NO2,0.92
Nicotinamide,C6H6N2O,1.62
Pantothenic acid,C9H17NO5,3.33
Riboflavin,C17H20N4O6,5.62
Serotonin,C10H12N2O,2.41
Cortisone,C21H28O5,8.93
Glycocholic acid,C26H43NO6,10.62
Taurocholic acid,C26H45NO7S,10.14
Glycochenodeoxycholic acid,C26H43NO5,11.58
Cholic acid,C24H40O5,11.21
LysoPC(16:0),C24H50NO7P,12.93
LysoPC(18:1),C26H52NO7P,13.24
", strip.white = TRUE)
mono <- c(C = 12, H = 1.00782503207, N = 14.0030740048, O = 15.99491461956, S = 31.97207100, P = 30.97376163)
mz_mh <- vapply(mets$formula, function(fm) {
  el <- regmatches(fm, gregexpr("[A-Z][a-z]?[0-9]*", fm))[[1]]
  n <- as.numeric(sub("^[A-Z][a-z]?", "", el)); n[is.na(n)] <- 1
  sum(mono[sub("[0-9]+$", "", el)] * n) + 1.00727646688   # [M+H]+
}, numeric(1))
n_m <- nrow(mets)
m_slope <- exp(stats::rnorm(n_m, log(2e7), 1))                    # Signal per ng/mL
m_sat <- ifelse(stats::runif(n_m) < 0.3, stats::runif(n_m, 40, 150), Inf)   # Some saturate
m_bkg <- m_slope * stats::runif(n_m, 0.001, 0.01)                 # Signal of the zero calibrant
m_cv <- stats::runif(n_m, 0.04, 0.10)                             # Replicate variation
m_med <- exp(stats::runif(n_m, log(0.5), log(15)))                # Median concentration in serum
m_bio_sd <- stats::runif(n_m, 0.25, 0.6)
m_nd <- match(c("Riboflavin", "Serotonin"), mets$name)            # Not detectable in serum
m_med[m_nd] <- 0
m_de <- sample(setdiff(seq_len(n_m), m_nd), 10)                   # Clear group differences
m_de_fold <- stats::runif(10, 2, 4)^sample(c(-1, 1), 10, replace = TRUE)
m_qc_conc <- sapply(qc_groups, function(g) m_med * lnorm(n_m, if (g == "QCall") 0.1 else 0.4))
m_resp <- function(k, conc, rb) m_slope[k] * rb * conc / (1 + conc / m_sat[k]) + m_bkg[k]

m_vals <- list()
for (b in names(seq_all)) {
  ord <- seq_all[[b]]
  g <- g_all[[b]]
  info <- ctrl[[b]]$info
  rb <- lnorm(n_m, 0.1)                                         # Batch response
  v <- matrix(0, n_m, nrow(ord))
  for (r in seq_len(nrow(ord))) {
    t <- ord$type[r]
    conc <- switch(t,
      Standard = rep(info$conc[ord$src_col[r]], n_m),
      QC = m_qc_conc[, ord$qc_group[r]],
      Sample = {
        cc <- m_med * lnorm(n_m, m_bio_sd)
        if (ord$class[r] == "Sick") cc[m_de] <- cc[m_de] * m_de_fold
        cc
      },
      Blank = rep(0, n_m)
    )
    v[, r] <- if (t == "Blank") m_bkg * lnorm(n_m, 0.3) else
      vapply(seq_len(n_m), function(k) m_resp(k, conc[k], rb[k]), numeric(1)) * g[r] * lnorm(n_m, m_cv)
  }
  sc <- which(ord$type == "Sample")
  k <- sample(length(v[, sc]), round(0.01 * length(v[, sc])))
  v[, sc][k] <- v[, sc][k] * stats::runif(length(k), 5, 15)
  low <- v < 1000 & matrix(stats::runif(length(v)) < 0.7, nrow(v))
  v[low] <- 0
  m_vals[[b]] <- v
}

# -----  ASSEMBLE AND WRITE  ---------------------------------------------------------------- #
ord_all <- do.call(rbind, seq_all)
vals_t <- do.call(cbind, vals_t)
# A second failed internal standard: not detected in ~20% of the injections
is_fail2 <- which(feat[, "Metabolite name"] == "Progesterone-d9_IS")
vals_t[is_fail2, sample(ncol(vals_t), round(0.2 * ncol(vals_t)))] <- 0
vals_m <- do.call(cbind, m_vals)
vals_nt <- do.call(cbind, nt_vals)
vals <- rbind(vals_t, vals_m, vals_nt)
stopifnot(!anyNA(vals), all(vals >= 0))

feat_nt <- matrix("", n_nontarget, n_feat_cols, dimnames = list(NULL, colnames(feat)))
feat_nt[, "Alignment ID"] <- nt_ids
feat_nt[, "Average Rt(min)"] <- sprintf("%.3f", stats::runif(n_nontarget, 0.8, 15))
feat_nt[, "Average Mz"] <- sprintf("%.5f", stats::runif(n_nontarget, 100, 900))
feat_nt[, "Metabolite name"] <- "Unknown"
feat_nt[, "Adduct type"] <- "[M+H]+"
feat_nt[, "S/N average"] <- sprintf("%.2f", exp(stats::rnorm(n_nontarget, log(30), 1)))
feat_m <- matrix("", n_m, n_feat_cols, dimnames = list(NULL, colnames(feat)))
feat_m[, "Alignment ID"] <- met_ids
feat_m[, "Average Rt(min)"] <- sprintf("%.3f", mets$rt)
feat_m[, "Average Mz"] <- sprintf("%.5f", mz_mh)
feat_m[, "Metabolite name"] <- mets$name
feat_m[, "Adduct type"] <- "[M+H]+"
feat_m[, "Comment"] <- "Quant"
feat_m[, "S/N average"] <- sprintf("%.2f", exp(stats::rnorm(n_m, log(200), 1)))
feat_all <- rbind(feat, feat_m, feat_nt)

ctrl_cls <- do.call(c, lapply(names(seq_all), function(b) {
  ord <- seq_all[[b]]
  ifelse(ord$type == "Sample", ord$class, ctrl[[b]]$info$class[ord$src_col])
}))
# QC groups (replicates of one QC sample) are identified by their class
ctrl_cls[ord_all$type == "QC"] <- ord_all$qc_group[ord_all$type == "QC"]
blank_feat <- rep("", n_feat_cols - 1L)
out <- c(
  paste(c(blank_feat, "Class", ctrl_cls), collapse = "\t"),
  paste(c(blank_feat, "File type", ord_all$type), collapse = "\t"),
  paste(c(blank_feat, "Injection order", ord_all$io), collapse = "\t"),
  paste(c(blank_feat, "Batch ID", ord_all$batch), collapse = "\t"),
  paste(c(colnames(feat), ord_all$name), collapse = "\t"),
  paste(apply(feat_all, 1, paste, collapse = "\t"),
        apply(matrix(sprintf("%.0f", vals), nrow(vals)), 1, paste, collapse = "\t"), sep = "\t")
)
txt <- file.path(tempdir(), paste0(out_name, ".txt"))
writeLines(out, txt)
zip_file <- file.path(normalizePath(out_dir), paste0(out_name, ".zip"))   # zip() changes the wd
unlink(zip_file)
zip::zip(zip_file, basename(txt), root = dirname(txt))

cat(sprintf("Features: %d targeted/IS + %d serum metabolites + %d non-targeted\n", n_t, n_m, n_nontarget))
cat("Serum metabolites with a group difference:", paste(mets$name[m_de], collapse = ", "), "\n")
cat("Serum metabolites not detectable:", paste(mets$name[m_nd], collapse = ", "), "\n")
cat("Second failed IS:", feat[is_fail2, "Metabolite name"], "\n")
print(table(type = ord_all$type, batch = ord_all$batch))
cat(sprintf("Text: %.1f MB, zip: %.1f MB -> %s\n", file.size(txt) / 1e6, file.size(zip_file) / 1e6, zip_file))
cat("Targeted features with a group difference:", paste(feat[de_targets, "Metabolite name"], collapse = ", "), "\n")
cat("Targeted features with a strong batch 3 effect:", paste(feat[strong_bt_targets, "Metabolite name"], collapse = ", "), "\n")
