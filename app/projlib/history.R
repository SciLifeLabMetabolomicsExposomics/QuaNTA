# ------------------------------------------------------------------------------------------- #
# What happened to each feature and sample during the processing: tables made from the event log
# of `process.R` (`to_report[["events"]]`), with the steps they passed. Used by the export
# (`export_data.R`), the internal report and the app.
# ------------------------------------------------------------------------------------------- #
box::use(util = ./msdial_utils)

#' The names of the data versions (matrix IDs), as in the app
#' @export
version_labels <- c(raw = "Raw", closest_norm = "Closest RT", loess_norm = "LOESS",
                    mvis_norm = "Multivariate (PCA)")

#' The event log as one table, with the names of the features and samples
#'
#' @param events A list of tibbles (`log_event()` of `process.R`)
#' @param se The data before normalization (all features and samples)
#' @returns A tibble of `step`, `scope`, `id`, `name`, `mat_id`, `batch_id`, `kind`, `event`,
#'   `detail`
#' @export
event_table <- function(events, se) {
  ev <- dplyr::bind_rows(events)
  if (nrow(ev) == 0) {
    ev <- tibble::tibble(step = character(), scope = character(), id = character(), mat_id = character(),
                         batch_id = character(), kind = character(), event = character(), detail = character())
  }
  feat <- stats::setNames(SumExp::row_df(se)$feature_name, rownames(se))
  smp <- stats::setNames(as.character(SumExp::col_df(se)$sample_name), colnames(se))
  name <- dplyr::case_when(
    ev$scope == "feature" ~ unname(feat[ev$id]),
    ev$scope == "sample" ~ unname(smp[ev$id]),
    ev$scope == "batch" ~ paste("Batch", ev$id),
    TRUE ~ ev$id
  )
  tibble::add_column(ev, name = dplyr::coalesce(name, ev$id), .after = "id")
}

#' The type of each feature, from its `Comment`
feature_type <- function(se) {
  dplyr::case_when(
    util$std_type(se) %in% "vIS" ~ "Volumetric IS",
    util$is_internal_std(se) ~ "Internal standard",
    util$is_targeted_feature(se) ~ "Targeted",
    TRUE ~ "Non-targeted"
  )
}

#' The events of one step for each feature ("" for none), e.g. "Removed: QC RSD% 31.2 (maximum 30)"
events_text <- function(ev, ids, with_batch = FALSE, detail_only = FALSE) {
  if (nrow(ev) == 0) return(rep("", length(ids)))
  txt <- if (detail_only) {
    dplyr::coalesce(ev$detail, ev$event)
  } else {
    paste0(ev$event, ifelse(is.na(ev$detail), "", paste0(" (", ev$detail, ")")))
  }
  txt <- paste0(if (with_batch) ifelse(is.na(ev$batch_id), "", paste0("Batch ", ev$batch_id, ": ")) else "", txt)
  out <- tapply(txt, ev$id, paste, collapse = "; ")
  x <- unname(out[ids])
  ifelse(is.na(x), "", x)
}

#' What happened to each feature, per data version
#'
#' One row per feature of the input (also those removed on the way), a column per step: what the
#' step did with the feature ("Passed", "Removed: ...", "Not applied", ...; empty where the step
#' does not concern it, or the feature was removed before).
#'
#' @param tr The `to_report` of `process.R` (with `events`)
#' @param lst_proc The calibrated data of `process.R` (`NULL` without calibration)
#' @returns A named list (by matrix ID) of tibbles
#' @export
feature_history <- function(tr, lst_proc = NULL) {
  se0 <- tr[["before normalization"]]
  ids <- rownames(se0)
  rd0 <- SumExp::row_df(se0)
  type <- feature_type(se0)
  ev_all <- tr[["events"]]
  ev_all <- ev_all[ev_all$scope == "feature", ]
  is_std <- type %in% c("Internal standard", "Volumetric IS")
  norm_rd <- tr[["full data"]]$norm_rows     # All features
  closest <- stats::setNames(norm_rd$closest_istd, rownames(norm_rd))
  failed_ev <- ev_all[ev_all$step == "Failed internal standards", ]
  failed <- ids %in% failed_ev$id
  rsd <- tr[["qc rsd filter"]]
  params <- tr[["params"]]
  versions <- tr[["matrix ids to blk subt"]]
  out <- lapply(stats::setNames(nm = versions), function(m) {
    ev <- ev_all[is.na(ev_all$mat_id) | ev_all$mat_id == m, ]
    of_step <- \(s) ev[ev$step == s, ]
    blank_rm <- ids %in% of_step("Blank filter")$id
    rsd_rm <- ids %in% of_step("QC RSD% filter")$id
    tbl <- tibble::tibble(
      Feature = rd0$feature_name,
      `Alignment ID` = rd0$alignment_id,
      Type = type,
      `RT [min]` = rd0$.rt,
      `m/z` = rd0$mz,
      `Failed IS check` = dplyr::case_when(
        type != "Internal standard" ~ "",
        failed ~ paste0("Removed: ", events_text(failed_ev, ids, detail_only = TRUE)),
        TRUE ~ "Passed"
      ),
      `Normalized by` = dplyr::case_when(
        is_std ~ "",
        m == "raw" ~ "Not normalized",
        m == "closest_norm" ~ dplyr::coalesce(unname(closest[ids]), ""),
        m == "loess_norm" ~ "LOESS fit of the IS over RT",
        m == "mvis_norm" ~ "PCA of the IS",
        TRUE ~ ""
      ),
      `Blank filter` = dplyr::case_when(
        failed ~ "",
        is_std ~ "Not filtered (standard)",
        !isTRUE(tr[["blank_filter"]]) ~ "Not applied",
        blank_rm ~ paste0("Removed: ", events_text(of_step("Blank filter"), ids, detail_only = TRUE)),
        TRUE ~ "Passed"
      ),
      `QC RSD% filter` = dplyr::case_when(
        failed | blank_rm ~ "",
        type == "Volumetric IS" | type == "Internal standard" ~ "Not filtered (standard)",
        !isTRUE(rsd$enabled) ~ "Not applied",
        isTRUE(rsd$skipped) ~ "Not done (no QC replicates)",
        rsd_rm ~ paste0("Removed: ", events_text(of_step("QC RSD% filter"), ids, detail_only = TRUE)),
        type == "Targeted" & is.na(rsd$max_targeted) ~ "Not applied (targeted)",
        type == "Non-targeted" & is.na(rsd$max_nontargeted) ~ "Not applied (non-targeted)",
        TRUE ~ "Passed"
      )
    )
    removed <- failed | blank_rm | rsd_rm
    status <- dplyr::case_when(
      failed ~ "Removed (failed IS)",
      type == "Internal standard" ~ "Used for the normalization",
      type == "Volumetric IS" ~ "Kept (volumetric IS)",
      blank_rm ~ "Removed (blank filter)",
      rsd_rm ~ "Removed (QC RSD% filter)",
      type == "Non-targeted" ~ "Kept",
      TRUE ~ "Not calibrated"
    )
    # Calibration: a column per batch
    cal <- lst_proc[[util$mat_id_of_blank_subtracted(m)]]
    ev_cal <- of_step("Calibration")
    notes_ev <- ev_cal[ev_cal$event %in% c("Full calibration range used", "Above the linear range"), ]
    exported <- rep(FALSE, length(ids))
    for (b in names(cal)) {
      concn <- cal[[b]]$concn
      col <- rep("", length(ids))
      if (!is.null(concn)) {
        rd <- SumExp::row_df(concn)
        i <- match(rownames(concn), ids)
        ok <- !is.na(i)
        rd <- rd[ok, , drop = FALSE]
        i <- i[ok]
        issue <- dplyr::coalesce(rd$range_issue, "no valid range")
        col[i] <- dplyr::case_when(
          !(!is.na(rd$has_proper_range) & rd$has_proper_range) ~ paste0("Not quantified: ", issue),
          !is.na(rd$kept_calcurve) & rd$kept_calcurve ~ paste0(util$calibration_label(dplyr::coalesce(rd$src_calcurve, "Batch"), length(cal)),
                                                               " curve kept although not valid: ", issue),
          rd$src_calcurve %in% "Global" & !is.na(rd$range_issue) ~ paste0("Global curve (", rd$range_issue, " in the batch)"),
          rd$src_calcurve %in% "Global" ~ "Global curve",
          length(cal) == 1 ~ "Global curve",   # One batch only: its curve is the global curve
          TRUE ~ "Batch curve"
        )
        to_exp <- !is.na(rd$to_export) & rd$to_export
        col[i][!to_exp & !startsWith(col[i], "Not quantified")] <-
          paste0(col[i][!to_exp & !startsWith(col[i], "Not quantified")], "; not exported (no study sample above the LLOQ)")
        # Exported only because of "Keep all calibration curves"
        kept_exp <- if (is.null(rd$exported_kept)) rep(FALSE, nrow(rd)) else rd$exported_kept %in% TRUE
        col[i][kept_exp] <- paste0(col[i][kept_exp], "; exported although no study sample above the LLOQ (Keep all calibration curves)")
        exported[i] <- exported[i] | to_exp
      }
      tbl[[paste("Calibration, batch", b)]] <- ifelse(type == "Targeted" & !removed, col, "")
    }
    if (length(cal) > 0) {
      status[type == "Targeted" & !removed] <- ifelse(exported[type == "Targeted" & !removed],
                                                      "Quantified", "Not quantified")
    }
    tbl$Notes <- events_text(notes_ev, ids, with_batch = TRUE)
    tibble::add_column(tbl, Status = status, .after = "m/z")
  })
  out
}

#' Where the features ended: a Sankey diagram from their type (left) to their status (right)
#'
#' @param fh A feature history (one data version of `feature_history()`)
#' @param height The height in pixels
#' @export
plotly_feature_flow <- function(fh, height = 420) {
  flow <- dplyr::count(fh, Type, Status)
  types <- unique(flow$Type)
  statuses <- unique(flow$Status)
  nodes <- c(types, statuses)
  n_node <- c(tapply(flow$n, flow$Type, sum)[types], tapply(flow$n, flow$Status, sum)[statuses])
  ok <- c("Quantified", "Kept", "Used for the normalization", "Kept (volumetric IS)")
  node_col <- ifelse(startsWith(nodes, "Removed") | nodes == "Not quantified", "#D55E00",
                     ifelse(nodes %in% ok, "#009E73", "#56B4E9"))
  plotly::plot_ly(
    type = "sankey", orientation = "h", arrangement = "snap",
    node = list(label = paste0(nodes, " (", n_node, ")"), color = node_col, pad = 18, thickness = 18),
    link = list(source = match(flow$Type, nodes) - 1L, target = match(flow$Status, nodes) - 1L,
                value = flow$n, color = "rgba(150, 160, 175, 0.35)"),
    height = height
  ) |>
    plotly::layout(font = list(size = 12), margin = list(l = 10, r = 10, t = 10, b = 10)) |>
    plotly::config(displayModeBar = FALSE)
}

#' The history tables of one data version as an Excel file
#'
#' Sheets "Feature history", "Sample history", "Events" (of this version and of all) and "Legend".
#'
#' @param tr The `to_report` of `process.R` (with `events`, `feature history`, `sample history`)
#' @param mat_id The data version (matrix ID, e.g. "loess_norm")
#' @param file The `.xlsx` file
#' @export
write_history_xlsx <- function(tr, mat_id, file) {
  ev <- tr[["events"]]
  ev <- ev[is.na(ev$mat_id) | ev$mat_id == mat_id, ]
  ev <- tibble::tibble(Step = ev$step, `Applies to` = ev$scope, Name = ev$name, ID = ev$id,
                       Batch = ev$batch_id, Kind = ev$kind, Event = ev$event, Detail = ev$detail)
  sheets <- list(`Feature history` = tr[["feature history"]][[mat_id]],
                 `Sample history` = tr[["sample history"]], Events = ev)
  wb <- openxlsx::createWorkbook()
  head_style <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#E3E8F1", border = "bottom")
  for (s in names(sheets)) {
    openxlsx::addWorksheet(wb, s)
    openxlsx::writeData(wb, s, sheets[[s]], headerStyle = head_style)
    openxlsx::freezePane(wb, s, firstRow = TRUE, firstCol = s != "Events")
    openxlsx::setColWidths(wb, s, cols = seq_len(ncol(sheets[[s]])), widths = "auto")
  }
  legend <- tibble::tibble(
    Sheet = c("Feature history", "", "", "Sample history", "", "Events", "", ""),
    Explanation = c(
      paste0("One row per feature of the input file, also those removed on the way. Data version: ",
             dplyr::coalesce(unname(version_labels[mat_id]), mat_id), "."),
      paste("A column per processing step, in the order of the processing: what the step did with the",
            "feature (Passed, Removed: why, Not applied, ...). Empty: the step does not concern the feature,",
            "or it was removed before."),
      paste("Calibration, batch N: the curve used for the feature in that batch (its own or the global one),",
            "or why it was not quantified. Status: where the feature ended."),
      "One row per sample of the input file: what each step did with it, and its status.",
      "Calibration (version): the calibration of the sample's batch in each data version.",
      "Everything the processing logged, one row per feature, sample or batch it concerned.",
      paste("Kind: removed (left out from there on), fallback (the data did not allow the usual way; what",
            "was done instead), changed (a value replaced), note (for information)."),
      "Applies to: feature, sample, batch, or run (the whole processing)."
    )
  )
  openxlsx::addWorksheet(wb, "Legend")
  openxlsx::writeData(wb, "Legend", legend, headerStyle = head_style)
  openxlsx::setColWidths(wb, "Legend", cols = 1:2, widths = c(18, 120))
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  invisible(file)
}

#' What happened to each sample (the same for all data versions, but the calibration)
#'
#' @param tr The `to_report` of `process.R` (with `events`)
#' @returns A tibble, one row per sample of the input
#' @export
sample_history <- function(tr) {
  se0 <- tr[["before normalization"]]
  ids <- colnames(se0)
  cd <- SumExp::col_df(se0)
  ev <- tr[["events"]]
  ev <- ev[ev$scope == "sample", ]
  of_step <- \(s) ev[ev$step == s, ]
  params <- tr[["params"]]
  out_ev <- of_step("Outlier samples")
  is_out <- ids %in% out_ev$id[out_ev$event == "Removed"]
  cat <- util$ctrl_smpl_cat(se0)
  batch <- as.character(cd$batch_id)
  no_blank <- !any(cat == "Blank")
  imp <- tr[["imputed IS zeros"]]
  n_imp <- if (is.null(imp)) rep(0L, length(ids)) else as.integer(table(factor(imp$sample_id, levels = ids)))
  tbl <- tibble::tibble(
    Sample = as.character(cd$sample_name),
    `Sample type` = cd$sample_type,
    Class = cd$Class,
    Batch = batch,
    `Injection order` = cd$injection_order,
    `vIS pre-normalization` = dplyr::case_when(
      !isTRUE(params$vis_prenorm) || is.null(tr[["vIS prenormalization"]]) ~ "Not applied",
      ids %in% of_step("vIS pre-normalization")$id ~
        paste0("Not corrected: ", events_text(of_step("vIS pre-normalization"), ids, detail_only = TRUE)),
      TRUE ~ "Corrected"
    ),
    `Outlier check` = dplyr::case_when(
      !isTRUE(params$rm_outlier) ~ "Not applied",
      ids %in% out_ev$id ~ paste0(events_text(out_ev, ids)),
      TRUE ~ "Passed"
    ),
    `IS zeros replaced` = ifelse(is_out, "", ifelse(
      n_imp == 0, "", paste0(n_imp, " (", events_text(of_step("Zeros of the internal standards"), ids, detail_only = TRUE), ")")
    )),
    `Blank subtraction` = dplyr::case_when(
      is_out ~ "",
      no_blank ~ "Not done (no blank samples)",
      cat == "Blank" ~ "Not subtracted (blank sample)",
      cat == "CalCurve" ~ "Not subtracted (calibration sample)",
      batch %in% tr[["batches without blank"]] ~ "Blank mean of all batches (no blank in the batch)",
      length(unique(batch)) == 1 ~ "Blank mean of all blanks",
      TRUE ~ "Blank mean of its batch"
    )
  )
  # The calibration of its batch, per data version
  cpb <- tr[["calibration per batch"]]
  if (!is.null(cpb) && nrow(cpb) > 0) {
    for (mb in unique(cpb$mat_id)) {
      m <- sub("_blk$", "", mb)
      x <- cpb[cpb$mat_id == mb, ]
      # One batch only: its curve is the global curve
      lab <- util$calibration_label(x$calibration, nrow(x))
      txt <- ifelse(is.na(x$calibration), "Not calibrated",
                    ifelse(is.na(x$note), lab, paste0(lab, " (", x$note, ")")))
      val <- unname(stats::setNames(txt, x$batch_id)[batch])
      tbl[[paste0("Calibration (", dplyr::coalesce(version_labels[m], m), ")")]] <- ifelse(is_out, "", val)
    }
  }
  tbl$Status <- ifelse(is_out, "Removed (outlier)", "Kept")
  dplyr::relocate(tbl, "Status", .after = "Injection order")
}
