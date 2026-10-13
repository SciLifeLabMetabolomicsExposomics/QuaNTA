# QuaNTA example dataset

A synthetic dataset to try out QuaNTA. It's an MS-DIAL alignment export (ESI+), zipped:

| File | Content |
| --- | --- |
| `QuaNTA_example_ESIpos.zip` | The MS-DIAL file (`QuaNTA_example_ESIpos.txt`, tab-delimited) |
| `QuaNTA_example_parameters.yml` | Project information and processing settings for the app (optional) |
| `make_example_data.R` | The script that created the dataset (for reference; its source file isn't included) |

## How to use it

1. Download `QuaNTA_example_ESIpos.zip` and upload it in the app. You don't need to unzip it first.
2. Optionally, load `QuaNTA_example_parameters.yml` with **Load parameters file** to fill in the project information and processing settings.
3. Click **Read**, then **Process**. Explore the tabs, choose the final normalization method, then export the tables and generate the reports.

## Content

- **3 batches.** Each batch has:
  - 3 injections of a 14-level calibration curve (0–50 ng/mL): at the start, in the middle and at the end
  - 7–8 procedural blanks
  - 10 QCs: 5 QC groups (`QCall`, `QC1`–`QC4`) with 2 replicates each
  - 40 samples
- **120 samples:** 60 `Healthy` and 60 `Sick`, with 20 + 20 in each batch.
- **222 targeted features:** 196 compounds (`Q`), 22 internal standards (`IS`), 1 volumetric IS (`vIS`) and 3 features marked for removal (`R`). The compounds are 151 environmental chemicals and drugs and 45 common serum metabolites (amino acids, acylcarnitines, purines, bile acids, lysophosphatidylcholines and others).
- **1000 non-targeted features** (`Unknown`).

The calibration standards, QCs and blanks of the 151 environmental chemicals and drugs come from a real measurement. Everything else is synthetic: all samples, the serum metabolites, all non-targeted features, the third calibration curve injection of each batch, and a copy of the calibration curve, QCs and blanks in batch 3. No real sample data is included.

Features built in to try the app on:

- **Group differences** between `Sick` and `Healthy`, up or down:
  - 10 environmental chemicals and 50 non-targeted features by a factor of 1.5–3
  - 10 serum metabolites and 47 more non-targeted features by a factor of 2–4
- **Signal drift** over the injection order and **matrix effects** per sample. Both are reflected by the internal standards, so normalization corrects them.
- **Batch effects.** Mild for all features. Strong for 5% of the non-targeted features and for 8 compounds in batch 3; for those compounds, batch-wise calibration corrects it.
- **Outliers:**
  - single outlying calibration points (batch 3)
  - extreme QC values
  - carry-over in one blank
  - outlying sample values
  - 4 failed injections (signal far too low or too high), which are removed as outlier samples
- **Failed internal standards:** 2 (`Thyroxine-13C6_IS`, `Progesterone-d9_IS`), with zero values in more than 10% of the injections.
- **Not detectable:** 2 serum metabolites (Riboflavin and Serotonin) are below the detection limit in all samples.
- **Blank contaminants:** about 6% of the non-targeted features, removed by blank filtering.
- **Noisy features in the QCs:** about 8% of the non-targeted features, removed by the QC RSD% filter.
- **Missing values:** zeros for low signals.

Note: the real QCs weren't spiked with the environmental chemicals, so many of them are near or below the detection limit in the QCs. Their QC RSD% is therefore high. That's why the example parameters apply the QC RSD% filter to non-targeted features only.
