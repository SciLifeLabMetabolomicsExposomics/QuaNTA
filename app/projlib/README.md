Here are reusable code modules,
which are imported by other scripts or functions within the current project.
These modules could contain utility functions, custom classes, or any other pieces of code that are used across multiple parts of the project.

### `check_io_exist.R`

Provides functions to **validate file** and directory existence before processing:

- `chq_all_files_exist()`: Verifies that all required input files exist
- `mkdir_if_not_exist()`: Creates output directories with proper permissions
- `check_io_exist()`: Main validation function that checks input files and creates output directories

### `msdial_utils.R`

Helper functions to access and **manipulate MS-DIAL data elements**:

- **Feature accessors**: `std_type()`, `is_targeted_feature()`, `is_internal_std()`, `retention_time()`
- **Sample accessors**: `ctrl_smpl_cat()`, `is_calcurve_sample()`, `spiked_conc_pts()`
- **Sample filtering**: `exclude_ctrl_smpl_cat()`, `extract_ctrl_smpl_cat()`, `split_into_calcurve_and_other()`
- **Matrix ID management**: `mat_id_of_blank_subtracted()`, `mat_id_in_calibration()`, `src_mat_id_for_conc()`
- **Matrix manipulation**: `extract_with_na()`
- **Statistical utilities**: `.rsd_perc()`, `avg_plus_std_times()`
- **Feature accessors**: `std_type()`, `is_targeted_feature()`, `is_internal_std()`, `retention_time()`
- **Sample accessors**: `ctrl_smpl_cat()`, `is_calcurve_sample()`, `spiked_conc_pts()`
- **Sample filtering**: `exclude_ctrl_smpl_cat()`, `extract_ctrl_smpl_cat()`, `split_into_calcurve_and_other()`
- **Matrix ID management**: `mat_id_of_blank_subtracted()`, `mat_id_in_calibration()`, `src_mat_id_for_conc()`
- **Matrix manipulation**: `extract_with_na()`
- **Statistical utilities**: `.rsd_perc()`, `avg_plus_std_times()`

### `msdial.R`

Handles **reading and writing** MS-DIAL raw and processed data:

- `get_raw_data_file_name()`: Generates standardized file names for intermediate data
- `get_three_section_indices()`: Identifies the three column sections in MS-DIAL output files
- `fetch_sample_info()`: Extracts sample metadata from MS-DIAL files
- `export_data_with_feature_table_xlsx()`: Exports processed data with feature tables to Excel
- `tbl_chemical_summary()`: Creates summary tables with LOD, LLOQ, detection rates, and calibration curve statistics
- `merge_sumexp()`: Merges multiple SumExp objects with identical features

### `proc.R`

**Core data processing** and normalization functions:

- **Zero imputation**: `impute_zeros_with_mean_of_same_type()` and `count_zeros_per_feature()`
- **Quality control**: `identify_outliers()` and `count_outliers_per_sample()`
- **Internal standard normalization**: `get_data_of_closest_istd()` finds closest internal standards by retention time
- **LOESS normalization**: `get_loess_fit()` performs retention time-based normalization
- **Blank subtraction**: `add_blank_subtracted_sumexp()` subtracts batch-specific blank sample averages
- **Calibration limits**: `find_calib_lim_pts_and_llox_from_llox_signal()` determines working ranges, LOD, and LLOQ
- **Model fitting**: `fit_and_test_calcurve_model()` fits linear/quadratic models with various weighting methods
- **Concentration calculation**: `compute_concentration()` transforms signals to concentrations using fitted models
- **Post-calibration**: `replace_below_lod_lloq()` and `replace_conc_whose_signal_below_lloq()` handle values outside quantification limits

### `show.R`

Functions for generating **plots and tables** for reports:

- **Quality metrics**: `compute_rsd_per_feature()` and `rsd_perc()`
- **Color schemes**: `get_colors_of_classes()` manages consistent color palettes for control categories and classes
- **QC visualization**: `extract_quant_qc()` extracts quantitative standards from QC samples for performance monitoring
- **Calibration curve plots**: `ggplot_calcurve_samples()` and `ggplot_calcurve_samples_facet()` create visualizations with sample points and fitted lines
- **RSD plots**: `ggplot_rsdp_metab()` visualizes RSD% distributions across different processing stages
- **Custom aesthetics**: `geom_calibration_curve_line()` and custom coordinate system `CoordZoomInByCC` for specialized calibration curve rendering
