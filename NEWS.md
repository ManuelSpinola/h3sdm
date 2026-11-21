# h3sdm 0.1.9

## Bug fixes

* `h3sdm_data()` now computes the `x` and `y` centroid coordinates as
  longitude and latitude (EPSG:4326), regardless of the CRS of the input,
  like the other functions changed in 0.1.8. In 0.1.8, when `h3sdm_data()`
  was used with projected data, the model was fitted with `x` and `y` in
  projected units while `h3sdm_predict()` computed them in degrees, which
  produced incorrect predictions for spatial terms such as `s(x, y)`.
  Models fitted with h3sdm 0.1.8 on projected data should be refitted.

* `h3sdm_aoa()` now always excludes the spatial coordinates `x` and `y` from
  the Dissimilarity Index. Previously, with recipes from `h3sdm_recipe_gam()`
  (where `x` and `y` are predictors for the `s(x, y)` term), the coordinates
  were included, so geographic distance to the training data counted as
  environmental dissimilarity. The AOA now measures similarity in
  environmental space only, for all model types. AOA results for GAM models
  from earlier versions should be recomputed.

# h3sdm 0.1.8

## Bug fixes

* `h3sdm_fit_model()` now fits `final_model` on all observations in
  `data_split`. Previously it was fitted only on the analysis (training) set
  of the first fold, so the final model ignored part of the data and depended
  on the fold assignment. Final models fitted with earlier versions should be
  refitted.

* `h3sdm_fit_model()` now treats presence (`"1"`) as the event class when
  computing classification metrics. Previously, because `presence` has levels
  `c("0", "1")`, the absence class was used as the event: the reported `sens`
  and `spec` were swapped and `f_meas` was computed for absences. `accuracy`,
  `kap`, `roc_auc`, and the TSS and Boyce values from `h3sdm_eval_metrics()`
  were not affected. Sensitivity, specificity and F1-score reported with
  earlier versions should be recomputed.

* `h3sdm_spatial_cv()` now stops with an informative error when
  `method = "block"`, `cellsize` is supplied and `data` uses geographic
  coordinates (degrees). Previously `cellsize` was silently interpreted in
  degrees, producing blocks of the wrong size. Project the data to a metric
  CRS with `sf::st_transform()` before creating the folds.

* `h3sdm_spatial_cv()` now adds the `x` and `y` centroid coordinates when they
  are missing from `data`. Previously the data used for cross-validation and
  for the final fit lacked these columns, causing an error with recipes
  created by `h3sdm_recipe()` and `h3sdm_recipe_gam()`.

* `x` and `y` are now always computed as the hexagon centroid longitude and
  latitude (EPSG:4326) in `h3sdm_spatial_cv()`, `h3sdm_recipe()`,
  `h3sdm_recipe_gam()` and `h3sdm_predict()`, regardless of the CRS of the
  input data. This guarantees that model fitting and prediction use the same
  coordinate units. Previously, a model fitted on projected data and used to
  predict on a grid in degrees (or vice versa) produced incorrect predictions
  for spatial terms such as `s(x, y)` without any warning. Models fitted with
  `x` and `y` in projected units should be refitted.

* `h3sdm_pa()` now returns all candidate hexagons, without clustering or
  sampling, when `n_pseudoabs` is equal to or larger than the number of
  hexagons available for pseudo-absences.

## Improvements

* `h3sdm_pa()` gains a `method` argument: `"environmental"` (default, k-means
  stratification in environmental space, as before) or `"random"` (random
  sampling of candidate hexagons).

# h3sdm 0.1.7

## Improvements

* `h3sdm_aoa()` no longer depends on `vip`. Variable importance for
  `ranger` and `xgboost` models is now extracted directly via
  `ranger::importance()` and `xgboost::xgb.importance()`, removing an
  external dependency after `vip` was archived from CRAN on 2026-07-13.
  Behavior is unchanged: importance weighting still uses each engine's
  native (non-permutation) importance measure.

* `h3sdm_workflow()` and `h3sdm_workflows()` now warn when a `ranger`
  model spec is created without an importance mode (e.g.
  `set_engine("ranger", importance = "impurity")`). Without it,
  `h3sdm_aoa()` silently falls back to equal variable weights instead of
  weighting by native importance.

# h3sdm 0.1.6

## New functions

* `h3sdm_filter_range()` filters spatial predictions outside the univariate 
  range of training data. Adds a `range_filter` column (1 = within range, 
  0 = outside range) to the prediction object. Complements `h3sdm_aoa()` and 
  `h3sdm_filter_outliers()`: while AOA detects combinations without analogues 
  in the training data and Mahalanobis distance identifies multivariate outliers, 
  this function detects extrapolation at the margins of individual variables 
  that multivariate methods may not capture.

* `h3sdm_pres_from_sf()` assigns pre-downloaded species occurrence records
  (an `sf` object, typically from `h3sdm_get_records()`) to H3 hexagons,
  returning only hexagons with at least one presence record. This enables
  a clean two-stage workflow where records are downloaded once and reused:
  `h3sdm_get_records()` → `h3sdm_pres_from_sf()` → `h3sdm_filter_outliers()`
  → `h3sdm_pa()` with balanced pseudo-absences (`n_pseudoabs = nrow(pres_clean)`).

# h3sdm 0.1.5

## New datasets

* `cr_outline` -- Costa Rica full outline (continental landmass + Isla del
  Coco and all minor oceanic islands), derived from GADM 4.1.

## New functions

* `h3sdm_filter_outliers()` removes environmental outliers from presence
  records prior to model training using Mahalanobis distance (D2) in
  environmental space. Only presences (`presence == "1"`) are evaluated;
  pseudo-absences are always retained unchanged. The outlier threshold is
  derived from the chi-squared distribution (`qchisq(threshold, df = k)`,
  default `threshold = 0.975`). Returns a list with the cleaned PA dataset,
  a data frame of removed records with their D2 values, the count of removed
  records, and the threshold value used. Complements `h3sdm_aoa()`: while the
  AOA evaluates prediction reliability after training, this function improves
  input data quality before training.

* `h3sdm_pres()` assigns species occurrence records to H3 hexagons and returns
  only hexagons with at least one presence record. This is the first step of a
  two-stage workflow where pseudo-absences are generated after environmental
  variables have been extracted.

* `h3sdm_pa()` has been redesigned to generate pseudo-absences stratified in
  environmental space using k-means clustering. Pseudo-absences now cover the
  full range of environmental conditions available in the AOI, reducing
  environmental bias introduced by spatially clustered occurrence records.
  The function now receives presence hexagons from `h3sdm_pres()` and the full
  hexagonal grid with extracted variables from `h3sdm_predictors()`.

## Improvements

* `cr_outline_c` dataset regenerated from GADM 4.1 with a fully reproducible
  script in `data-raw/cr_outline.R`. Source attribution updated to GADM 4.1.
  Geometry is now consistent with the `cr_outline_c` dataset in `paisaje`.

* `h3sdm_pa()` and `h3sdm_pa_from_records()` now accept a `buffer_k` argument
  (default `1`). Hexagons within `buffer_k` H3 rings of any presence hexagon
  are excluded from the pseudo-absence candidate pool, preventing pseudo-absences
  from being placed in areas likely occupied but not yet recorded. Set to `0`
  to disable.

* `h3sdm_pa_from_records()` now accepts an optional `predictors_sf` argument.
  When provided, pseudo-absences are selected by stratified sampling in
  environmental space using k-means clustering. If `NULL` (default), the
  previous random geographic sampling behaviour is preserved.

## Bug fixes

* `h3sdm_aoa()` now extracts predictor variable names from the model recipe
  instead of the model formula, fixing an error with GLM and other engines
  where parsnip stores a generic formula internally.

* `h3sdm_aoa()` now uses `na.rm = TRUE` when computing the inside/outside AOA
  summary, avoiding `NA` in the progress message when hexagons have missing
  values.

# h3sdm 0.1.4

## Bug fixes
* `h3sdm_pa()` now transforms presence records to the CRS of the H3 grid
  before joining, fixing an error when the grid is in a projected CRS.

# h3sdm 0.1.3

## New functions
* `h3sdm_aoa()` estimates the Dissimilarity Index (DI) and the Area of
  Applicability (AOA) for spatial prediction models, based on Meyer &
  Pebesma (2021).

## Improvements
* `h3sdm_get_grid()` now preserves the CRS of the input `sf_object`.
  Previously, the function always returned the grid in WGS84 (EPSG:4326)
  regardless of the input CRS. Now, if the AOI is in a projected CRS,
  the output grid will be reprojected to match it. The internal H3
  computation still uses WGS84 as required by the H3 system.
* `h3sdm_predict()` internal comments translated to English and
  `@seealso` updated to include `h3sdm_aoa()`.

# h3sdm 0.1.2

## Improvements
* `h3sdm_fit_model()` now automatically detects model mode (classification or regression),
  enabling count-based models (Poisson, Negative Binomial) with appropriate metrics
  (RMSE, R2, MAE) without requiring manual configuration.

* `h3sdm_fit_model()` and `h3sdm_predict()` now automatically detect model mode
  (classification or regression), enabling count-based models (Poisson, Negative
  Binomial) without manual configuration. Full backward compatibility maintained.

* `h3sdm_get_records()` now supports `"biodatacr"` as an optional provider,
  querying occurrence records from BiodataCR (Costa Rica) via the `rbiodatacr`
  package. `h3sdm_pa()` inherits this support automatically through its
  `providers` argument. `rbiodatacr` is listed as a suggested dependency.

# h3sdm 0.1.1

## New functions

* `h3sdm_pa_from_records()`: generates a presence/pseudo-absence dataset from
  user-provided records. Accepts a `data.frame` or `sf` object with coordinates
  in WGS84 (EPSG:4326). Supports optional filtering by a `geospatialKosher`
  column to remove records with questionable spatial quality.

* `h3sdm_count_from_records()`: generates a hexagonal grid with count-based
  response variables (species richness, total detections, or individual abundance)
  from user-provided records. Accepts a `data.frame` or `sf` object. Supports
  optional filtering by presence column, confidence threshold, and date range.

## Changes to existing functions

* `h3sdm_recipe()`: added `response_col` parameter (default `"presence"`) to
  support count-based response variables. Use `response_col = "count"` when
  working with data generated by `h3sdm_count_from_records()`.

* `h3sdm_recipe_gam()`: added `response_col` parameter (default `"presence"`)
  with the same behavior as `h3sdm_recipe()`. Also added documentation examples
  for both presence/absence and count-based models.

* `h3sdm_workflow_gam()`: updated documentation to clarify the use of
  `set_mode("classification")` for presence/absence models and
  `set_mode("regression")` with `family = poisson()` for count-based models.

* `h3sdm_workflow()`: updated documentation to clarify model mode selection
  for presence/absence and count-based models.

* `h3sdm_workflows()`: updated documentation to clarify model mode selection
  for presence/absence and count-based models.

# h3sdm 0.1.0

* Initial CRAN submission.
