# CRAN Submission Comments — h3sdm 0.1.8

## Notes for CRAN

This is a bug-fix release. It corrects two errors in `h3sdm_fit_model()`
(the final model was fitted on a subset of the data, and sensitivity and
specificity were reported for the absence class instead of the presence
class), adds an input check to `h3sdm_spatial_cv()` for geographic
coordinates, and makes the centroid coordinates used by the model
consistent between fitting and prediction. It also adds a `method`
argument to `h3sdm_pa()`.

## Changes in 0.1.8

* `h3sdm_fit_model()`: `final_model` is now fitted on all observations, and
  presence (`"1"`) is treated as the event class for classification metrics.
* `h3sdm_spatial_cv()`: informative error when `cellsize` is supplied with
  data in geographic coordinates; adds missing `x` and `y` columns.
* `h3sdm_spatial_cv()`, `h3sdm_recipe()`, `h3sdm_recipe_gam()` and
  `h3sdm_predict()`: `x` and `y` are always computed as centroid longitude
  and latitude (EPSG:4326).
* `h3sdm_pa()`: new `method` argument (`"environmental"` or `"random"`), and
  all candidates are returned when `n_pseudoabs` exceeds the number available.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local R installation: R 4.4.x, macOS
* win-builder (release and devel)
