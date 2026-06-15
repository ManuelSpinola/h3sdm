# CRAN Submission Comments — h3sdm 0.1.6

## Resubmission

Bug-fix release addressing a critical error introduced in 0.1.5:

* Fixed hardcoded geometry column name `"geometry"` in `h3sdm_pa()` and
  `h3sdm_pres_from_sf()`. The geometry column is now standardized at the
  start of each function using `sf::st_geometry(x) <- "geometry"`, ensuring
  compatibility with sf objects from `.gpkg` files where the geometry column
  is named `"geom"` by default.

## New functions in 0.1.6

* `h3sdm_filter_range()`: filters spatial predictions outside the univariate
  range of training data, complementing `h3sdm_aoa()` and
  `h3sdm_filter_outliers()`.

* `h3sdm_pres_from_sf()`: assigns pre-downloaded species occurrence records
  (an sf object) to H3 hexagons, enabling workflows where records are
  downloaded once and reused across multiple modeling steps.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local R installation: R 4.4.x, macOS
* win-builder (release and devel)
