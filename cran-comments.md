# CRAN Submission Comments — h3sdm 0.1.9

## Notes for CRAN

This is a quick follow-up to 0.1.8, which was accepted on 2026-11-21.
It fixes a regression introduced in 0.1.8: `h3sdm_data()` still computed
centroid coordinates in the CRS of the input, while the other functions
changed in 0.1.8 compute them as longitude and latitude. With projected
data this produced incorrect model predictions without any warning. We
are submitting ahead of the usual monthly cadence so that users do not
keep fitting models with the inconsistent version. No other changes are
included.

## Changes in 0.1.9

* `h3sdm_data()`: `x` and `y` are now computed as centroid longitude and
  latitude (EPSG:4326), consistent with `h3sdm_spatial_cv()`,
  `h3sdm_recipe()`, `h3sdm_recipe_gam()` and `h3sdm_predict()`.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local R installation: R 4.4.x, macOS
* win-builder (release and devel)
