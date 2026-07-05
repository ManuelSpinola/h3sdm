# CRAN Submission Comments — h3sdm 0.1.7

## Notes for CRAN

This release removes a dependency on the 'vip' package, which is
scheduled for CRAN archival on 2026-07-13. 'vip' has been replaced with
equivalent code using `ranger::importance()` and
`xgboost::xgb.importance()` (both already listed in Suggests), with no
change in behavior. This submission is made ahead of our usual monthly
cadence specifically to address this upcoming archival before it takes
effect.

## Changes in 0.1.7

* `h3sdm_aoa()` no longer depends on `vip`. Variable importance for
  `ranger` and `xgboost` models is now extracted directly via
  `ranger::importance()` and `xgboost::xgb.importance()`.
* `h3sdm_workflow()` and `h3sdm_workflows()` now warn when a `ranger`
  model spec is created without an importance mode (e.g.
  `set_engine("ranger", importance = "impurity")`), since this is
  required for `h3sdm_aoa()` to weight variables by native importance.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local R installation: R 4.4.x, macOS
* win-builder (release and devel)
