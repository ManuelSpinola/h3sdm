# CRAN Submission Comments — h3sdm 0.1.5

## Resubmission

Addressing feedback from Uwe Ligges (2026-06-11):

* Removed `tidytemplate` from `Suggests`. It is only used internally
  for building the pkgdown site and is already declared under
  `Config/Needs/website`, which is the appropriate field for
  development-only dependencies not required by package users.

* Removed duplicate entry of `DALEXtra` from `Suggests`.

* Moved `h3jsr`, `landscapemetrics`, `spocc`, `rbiodatacr`, `vip`,
  `exactextractr`, and `tidyr` from `Suggests` to `Imports`, as they
  are called directly with `::` in package functions without
  `requireNamespace()` guards.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local R installation: R 4.4.x, macOS
* win-builder (release and devel)
