# Area of Applicability (AOA) of spatial prediction models

Estimates the Dissimilarity Index (DI) and the Area of Applicability
(AOA) for new data given the training data and a fitted model. This
function is designed to be applied directly to the output of
[`h3sdm_predict()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_predict.md),
so that both the predicted values and the AOA are available in a single
`sf` object ready for mapping.

## Usage

``` r
h3sdm_aoa(newdata, train, fit_object, cv = NULL, verbose = TRUE)
```

## Arguments

- newdata:

  An `sf` object, typically the direct output of
  [`h3sdm_predict()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_predict.md),
  containing the predictor variables and a `prediction` column.

- train:

  An `sf` object, typically the output of
  [`h3sdm_data()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_data.md),
  containing the training observations with the same predictor variables
  used in `fit_object`.

- fit_object:

  The list returned by
  [`h3sdm_fit_model()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_fit_model.md),
  used to extract the recipe (predictor variable names) and variable
  importances when available.

- cv:

  An `rset` object returned by
  [`h3sdm_spatial_cv()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_spatial_cv.md),
  used to extract cross-validation fold assignments. If `NULL`
  (default), Leave-One-Out (LOO) cross-validation is used.

- verbose:

  Logical. Should progress messages be printed? Default `TRUE`.

## Value

The input `newdata` `sf` object with two additional columns:

- `DI`:

  Numeric. Dissimilarity Index for each observation. Values near 0
  indicate high similarity to the training data; larger values indicate
  increasing dissimilarity.

- `AOA`:

  Integer. `1` = inside the AOA; `0` = outside the AOA.

## Details

The algorithm follows Meyer & Pebesma (2021). Predictor variables are
extracted automatically from the recipe inside `fit_object`. They are
standardized using z-score scaling computed from `train`, then
optionally weighted by variable importance. The mean nearest-neighbor
distance among training points (`trainDist_avrgmean`) is used to
normalize DI values. The AOA threshold is the maximum cross-validated
training DI after removing outliers with Tukey's rule (Q3 + 1.5 \* IQR).
Locations with `DI <= threshold` are inside the AOA; locations above the
threshold should be interpreted with caution.

Variable importance is extracted automatically for `ranger` and
`xgboost` models via
[`vip::vi()`](https://koalaverse.github.io/vip/reference/vi.html). For
GAM models, or when importance cannot be extracted, all variables
receive equal weight.

## References

Meyer, H., Pebesma, E. (2021): Predicting into unknown space? Estimating
the area of applicability of spatial prediction models. *Methods in
Ecology and Evolution* 12: 1620–1633.
[doi:10.1111/2041-210X.13650](https://doi.org/10.1111/2041-210X.13650)

## See also

[`h3sdm_predict()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_predict.md),
[`h3sdm_fit_model()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_fit_model.md),
[`h3sdm_spatial_cv()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_spatial_cv.md),
[`h3sdm_data()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_data.md)

## Examples

``` r
if (FALSE) { # \dontrun{
cv      <- h3sdm_spatial_cv(dat, method = "block")
fit     <- h3sdm_fit_model(workflow, cv)
pred    <- h3sdm_predict(fit, new_data = h7)
result  <- h3sdm_aoa(pred, train = dat, fit_object = fit, cv = cv)
} # }
```
