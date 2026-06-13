# Filter predictions outside the univariate range of training data

Adds a `range_filter` column to `newdata` indicating whether each
observation falls within the univariate range of the training data for
all specified variables. This function is complementary to
[`h3sdm_aoa()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_aoa.md)
and Mahalanobis distance filtering, as it detects extrapolation at the
margins of individual variables that multivariate methods may not
capture.

## Usage

``` r
h3sdm_filter_range(newdata, train, variables)
```

## Arguments

- newdata:

  An `sf` object with predictions (output of
  [`h3sdm_predict()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_predict.md)
  or
  [`h3sdm_aoa()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_aoa.md)).

- train:

  An `sf` object with the training data used to fit the model.

- variables:

  A character vector of variable names to check. Should match the
  covariates used in the model.

## Value

The `newdata` object with an additional integer column `range_filter`:

- 1:

  Observation is within the training range for all variables.

- 0:

  Observation is outside the training range for at least one variable.

## Details

This function implements univariate range filtering as a quality control
step for spatial predictions. It complements multivariate methods such
as the Area of Applicability (AOA) and Mahalanobis distance, which may
not detect extrapolation when a prediction point lies at the margin of a
single variable but within the multivariate space of the training data.

The three methods detect different types of extrapolation:

- **Range filter**: extrapolation in individual variables.

- **Mahalanobis distance**: points far from the multivariate centroid.

- **AOA**: combinations without analogues in training data.

## Examples

``` r
if (FALSE) { # \dontrun{
aoa_result <- h3sdm_aoa(
  newdata    = prediccion_especies,
  train      = dat_modelo,
  fit_object = mod_gam,
  cv         = scv
)

aoa_result <- h3sdm_filter_range(
  newdata   = aoa_result,
  train     = dat_modelo,
  variables = c("bio12", "bio17", "bio8", "class_prop_1", "class_prop_11")
)

# Inspect filtered hexagons
table(aoa_result$range_filter)

# Mask predictions outside range
aoa_result <- aoa_result |>
  dplyr::mutate(prediction = ifelse(range_filter == 0, NA, prediction))
} # }
```
