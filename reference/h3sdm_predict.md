# Predict species presence probability using H3 hexagons

Uses a fitted tidymodels workflow (from `h3sdm_fit_model` or a
standalone workflow) to predict species presence probabilities on a new
spatial H3 grid. Automatically generates centroid coordinates (`x` and
`y`) if missing. The `new_data` must contain the same predictor
variables as used in model training.

## Usage

``` r
h3sdm_predict(fit_object, new_data)
```

## Arguments

- fit_object:

  A fitted `tidymodels` workflow or the output list from
  `h3sdm_fit_model`.

- new_data:

  An `sf` object containing the spatial grid and the same predictor
  variables used for model training.

## Value

An `sf` object with the original geometry and a new column `prediction`
containing the predicted probability of presence for each hexagon.

## Examples

``` r
if (FALSE) { # \dontrun{
# Predict presence probabilities on a new hex grid
predictions_sf <- h3sdm_predict(
  fit_object = fitted_model,
  new_data   = grid_sf
)
} # }
```
