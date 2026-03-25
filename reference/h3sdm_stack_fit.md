# Creates and fully fits an ensemble model (Stack).

This function combines the process of creating the model stack,
optimizing the weights (`blend_predictions`), and fitting the base
models to the complete training set (`fit_members()`) into a single
step.

**Warning:** It does not follow the canonical tidymodels flow but is
convenient. It requires that the fitting results were generated using
`h3sdm_fit_model(..., for_stacking = TRUE)`.

## Usage

``` r
h3sdm_stack_fit(..., non_negative = TRUE, metric = NULL)
```

## Arguments

- ...:

  List objects that are the result of
  [`h3sdm_fit_model()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_fit_model.md).
  Each object must contain the `cv_model` element (result of
  fit_resamples).

- non_negative:

  Logical. If `TRUE` (default), forces the candidate model weights to be
  non-negative.

- metric:

  The metric used to optimize the combination of weights.

## Value

A list containing two elements: `blended_model` (the stack after
blending) and `final_model` (a fully `fitted` `model_stack` object). The
`final_model` is ready for direct prediction with
[`predict()`](https://rspatial.github.io/terra/reference/predict.html).

## See also

Other h3sdm_tools:
[`h3sdm_recipe_gam()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_recipe_gam.md),
[`h3sdm_workflow_gam()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_workflow_gam.md)
