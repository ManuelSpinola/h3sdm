# Fits an SDM workflow to data using resampling and prepares it for stacking.

Fits a Species Distribution Model (SDM) workflow to resampling data
(cross-validation). This function is the main training step and
optionally configures the results to be used with the 'stacks' package.

## Usage

``` r
h3sdm_fit_model(
  workflow,
  data_split,
  presence_data = NULL,
  truth_col = "presence",
  pred_col = ".pred_1",
  for_stacking = FALSE,
  ...
)
```

## Arguments

- workflow:

  A 'workflow' object from tidymodels (e.g., GAM or Random Forest).

- data_split:

  An 'rsplit' or 'rset' object (e.g., result of vfold_cv or
  spatial_block_cv).

- presence_data:

  (Optional) Original presence data (used for extended metrics).

- truth_col:

  Column name of the response variable (defaults to "presence").

- pred_col:

  Column name for the prediction of the class of interest (defaults to
  ".pred_1").

- for_stacking:

  Logical. If `TRUE`, uses `control_stack_resamples()` to save all
  workflow information required for the 'stacks' package. If `FALSE`,
  uses the standard control with `save_pred = TRUE`.

- ...:

  Arguments passed on to other functions (e.g., to
  [`tune::fit_resamples`](https://tune.tidymodels.org/reference/fit_resamples.html)
  if needed).

## Value

A list with three elements:

- `cv_model`: The result of `fit_resamples()`.

- `final_model`: The model fitted to the entire training set (first
  split).

- `metrics`: Extended evaluation metrics (if `presence_data` is
  provided).
