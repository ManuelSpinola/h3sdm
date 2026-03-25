# Fit and evaluate multiple H3SDM species distribution models

Fits one or more species distribution models using tidymodels workflows
and a specified resampling scheme, then computes standard metrics (ROC
AUC, accuracy, sensitivity, specificity, F1-score, Kappa) along with TSS
(True Skill Statistic) and the Boyce index for model evaluation. Returns
both the fitted models and a comparative metrics table.

## Usage

``` r
h3sdm_fit_models(
  workflows,
  data_split,
  presence_data = NULL,
  truth_col = "presence",
  pred_col = ".pred_1"
)
```

## Arguments

- workflows:

  A named list of tidymodels workflows created with
  [`h3sdm_workflow()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_workflow.md)
  or manually.

- data_split:

  A resampling object (e.g., from `vfold_cv()` or
  [`h3sdm_spatial_cv()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_spatial_cv.md))
  for cross-validation.

- presence_data:

  An `sf` object or tibble with presence locations to compute the Boyce
  index (optional).

- truth_col:

  Character. Name of the column containing true presence/absence values
  (default `"presence"`).

- pred_col:

  Character. Name of the column containing predicted probabilities
  (default `".pred_1"`).

## Value

A list with two elements:

- models:

  A list of fitted models returned by
  [`h3sdm_fit_model()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_fit_model.md).

- metrics:

  A tibble with one row per model per metric, including standard
  yardstick metrics, TSS, and Boyce index.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example requires prepared recipes and resampling objects
mod_log <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")

mod_rf <- rand_forest() %>%
  set_engine("ranger") %>%
  set_mode("classification")

workflows_list <- list(
  logistic = h3sdm_workflow(mod_log, my_recipe),
  rf       = h3sdm_workflow(mod_rf, my_recipe)
)

results <- h3sdm_fit_models(
  workflows     = workflows_list,
  data_split    = my_cv_folds,
  presence_data = presence_sf
)
metrics_table <- results$metrics
} # }
```
