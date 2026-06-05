# Evaluate performance metrics for a fitted H3SDM model

Computes a set of performance metrics for a single fitted species
distribution model. Includes standard yardstick metrics such as ROC AUC,
accuracy, sensitivity, specificity, F1-score, Kappa, as well as
ecological metrics such as the True Skill Statistic (TSS) and Boyce
index. This function is designed as a helper for evaluating models
produced by `h3sdm_fit_model` or `h3sdm_fit_models`.

## Usage

``` r
h3sdm_eval_metrics(
  fitted_model,
  presence_data = NULL,
  truth_col = "presence",
  pred_col = ".pred_1"
)
```

## Arguments

- fitted_model:

  A fitted model object, typically the output of
  [`h3sdm_fit_model()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_fit_model.md).

- presence_data:

  Optional. An `sf` object or tibble containing presence locations used
  to compute the Boyce index. If not provided, the Boyce index will not
  be calculated.

- truth_col:

  Character. Name of the column containing the true presence/absence
  values (default `"presence"`).

- pred_col:

  Character. Name of the column containing predicted probabilities
  (default `".pred_1"`).

## Value

A tibble with one row per metric, containing:

- .metric:

  Metric name (e.g., "roc_auc", "tss", "boyce").

- .estimator:

  Estimator type (usually "binary").

- mean:

  Metric value.

- std_err:

  Standard error (NA for TSS and Boyce).

- conf_low:

  Lower bound of the 95% confidence interval (NA for TSS and Boyce).

- conf_high:

  Upper bound of the 95% confidence interval (NA for TSS and Boyce).

## Details

This function centralizes model evaluation for a single fitted H3SDM
model, combining both general classification metrics and ecological
indices. It is especially useful for systematically comparing model
performance across species or modeling approaches.

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming 'fitted' is the result of h3sdm_fit_model()
metrics <- h3sdm_eval_metrics(
  fitted_model = fitted,
  presence_data = presence_sf,
  truth_col = "presence",
  pred_col = ".pred_1"
)
print(metrics)
} # }
```
