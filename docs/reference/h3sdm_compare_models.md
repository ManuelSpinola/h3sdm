# Compare multiple H3SDM species distribution models

Computes and combines performance metrics for multiple species
distribution models created with
[`h3sdm_fit_models()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_fit_models.md)
or similar workflows. Metrics include standard yardstick metrics (ROC
AUC, TSS, Boyce index, etc.). Returns a tibble summarizing model
performance.

## Usage

``` r
h3sdm_compare_models(h3sdm_results)
```

## Arguments

- h3sdm_results:

  A list or workflow set containing fitted models with a `metrics`
  tibble. Typically, this object is the output of
  [`h3sdm_fit_models()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_fit_models.md).

## Value

A tibble with one row per model per metric, containing:

- model:

  Model name

- .metric:

  Metric name (ROC AUC, TSS, Boyce, etc.)

- .estimator:

  Metric type (usually "binary")

- mean:

  Metric value

## Examples

``` r
# \donttest{
# Minimal reproducible example
example_metrics <- tibble::tibble(
  model = c("model1", "model2"),
  .metric = c("roc_auc", "tss_max"),
  .estimator = c("binary", "binary"),
  mean = c(0.85, 0.7)
)
example_results <- list(metrics = example_metrics)
h3sdm_compare_models(example_results)
#> # A tibble: 1 × 4
#>   model  .metric .estimator  mean
#>   <chr>  <chr>   <chr>      <dbl>
#> 1 model1 roc_auc binary      0.85
# }
```
