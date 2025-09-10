#' @name h3sdm_compare_models
#' @title Compare multiple H3SDM species distribution models
#' @description
#' Computes and combines performance metrics for multiple species distribution models
#' created with `h3sdm_fit` or similar workflows. Metrics include standard yardstick
#' metrics (ROC AUC, TSS, Boyce index, etc.). Returns a tibble summarizing model performance.
#'
#' @param h3sdm_results A list or workflow set containing fitted models with a `metrics` tibble.
#'
#' @return A tibble with one row per model per metric, containing:
#'   \describe{
#'     \item{model}{Model name}
#'     \item{.metric}{Metric name (ROC AUC, TSS, Boyce, etc.)}
#'     \item{.estimator}{Metric type (usually "binary")}
#'     \item{mean}{Metric value}
#'   }
#'
#' @examples
#' \donttest{
#' # Minimal reproducible example
#' example_metrics <- tibble::tibble(
#'   model = c("model1", "model2"),
#'   .metric = c("roc_auc", "tss_max"),
#'   .estimator = c("binary", "binary"),
#'   mean = c(0.85, 0.7)
#' )
#' example_results <- list(metrics = example_metrics)
#' h3sdm_compare_models(example_results)
#' }
#'
#' @export

h3sdm_compare_models <- function(h3sdm_results) {
  # Check that metrics exist
  if (is.null(h3sdm_results$metrics) || nrow(h3sdm_results$metrics) == 0) {
    stop("No metrics found in h3sdm_results$metrics")
  }

  # Sort the table by AUC (or any other metric you want to prioritize)
  metrics_sorted <- h3sdm_results$metrics %>%
    dplyr::filter(.metric %in% c("roc_auc", "tss_max", "boyce")) %>%
    dplyr::arrange(dplyr::desc(mean))

  return(metrics_sorted)
}
