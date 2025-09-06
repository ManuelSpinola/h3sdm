#' @name h3sdm_compare_models
#' @title Compare multiple H3SDM species distribution models
#' @description
#' Computes and combines performance metrics for multiple species distribution models
#' created with `h3sdm_fit` or similar workflows. Metrics include standard yardstick
#' metrics (ROC AUC, accuracy, etc.), TSS (True Skill Statistic), and Boyce index.
#' Returns a tibble summarizing model performance across all metrics.
#'
#' @param h3sdm_results A workflow set or list of fitted models returned by `h3sdm_fit_models` or `workflow_map`.
#' @param presence_data An `sf` object or tibble containing the presence points for computing the Boyce index.
#' @param id_col Character. Name of the column identifying spatial units (default `"h3_address"`).
#' @param pred_col Character. Name of the predicted probability column (default `".pred_presence"`).
#' @param truth_col Character. Name of the column with true presence/absence values (default `"presence"`).
#'
#' @return A tibble with one row per model per metric, containing:
#'   \describe{
#'     \item{model}{Model name}
#'     \item{.metric}{Metric name (ROC AUC, accuracy, TSS, Boyce, etc.)}
#'     \item{.estimator}{Metric type (usually "binary")}
#'     \item{mean}{Metric value}
#'   }
#'
#' @examples
#' \dontrun{
#' metrics_table <- h3sdm_compare_models(
#'   h3sdm_results = h3sdm_models_fitted,
#'   presence_data = presence_sf
#' )
#' }
#'
#' @export

h3sdm_compare_models <- function(h3sdm_results) {
  # Revisar que haya métricas
  if (is.null(h3sdm_results$metrics) || nrow(h3sdm_results$metrics) == 0) {
    stop("No hay métricas en h3sdm_results$metrics")
  }

  # Ordenar tabla por AUC (o cualquier métrica que quieras priorizar)
  metrics_sorted <- h3sdm_results$metrics %>%
    dplyr::filter(.metric %in% c("roc_auc", "tss_max", "boyce")) %>%
    dplyr::arrange(dplyr::desc(mean))

  return(metrics_sorted)
}
