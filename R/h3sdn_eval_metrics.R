#' @name h3sdm_eval_metrics
#' @title Evaluate performance metrics for a fitted H3SDM model
#' @description
#' Computes a set of performance metrics for a single fitted species distribution model.
#' Includes standard yardstick metrics such as ROC AUC, accuracy, sensitivity,
#' specificity, F1-score, Kappa, as well as ecological metrics such as the
#' True Skill Statistic (TSS) and Boyce index.
#' This function is designed as a helper for evaluating models produced by
#' `h3sdm_fit_model` or `h3sdm_fit_models`.
#'
#' @param fitted_model A fitted model object, typically the output of `h3sdm_fit_model()`.
#' @param presence_data Optional. An `sf` object or tibble containing presence locations
#'   used to compute the Boyce index. If not provided, the Boyce index will not be calculated.
#' @param truth_col Character. Name of the column containing the true presence/absence values
#'   (default `"presence"`).
#' @param pred_col Character. Name of the column containing predicted probabilities
#'   (default `".pred_1"`).
#'
#' @return A tibble with one row per metric, containing:
#' \describe{
#'   \item{.metric}{Metric name (e.g., "roc_auc", "tss", "boyce").}
#'   \item{.estimator}{Estimator type (usually "binary").}
#'   \item{mean}{Metric value.}
#'   \item{std_err}{Standard error (NA for TSS and Boyce).}
#'   \item{conf_low}{Lower bound of the 95% confidence interval (NA for TSS and Boyce).}
#'   \item{conf_high}{Upper bound of the 95% confidence interval (NA for TSS and Boyce).}
#' }
#'
#' @details
#' This function centralizes model evaluation for a single fitted H3SDM model,
#' combining both general classification metrics and ecological indices.
#' It is especially useful for systematically comparing model performance
#' across species or modeling approaches.
#'
#' @examples
#' \dontrun{
#' # Assuming 'fitted' is the result of h3sdm_fit_model()
#' metrics <- h3sdm_eval_metrics(
#'   fitted_model = fitted,
#'   presence_data = presence_sf,
#'   truth_col = "presence",
#'   pred_col = ".pred_1"
#' )
#' print(metrics)
#' }
#'
#' @importFrom tune collect_metrics collect_predictions
#' @importFrom yardstick sens_vec spec_vec accuracy_vec roc_auc_vec
#' @importFrom dplyr mutate bind_rows select
#' @importFrom tibble tibble
#'
#' @export

h3sdm_eval_metrics <- function(fitted_model, presence_data = NULL,
                               truth_col = "presence", pred_col = ".pred_1") {

  # 1️⃣ Collect standard metrics from the fitted model.
  standard_metrics <- tune::collect_metrics(fitted_model)

  # Calculate the confidence interval for the traditional metrics
  metrics_with_ci <- standard_metrics %>%
    dplyr::mutate(
      conf_low = mean - 1.96 * std_err,
      conf_high = mean + 1.96 * std_err,
      .groups = "drop"
    ) %>%
    dplyr::select(.metric, .estimator, mean, std_err, conf_low, conf_high)

  # 2️⃣ Collect all predictions for TSS and Boyce
  all_preds <- tune::collect_predictions(fitted_model) %>%
    dplyr::mutate(
      truth = .data[[truth_col]],
      pred  = .data[[pred_col]]
    )

  # 3️⃣ Calculate a single, global TSS value from all predictions
  tss_val <- {
    ths <- seq(0.01, 0.99, by = 0.01)
    max(sapply(ths, function(th) {
      binary <- factor(ifelse(all_preds$pred >= th, "1", "0"), levels = c("0", "1"))
      yardstick::sens_vec(all_preds$truth, binary) + yardstick::spec_vec(all_preds$truth, binary) - 1
    }))
  }

  # 4️⃣ Calculate a single, global Boyce value from all predictions
  boyce_val <- if (!is.null(presence_data)) {
    obs_vals <- all_preds$pred[all_preds$truth == "1" | all_preds$truth == "presence"]
    tryCatch(
      ecospat::ecospat.boyce(fit = all_preds$pred, obs = obs_vals, PEplot = FALSE)$cor,
      error = function(e) NA_real_
    )
  } else NA_real_

  # 5️⃣ Combine all the metrics into a single tibble
  final_metrics <- dplyr::bind_rows(
    metrics_with_ci,
    tibble::tibble(
      .metric = c("tss", "boyce"),
      .estimator = "binary",
      mean = c(tss_val, boyce_val),
      std_err = NA_real_,
      conf_low = NA_real_,
      conf_high = NA_real_
    )
  )

  return(final_metrics)
}
