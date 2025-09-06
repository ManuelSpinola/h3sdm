#' @name h3sdm_eval_metrics
#' @title Evaluate a single fitted model
#' @description A helper function to calculate all metrics for a single fitted model.
#' @param fitted_model The fitted model object.
#' @param presence_data A tibble with the true presence locations.
#' @return A tibble with all evaluation metrics.
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
