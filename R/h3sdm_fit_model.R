#' @name h3sdm_fit_model
#' @title Fit a tidymodels workflow with spatial data
#' @description This function trains a tidymodels workflow using a spatial split,
#'   calculating performance metrics for each fold.
#' @param sdm_workflow A `workflow` object from `h3sdm_workflow()`.
#' @param data_split An `rsplit` object from `h3sdm_split()`.
#' @return An `rsample_results` object with performance metrics for each fold.
#' @importFrom tune fit_resamples
#' @importFrom yardstick roc_auc_vec
#' @examples
#' # Assuming a workflow and a data split are defined
#' # sdm_wf <- h3sdm_workflow(...)
#' # spatial_split <- h3sdm_split(...)
#' # fitted_model <- h3sdm_fit(sdm_wf, spatial_split)
#' @export

h3sdm_fit_model <- function(sdm_workflow, data_split, presence_data = NULL,
                      truth_col = "presence", pred_col = ".pred_1") {

  sdm_metrics <- yardstick::metric_set(roc_auc, accuracy, sens, spec, f_meas, kap)

  # Cross-validation
  cv_model <- tune::fit_resamples(
    object    = sdm_workflow,
    resamples = data_split,
    metrics   = sdm_metrics,
    control   = tune::control_resamples(save_pred = TRUE)
  )

  # Modelo final sobre todos los datos
  final_model <- workflows::fit(sdm_workflow, data = rsample::analysis(data_split$splits[[1]]))

  # Métricas extendidas
  final_metrics <- NULL
  if (!is.null(presence_data)) {
    final_metrics <- h3sdm_eval_metrics(
      fitted_model  = cv_model,
      presence_data = presence_data,
      truth_col     = truth_col,
      pred_col      = pred_col
    )
  }

  return(list(
    cv_model     = cv_model,
    final_model  = final_model,
    metrics      = final_metrics
  ))
}
