#' @name h3sdm_fit_model
#' @title Fit a single H3SDM species distribution model
#' @description
#' Trains a single tidymodels workflow using a spatial split, computes performance metrics
#' for each fold, and fits the final model to the full dataset. Optionally computes
#' post-hoc metrics like TSS and the Boyce index.
#'
#' @param sdm_workflow A `workflow` object from `h3sdm_workflow()` or manually created.
#' @param data_split A resampling object (e.g., from `vfold_cv()` or `h3sdm_spatial_cv()`) for cross-validation.
#' @param presence_data Optional `sf` or tibble with presence locations to compute Boyce index.
#' @param truth_col Character. Name of the column containing the true presence/absence values (default `"presence"`).
#' @param pred_col Character. Name of the column containing predicted probabilities (default `".pred_1"`).
#'
#' @return A list with:
#' \describe{
#'   \item{cv_model}{Results of cross-validation (`tune_results`).}
#'   \item{final_model}{Fitted workflow on full data.}
#'   \item{metrics}{Tibble with performance metrics including ROC AUC, accuracy, sensitivity,
#'                 specificity, F1-score, Kappa, TSS, and Boyce index.}
#' }
#'
#' @importFrom tune fit_resamples control_resamples
#' @importFrom yardstick roc_auc_vec accuracy_vec sens_vec spec_vec f_meas_vec kap_vec
#' @importFrom workflows fit
#' @importFrom rsample analysis
#' @importFrom purrr imap map2_dfr
#' @importFrom dplyr mutate
#' @importFrom tibble tibble
#' @examples
#' \dontrun{
#' fitted <- h3sdm_fit_model(
#'   sdm_workflow  = my_workflow,
#'   data_split    = my_cv,
#'   presence_data = my_presence_sf
#' )
#' }
#'
#' @export

h3sdm_fit_model <- function(sdm_workflow, data_split, presence_data = NULL, truth_col = "presence", pred_col = ".pred_1") {

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
