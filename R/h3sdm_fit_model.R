#' @name h3sdm_fit_model
#' @title Fit a single H3SDM species distribution model
#' @description
#' Fits a single species distribution model (SDM) workflow using spatial cross-validation.
#' Computes performance metrics for each fold and fits the final model to the entire dataset.
#' Optionally calculates post-hoc metrics such as True Skill Statistic (TSS) and Boyce index.
#'
#' @param sdm_workflow A `workflow` object from `h3sdm_workflow()` or manually created.
#' @param data_split A resampling object (e.g., from `vfold_cv()` or `h3sdm_spatial_cv()`) used for cross-validation.
#' @param presence_data Optional `sf` or tibble object containing presence locations to compute Boyce index.
#' @param truth_col Character string specifying the name of the column containing the true presence/absence values. Defaults to `"presence"`.
#' @param pred_col Character string specifying the name of the column containing predicted probabilities. Defaults to `".pred_1"`.
#'
#' @return A list containing:
#' \describe{
#'   \item{cv_model}{Cross-validation results (`tune_results`).}
#'   \item{final_model}{Fitted workflow on the full dataset.}
#'   \item{metrics}{A tibble of performance metrics including ROC AUC, accuracy, sensitivity,
#'                 specificity, F1-score, Kappa, TSS, and Boyce index.}
#' }
#'
#' @examples
#' \dontrun{
#' library(h3sdm)
#' library(tidymodels)
#' library(sf)
#'
#' # Simular datos espaciales
#' coords <- cbind(
#'   x = runif(20, -100, 100),
#'   y = runif(20, -100, 100)
#' )
#' dat_sf <- st_as_sf(
#'   data.frame(
#'     x1 = rnorm(20),
#'     x2 = rnorm(20),
#'     presence = factor(sample(0:1, 20, replace = TRUE))
#'   ),
#'   coords = c("x", "y"),
#'   crs = 4326
#' )
#'
#' # Crear receta
#' rec <- recipe(presence ~ x1 + x2, data = dat_sf)
#'
#' # Modelo simple
#' mod_spec <- logistic_reg() %>%
#'   set_engine("glm") %>%
#'   set_mode("classification")
#'
#' # Crear workflow
#' my_workflow <- workflow() %>%
#'   add_model(mod_spec) %>%
#'   add_recipe(rec)
#'
#' # Resampling espacial
#' my_cv <- vfold_cv(dat_sf, v = 3)
#'
#' # Ajustar modelo
#' fitted <- h3sdm_fit_model(
#'   sdm_workflow  = my_workflow,
#'   data_split    = my_cv,
#'   presence_data = dat_sf
#' )
#' }
#'
#' @importFrom tune fit_resamples control_resamples
#' @importFrom yardstick roc_auc_vec accuracy_vec sens_vec spec_vec f_meas_vec kap_vec
#' @importFrom workflows fit
#' @importFrom rsample analysis
#' @importFrom purrr imap map2_dfr
#' @importFrom dplyr mutate
#' @importFrom tibble tibble
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

  # Convertir sf a data.frame si es necesario
  if (!is.null(presence_data) && inherits(presence_data, "sf")) {
    presence_data <- sf::st_drop_geometry(presence_data)
  }

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
