#' @name h3sdm_fit_models
#' @title Fit and evaluate multiple H3SDM species distribution models
#' @description
#' Fits one or more species distribution models using tidymodels workflows and
#' a specified resampling scheme, then computes standard metrics (ROC AUC, accuracy,
#' sensitivity, specificity, F1-score, Kappa) along with TSS (True Skill Statistic)
#' and the Boyce index for model evaluation. Returns both the fitted models and
#' a comparative metrics table.
#'
#' @param workflows A named list of tidymodels workflows created with `h3sdm_workflow()` or manually.
#' @param data_split A resampling object (e.g., from `vfold_cv()` or `h3sdm_spatial_cv()`) for cross-validation.
#' @param presence_data An `sf` object or tibble with presence locations to compute the Boyce index (optional).
#' @param truth_col Character. Name of the column containing true presence/absence values (default `"presence"`).
#' @param pred_col Character. Name of the column containing predicted probabilities (default `".pred_1"`).
#'
#' @return A list with two elements:
#' \describe{
#'   \item{models}{A list of fitted models returned by `h3sdm_fit_model()`.}
#'   \item{metrics}{A tibble with one row per model per metric, including standard yardstick metrics, TSS, and Boyce index.}
#' }
#'
#' @examples
#' \dontrun{
#' # Define workflows for different models
#' mod_log <- logistic_reg() %>%
#'   set_engine("glm") %>%
#'   set_mode("classification")
#'
#' mod_rf <- rand_forest() %>%
#'   set_engine("ranger") %>%
#'   set_mode("classification")
#'
#' workflows_list <- list(
#'   logistic = h3sdm_workflow(mod_log, my_recipe),
#'   rf       = h3sdm_workflow(mod_rf, my_recipe)
#' )
#'
#' results <- h3sdm_fit_models(
#'   workflows     = workflows_list,
#'   data_split    = my_cv_folds,
#'   presence_data = presence_sf
#' )
#' metrics_table <- results$metrics
#' }
#'
#' @importFrom purrr imap map2_dfr
#' @importFrom dplyr mutate
#' @importFrom tibble tibble
#' @export

h3sdm_fit_models <- function(workflows, data_split, presence_data = NULL,
                             truth_col = "presence", pred_col = ".pred_1") {

  results <- purrr::imap(workflows, function(wf, name) {
    fit_res <- h3sdm_fit_model(wf, data_split, presence_data, truth_col, pred_col)
    fit_res$model_name <- name
    fit_res
  })

  # Combinar métricas en una tabla comparativa
  metrics_table <- purrr::map2_dfr(results, names(workflows), function(res, name) {
    if (!is.null(res$metrics)) {
      res$metrics %>%
        dplyr::mutate(model = name, .before = 1)
    } else {
      tibble::tibble(model = name, .metric = NA, mean = NA, std_err = NA, conf_low = NA, conf_high = NA)
    }
  })

  return(list(
    models  = results,
    metrics = metrics_table
  ))
}
