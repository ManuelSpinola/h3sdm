#' @name h3sdm_fit_models
#' @title Fit and evaluate multiple H3SDM species distribution models
#' @description
#' Fits one or more species distribution models using tidymodels workflows and
#' a specified resampling scheme, then computes standard metrics (ROC AUC, accuracy,
#' sensitivity, specificity, F1-score, Kappa) along with TSS (True Skill Statistic)
#' and the Boyce index for model evaluation. Returns both the fitted models and
#' a comparative metrics table.
#'
#' @param workflows A named list of tidymodels workflows created with `h3sdm_workflow` or manually.
#' @param data_split A resampling object (e.g., created with `vfold_cv` or `spatial_block_cv`) for cross-validation.
#' @param presence_data An `sf` object or tibble with presence locations to compute the Boyce index (optional).
#' @param id_col Character. Name of the column identifying spatial units (default `"h3_address"`).
#' @param truth_col Character. Name of the column containing true presence/absence values (default `"presence"`).
#'
#' @return A list with two elements:
#' \describe{
#'   \item{models}{A list of fitted models returned by `fit_resamples`.}
#'   \item{metrics}{A tibble with one row per model per metric, including standard yardstick metrics, TSS, and Boyce index.}
#' }
#'
#' @examples
#' \dontrun{
#' workflows_list <- list(
#'   logistic = h3sdm_workflow(mod_log, my_recipe),
#'   rf       = h3sdm_workflow(mod_rf, my_recipe)
#' )
#' results <- h3sdm_fit_models(
#'   workflows = workflows_list,
#'   data_split = my_cv_folds,
#'   presence_data = presence_sf
#' )
#' metrics_table <- results$metrics
#' }
#'
#' @export

h3sdm_fit_models <- function(workflows, data_split, presence_data = NULL,
                             truth_col = "presence", pred_col = ".pred_1") {

  results <- purrr::imap(workflows, function(wf, name) {
    fit_res <- h3sdm_fit(wf, data_split, presence_data, truth_col, pred_col)
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
