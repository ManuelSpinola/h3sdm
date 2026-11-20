#' @name h3sdm_fit_model
#'
#' @title Fits an SDM workflow to data using resampling and prepares it for stacking.
#'
#' @description
#' Fits a Species Distribution Model (SDM) workflow to resampling data (cross-validation).
#' This function is the main training step and optionally configures the results
#' to be used with the 'stacks' package. Supports both classification (presence/absence)
#' and regression (count-based) models, detected automatically from the workflow mode.
#'
#' @param workflow A 'workflow' object from tidymodels (e.g., GAM or Random Forest).
#' @param data_split An 'rsplit' or 'rset' object (e.g., result of vfold_cv or spatial_block_cv).
#' @param presence_data (Optional) Original presence data (used for extended metrics).
#' @param truth_col Column name of the response variable. Defaults to \code{"presence"} for
#'   classification models and \code{"count"} for regression models.
#' @param pred_col Column name for the prediction of the class of interest. Defaults to
#'   \code{".pred_1"} for classification models and \code{".pred"} for regression models.
#' @param for_stacking Logical. If \code{TRUE}, uses \code{control_stack_resamples()}
#'   to save all workflow information required for the 'stacks' package.
#'   If \code{FALSE}, uses the standard control with \code{save_pred = TRUE}.
#' @param ... Arguments passed on to other functions (e.g., to \code{tune::fit_resamples} if needed).
#'
#' @details
#' For classification models, the class \code{"1"} (presence) is treated as the
#' event when computing sensitivity, specificity and F1-score, regardless of the
#' order of the factor levels in the response variable.
#'
#' @return A list with three elements:
#' \itemize{
#'   \item \code{cv_model}: The result of \code{fit_resamples()}.
#'   \item \code{final_model}: The model fitted to all observations in \code{data_split}.
#'   \item \code{metrics}: Extended evaluation metrics (if \code{presence_data} is provided).
#' }
#' @export
#'
#' @importFrom yardstick metric_set roc_auc accuracy sens spec f_meas kap rmse rsq mae
#' @importFrom tune fit_resamples control_resamples
#' @importFrom workflows fit
#' @importFrom rsample analysis
#' @importFrom sf st_drop_geometry
#'
h3sdm_fit_model <- function(workflow, data_split, presence_data = NULL,
                            truth_col = NULL, pred_col = NULL,
                            for_stacking = FALSE, ...) {

  # Detect model mode automatically
  model_mode <- workflow$fit$actions$model$spec$mode

  if (model_mode == "regression") {
    sdm_metrics <- yardstick::metric_set(rmse, rsq, mae)
    if (is.null(truth_col)) truth_col <- "count"
    if (is.null(pred_col))  pred_col  <- ".pred"
    # Not used for regression metrics; defined so the control call works
    event_level <- "first"
  } else {
    sdm_metrics <- yardstick::metric_set(roc_auc, accuracy, sens, spec, f_meas, kap)
    if (is.null(truth_col)) truth_col <- "presence"
    if (is.null(pred_col))  pred_col  <- ".pred_1"
    # Treat presence ("1") as the event for sens, spec and f_meas
    outcome_levels <- levels(data_split$splits[[1]]$data[[truth_col]])
    event_level <- if (outcome_levels[1] == "1") "first" else "second"
  }

  # 1. Dynamic control configuration
  if (for_stacking) {
    # Same settings as stacks::control_stack_resamples(), plus event_level
    resamples_control <- tune::control_resamples(
      save_pred = TRUE, save_workflow = TRUE, event_level = event_level
    )
  } else {
    # Standard control: Saves predictions for evaluation
    resamples_control <- tune::control_resamples(
      save_pred = TRUE, event_level = event_level
    )
  }

  # 2. Cross-validation
  cv_model <- tune::fit_resamples(
    object    = workflow,
    resamples = data_split,
    metrics   = sdm_metrics,
    control   = resamples_control
  )

  # 3. Return for STACKING
  if (for_stacking) {
    # Returns the PURE cv_model object (solution for stacks!)
    return(cv_model)
  }

  # --- Normal Flow (NO Stacking): Full List Return ---

  # Initialization (FIX: avoids 'object final_metrics not found' error)
  final_metrics <- NULL

  # 4. Final model fitting
  final_model <- workflows::fit(workflow, data = data_split$splits[[1]]$data)

  # 5. Final Metrics Calculation
  if (!is.null(presence_data)) {
    # Ensures data has no geometry for calculation
    if (inherits(presence_data, "sf")) {
      presence_data <- sf::st_drop_geometry(presence_data)
    }

    # Assumes h3sdm_eval_metrics is defined and uses cv_model
    final_metrics <- h3sdm_eval_metrics(
      fitted_model  = cv_model,
      presence_data = presence_data,
      truth_col     = truth_col,
      pred_col      = pred_col
    )
  }

  # 6. Return the complete list (for analysis/inspection)
  return(list(
    cv_model    = cv_model,
    final_model = final_model,
    metrics     = final_metrics
  ))
}
