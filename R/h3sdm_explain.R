#' Create a DALEX explainer for h3sdm workflows
#'
#' This function creates a DALEX explainer for a species distribution model
#' fitted with `h3sdm_fit_model()`. It prepares the response and predictor
#' variables, ensuring that all columns used during model training (including
#' `h3_address` and coordinates) are included. The explainer can then be used
#' for feature importance, model residuals, and other DALEX diagnostics.
#'
#' @name h3sdm_explain
#' @title DALEX Explainer for h3sdm Workflows
#' @description Combines species presence/absence data with predictor variables
#'   and generates a DALEX explainer suitable for model diagnostics.
#'
#' @param model A fitted workflow returned by `h3sdm_fit_model()`.
#' @param data A `data.frame` or `sf` object containing the original predictors
#'   and response variable. If an `sf` object, the geometry is dropped automatically.
#' @param response Character string specifying the name of the response column.
#'   Must be a binary factor or numeric vector (0/1). Defaults to `"presence"`.
#' @param label Character string specifying a label for the explainer. Defaults
#'   to `"h3sdm workflow"`.
#'
#' @return An object of class `explainer` from the **DALEX** package, ready to be
#'   used with `feature_importance()`, `model_performance()`, `predict_parts()`,
#'   and other DALEX functions.
#'
#' @examples
#' \donttest{
#' library(h3sdm)
#' library(DALEX)
#'
#' # Assume dat is your prepared h3sdm dataset
#' # and f$final_model is a fitted workflow
#' expl <- h3sdm_explain(f$final_model, data = dat)
#' feature_importance(ex)
#' }
#'
#' @export
h3sdm_explain <- function(model, data, response = "presence", label = "h3sdm workflow") {

  # Drop geometry if it exists
  if (inherits(data, "sf")) data <- sf::st_drop_geometry(data)

  # Prepare the response variable
  y <- data[[response]]
  if (is.factor(y)) y <- as.numeric(as.character(y))

  # Pass all columns except the response to DALEX
  # Ensures 'h3_address' is included if present
  X <- data[, setdiff(names(data), response), drop = FALSE]

  # Define a custom prediction function (probability of presence)
  custom_predict <- function(object, newdata) {
    preds <- predict(object, newdata, type = "prob")
    as.data.frame(preds)[, 2]
  }

  # Build the explainer object
  explainer <- DALEX::explain(
    model = model,
    data = X,
    y = y,
    label = label,
    predict_function = custom_predict,
    colorize = FALSE,
    verbose = TRUE
  )

  return(explainer)
}
