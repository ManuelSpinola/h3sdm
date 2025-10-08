#' @name h3sdm_explain
#' @title Create a DALEX explainer for h3sdm workflows
#' @description Creates a DALEX explainer for a species distribution model fitted
#'   with `h3sdm_fit_model()`. Prepares response and predictor variables,
#'   ensuring that all columns used during model training (including `h3_address`
#'   and coordinates) are included. The explainer can be used for feature
#'   importance, model residuals, and other DALEX diagnostics.
#'
#' @param model A fitted workflow returned by `h3sdm_fit_model()`.
#' @param data A `data.frame` or `sf` object containing the original predictors
#'   and response variable. If an `sf` object, geometry is dropped automatically.
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
#' library(parsnip)
#'
#' dat <- data.frame(
#'   x1 = rnorm(20),
#'   x2 = rnorm(20),
#'   presence = factor(sample(0:1, 20, replace = TRUE))
#' )
#'
#' model <- logistic_reg() |>
#'   fit(presence ~ x1 + x2, data = dat)
#'
#' explainer <- h3sdm_explain(model, data = dat, response = "presence")
#' feature_importance(explainer)
#' }
#'
#' @importFrom DALEX feature_importance model_performance predict_parts
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
