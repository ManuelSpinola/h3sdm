#' Predict species presence probability or counts using H3 hexagons
#'
#' Uses a fitted tidymodels workflow (from `h3sdm_fit_model` or a standalone workflow)
#' to predict species presence probabilities or counts on a new spatial H3 grid.
#' Automatically generates centroid coordinates (`x` and `y`) if missing.
#' The `new_data` must contain the same predictor variables as used in model training.
#' Model mode (classification or regression) is detected automatically.
#'
#' @param fit_object A fitted `tidymodels` workflow or the output list from `h3sdm_fit_model`.
#' @param new_data An `sf` object containing the spatial grid and the same predictor variables used for model training.
#'
#' @return An `sf` object with the original geometry and a new column `prediction` containing
#'   the predicted probability of presence (classification) or predicted count (regression)
#'   for each hexagon.
#'
#' @examples
#' \dontrun{
#' # Predict presence probabilities on a new hex grid
#' predictions_sf <- h3sdm_predict(
#'   fit_object = fitted_model,
#'   new_data   = grid_sf
#' )
#' }
#'
#' @seealso [h3sdm_fit_model()], [h3sdm_aoa()]
#'
#' @importFrom sf st_drop_geometry st_centroid st_coordinates
#' @importFrom dplyr mutate
#' @export

h3sdm_predict <- function(fit_object, new_data) {
  # If the full list from h3sdm_fit_model() is passed, use the final workflow
  if (is.list(fit_object) && "final_model" %in% names(fit_object)) {
    workflow <- fit_object$final_model
  } else {
    workflow <- fit_object
  }

  # Detect model mode automatically
  model_mode <- workflow$fit$actions$model$spec$mode

  # Add centroid coordinates if missing
  if (!all(c("x", "y") %in% names(new_data))) {
    message("Missing x and y coordinates. Creating them from polygon centroids.")
    coords <- suppressWarnings(sf::st_coordinates(sf::st_centroid(new_data)))
    new_data <- new_data %>%
      dplyr::mutate(x = coords[, 1], y = coords[, 2])
  }

  # Drop geometry before predicting
  new_data_no_geom <- sf::st_drop_geometry(new_data)

  # Generate predictions according to model mode
  if (model_mode == "regression") {
    predictions <- predict(workflow, new_data = new_data_no_geom, type = "numeric")
    new_data$prediction <- predictions$.pred
  } else {
    predictions <- predict(workflow, new_data = new_data_no_geom, type = "prob")
    if (".pred_1" %in% names(predictions)) {
      new_data$prediction <- predictions$.pred_1
    } else {
      stop("No '.pred_1' column found in predictions. Check your workflow and response variable.")
    }
  }

  return(new_data)
}
