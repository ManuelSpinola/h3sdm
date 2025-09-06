#' @name h3sdm_predict
#' @title Predict species distribution with H3 hexagons
#' @description Uses a fitted tidymodels workflow to predict species
#'   presence probability on a new spatial grid. This function
#'   automatically handles the creation of 'x' and 'y' coordinates
#'   if they are not present in the input data.
#' @param sdm_workflow A fitted `tidymodels` workflow object,
#'   typically the result of `workflows::fit()`.
#' @param new_data An `sf` object containing the spatial grid and
#'   the same predictor variables used for model training.
#' @return An `sf` object with a new column named `prediction` containing
#'   the predicted probability of presence for each hexagon.
#' @importFrom sf st_drop_geometry st_centroid st_coordinates
#' @importFrom dplyr mutate
#' @export

h3sdm_predict <- function(fit_object, new_data) {
  # Si se pasa la lista completa de h3sdm_fit, usar el workflow final
  if (is.list(fit_object) && "final_model" %in% names(fit_object)) {
    workflow <- fit_object$final_model
  } else {
    workflow <- fit_object
  }

  # Crear coordenadas si faltan
  if (!all(c("x", "y") %in% names(new_data))) {
    message("Missing x and y coordinates. Creating them from polygon centroids.")
    coords <- suppressWarnings(sf::st_coordinates(sf::st_centroid(new_data)))
    new_data <- new_data %>%
      dplyr::mutate(x = coords[, 1], y = coords[, 2])
  }

  # Eliminar geometría
  new_data_no_geom <- sf::st_drop_geometry(new_data)

  # Predicciones
  predictions <- predict(workflow, new_data = new_data_no_geom, type = "prob")

  # Asignar predicción de presencia
  if (".pred_1" %in% names(predictions)) {
    new_data$prediction <- predictions$.pred_1
  } else {
    stop("No '.pred_1' column found in predictions. Check your workflow and response variable.")
  }

  return(new_data)
}
