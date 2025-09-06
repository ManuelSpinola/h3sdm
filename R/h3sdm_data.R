#' @name h3sdm_data
#' @title Combina datos para modelos de distribución de especies (SDM) basados en H3
#' @description Combines species presence-absence data with environmental predictors.
#'   It also calculates the centroid coordinates (x and y) for each hexagon.
#' @param pa_sf An sf object from the h3sdm_pa function containing species presence-absence data.
#' @param pred_sf An sf object from the h3sdm_pred function with environmental predictors.
#' @return An sf object containing species, environmental, and spatial data.
#' @importFrom dplyr left_join
#' @importFrom sf st_drop_geometry st_centroid st_coordinates
#' @examples
#' # Assuming you have a species dataset (my_species_pa) and a predictor dataset (my_predictors)
#' # my_species_pa <- h3sdm_pa(...)
#' # my_predictors <- h3sdm_predictors(...)
#' # combined_data <- h3sdm_data(my_species_pa, my_predictors)
#' @export

h3sdm_data <- function(pa_sf, predictors_sf) {

  # Validaciones iniciales
  if (!inherits(pa_sf, "sf") || !"h3_address" %in% names(pa_sf)) {
    stop("pa_sf must be a valid sf object with an 'h3_address' column.")
  }
  if (!inherits(pred_sf, "sf") || !"h3_address" %in% names(pred_sf)) {
    stop("pred_sf must be a valid sf object with an 'h3_address' column.")
  }

  # Unir los dataframes por la columna 'h3_address'.
  combined_sf <- pa_sf %>%
    dplyr::left_join(
      sf::st_drop_geometry(pred_sf),
      by = "h3_address"
    )

  # --- El cambio clave: Agregar las coordenadas x e y ---
  # Se extraen las coordenadas del centroide y se añaden como columnas
  # Se utiliza suppressWarnings() para evitar la advertencia de sf
  coords <- suppressWarnings(sf::st_coordinates(sf::st_centroid(combined_sf)))

  combined_sf <- combined_sf %>%
    dplyr::mutate(
      x = coords[, 1],
      y = coords[, 2]
    )

  return(combined_sf)
}
