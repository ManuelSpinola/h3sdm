#' @name h3sdm_data
#' @title Combine species and environmental data for SDMs using H3 grids
#' @description Combines species presence–absence data with environmental predictors.
#'   It also calculates centroid coordinates (x and y) for each hexagon grid cell.
#' @param pa_sf An `sf` object from `h3sdm_pa()` containing species presence–absence data.
#' @param predictors_sf An `sf` object from `h3sdm_predictors()` containing environmental predictors.
#' @return An `sf` object containing species presence–absence, environmental predictor variables,
#'   and centroid coordinates for each hexagon cell.
#' @importFrom dplyr left_join
#' @importFrom sf st_drop_geometry st_centroid st_coordinates
#' @examples
#' \dontrun{
#' my_species_pa <- h3sdm_pa("Panthera onca", res = 6)
#' my_predictors <- h3sdm_predictors(my_species_pa)
#' combined_data <- h3sdm_data(my_species_pa, my_predictors)
#' }
#' @export

h3sdm_data <- function(pa_sf, predictors_sf) {

  # Validaciones iniciales
  if (!inherits(pa_sf, "sf") || !"h3_address" %in% names(pa_sf)) {
    stop("pa_sf must be a valid sf object with an 'h3_address' column.")
  }
  if (!inherits(predictors_sf, "sf") || !"h3_address" %in% names(predictors_sf)) {
    stop("pred_sf must be a valid sf object with an 'h3_address' column.")
  }

  # Unir los dataframes por la columna 'h3_address'.
  combined_sf <- pa_sf %>%
    dplyr::left_join(
      sf::st_drop_geometry(predictors_sf),
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
