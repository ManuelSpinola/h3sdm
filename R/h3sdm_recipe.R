#' @name h3sdm_recipe
#' @title Create a tidymodels recipe for H3-based SDMs
#' @description
#' Prepares an `sf` object with H3 hexagonal data for modeling with the
#' `tidymodels` ecosystem. Extracts centroid coordinates, assigns appropriate
#' roles to the variables automatically, and returns a ready-to-use recipe for
#' modeling species distributions.
#'
#' @param data An `sf` object, typically the output of `h3sdm_data()`,
#'   including species presence-absence, H3 addresses, and environmental predictors.
#'   The geometry must be of type `MULTIPOLYGON`.
#'
#' @details
#' This function prepares spatial H3 grid data for species distribution modeling:
#' \itemize{
#'   \item Extracts centroid coordinates (`x` and `y`) from MULTIPOLYGON geometries using sf functions.
#'   \item Removes the geometry column to create a purely tabular dataset for tidymodels.
#'   \item Assigns roles to columns:
#'     \itemize{
#'       \item `presence` → `"outcome"` (target variable)
#'       \item `h3_address` → `"id"` (used for joining predictions later)
#'       \item `x` and `y` → `"spatial_predictor"`
#'     }
#'   \item All other columns are assigned `"predictor"` role.
#' }
#'
#' @return A `tidymodels` recipe object (class `"h3sdm_recipe"`) ready for modeling.
#'
#' @examples
#' \dontrun{
#' # Example: Prepare H3 hexagonal SDM data for modeling
#' # `combined_data` is typically the output of h3sdm_data()
#' sdm_recipe <- h3sdm_recipe(combined_data)
#' sdm_recipe  # inspect the recipe object
#' }
#'
#' @importFrom recipes recipe
#' @importFrom dplyr mutate
#' @importFrom sf st_coordinates st_centroid st_geometry st_drop_geometry
#'
#' @export


h3sdm_recipe <- function(data) {
  if (!inherits(data, "sf")) stop("data must be an sf object.")

  # Extraer geometrías para calcular centroides sin atributos
  centroids <- sf::st_centroid(sf::st_geometry(data))
  coords <- sf::st_coordinates(centroids)

  # Combinar coordenadas con los atributos del sf
  data_no_geom <- data %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(
      x = coords[, 1],
      y = coords[, 2]
    )

  # Preparar variables y roles
  all_vars <- names(data_no_geom)
  roles <- rep("predictor", length(all_vars))
  roles[all_vars == "presence"] <- "outcome"
  roles[all_vars == "h3_address"] <- "id"
  roles[all_vars == "x"] <- "spatial_predictor"
  roles[all_vars == "y"] <- "spatial_predictor"

  # Crear la receta
  rec <- recipes::recipe(
    x = data_no_geom,
    vars = all_vars,
    roles = roles,
    strings_as_factors = FALSE
  )

  class(rec) <- c("h3sdm_recipe", class(rec))
  return(rec)
}
