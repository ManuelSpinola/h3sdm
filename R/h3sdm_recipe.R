#' @name h3sdm_recipe
#' @title Create a tidymodels recipe for H3-based SDMs
#' @description This function prepares an sf object with H3 hexagonal data for
#'   modeling with the `tidymodels` ecosystem. It automatically extracts
#'   centroid coordinates and assigns appropriate roles to the variables.
#'
#' @param data An `sf` object, typically the result of the `h3sdm_data()`
#'   function, which includes species presence-absence data, H3 addresses, and
#'   environmental predictors. The geometry must be of type `MULTIPOLYGON`.
#'
#' @details The function handles several key data preparation steps:
#'   \itemize{
#'     \item It extracts the centroid coordinates (x and y) from the polygon
#'       geometries to be used as spatial predictors in the model.
#'     \item It removes the geometry column, as `tidymodels` works with
#'       tabular data.
#'     \item It explicitly assigns roles to key columns:
#'       \itemize{
#'         \item \strong{`presence`}: Assigned the `"outcome"` role, as it's the
#'           variable to be predicted.
#'         \item \strong{`h3_address`}: Assigned the `"id"` role, ensuring it is
#'           kept in the dataset but not used as a predictor. This is essential
#'           for later joining with prediction results.
#'         \item \strong{`x` and `y`}: Assigned the `"spatial_predictor"` role.
#'       }
#'   }
#'   All other columns are automatically assigned the default `"predictor"` role.
#'
#' @return A `tidymodels` recipe object ready to be used in a modeling workflow.
#'   It includes a custom class of `"h3sdm_recipe"`.
#'
#' @importFrom recipes recipe
#' @importFrom dplyr mutate
#' @importFrom sf st_coordinates st_centroid st_drop_geometry
#'
#' @examples
#' \dontrun{
#' # Assuming `combined_data` is an sf object from h3sdm_data()
#' # with a 'presence' variable and other predictors.
#'
#' # Create the tidymodels recipe
#' sdm_recipe <- h3sdm_recipe(combined_data)
#'
#' # View the recipe's structure
#' sdm_recipe
#'
#' # You can now use `sdm_recipe` in a tidymodels workflow.
#' }
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
