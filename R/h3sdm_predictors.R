#' @name h3sdm_predictors
#' @title Combine Predictor Data from Multiple sf Objects
#'
#' @description
#' This function merges predictor variables from multiple `sf` objects
#' into a single `sf` object. It preserves the geometry from the first
#' input (`num_sf`) and joins columns from the other `sf` objects
#' using a common key (`h3_address` or `ID`).
#'
#' @param num_sf An `sf` object containing numeric predictor variables.
#' @param cat_sf An `sf` object containing categorical predictor variables.
#' @param it_sf An `sf` object containing landscape or information theory metrics.
#'
#' @return An `sf` object containing the geometry of `num_sf` and all predictor
#' columns from `num_sf`, `cat_sf`, and `it_sf`.
#'
#' @details
#' The function uses a left join based on the `h3_address` column if present,
#' otherwise it falls back to `ID`. Geometries from the right-hand side `sf`
#' objects are dropped to avoid conflicts, and the final geometry is cast
#' to `MULTIPOLYGON`.
#'
#' @examples
#' \donttest{
#' # Assume you have 2 or more sf objects with predictor variables:
#' # num_sf: numeric predictors
#' # cat_sf: categorical predictors
#' # it_sf: landscape or information metrics
#'
#' # Combine them into a single sf object
#' combined <- h3sdm_predictors(num_sf, cat_sf, it_sf)
#'
#' # Resulting object contains geometry from num_sf and all attribute columns
#' head(combined)
#' }
#' @export

h3sdm_predictors <- function(...) {
  # Recibir todos los objetos sf como lista
  sf_list <- list(...)

  # Filtrar NULLs
  sf_list <- sf_list[!sapply(sf_list, is.null)]

  # Validar que haya al menos uno
  if (length(sf_list) == 0) stop("At least one sf object must be provided.")

  # Validar que todos sean sf
  if (!all(sapply(sf_list, inherits, "sf"))) {
    stop("All inputs must be sf objects.")
  }

  # Determinar la clave de unión
  join_key <- if ("h3_address" %in% names(sf_list[[1]])) "h3_address" else "ID"

  # Empezar con el primer sf
  combined_sf <- sf_list[[1]]

  # Iterar sobre el resto y hacer left_join sin geometría
  if (length(sf_list) > 1) {
    for (i in 2:length(sf_list)) {
      combined_sf <- dplyr::left_join(
        combined_sf,
        sf_list[[i]] %>% sf::st_drop_geometry(),
        by = join_key
      )
    }
  }

  # Asegurar que la geometría sea MULTIPOLYGON
  combined_sf <- sf::st_cast(combined_sf, "MULTIPOLYGON")

  return(combined_sf)
}
