#' @name h3sdm_predictors
#' @title Combine Predictor Data from Multiple sf Objects
#'
#' @description
#' This function merges predictor variables from multiple `sf` objects
#' into a single `sf` object. It preserves the geometry from the first
#' input and joins columns from the other `sf` objects using a common
#' key (`h3_address` or `ID`).
#'
#' @param ... Two or more \code{sf} objects containing predictor variables.
#'   The first object must contain the geometry to preserve. All objects
#'   must share a common key column (\code{h3_address} or \code{ID}).
#'
#' @return An `sf` object containing the geometry of the first input and
#' all predictor columns from all provided `sf` objects.
#'
#' @details
#' The function uses a left join based on the `h3_address` column if present,
#' otherwise it falls back to `ID`. Geometries from the right-hand side `sf`
#' objects are dropped to avoid conflicts, and the final geometry is cast
#' to `MULTIPOLYGON`.
#'
#' @examples
#' \dontrun{
#' # Combine sf objects with different predictor types into one
#' combined <- h3sdm_predictors(num_sf, cat_sf, it_sf)
#' head(combined)
#' }
#'
#' @export

h3sdm_predictors <- function(...) {
  # Collect all sf objects in a list
  sf_list <- list(...)

  # Drop NULL inputs
  sf_list <- sf_list[!sapply(sf_list, is.null)]

  # Check that at least one object was provided
  if (length(sf_list) == 0) stop("At least one sf object must be provided.")

  # Check that all inputs are sf objects
  if (!all(sapply(sf_list, inherits, "sf"))) {
    stop("All inputs must be sf objects.")
  }

  # Choose the join key
  join_key <- if ("h3_address" %in% names(sf_list[[1]])) "h3_address" else "ID"

  # Start from the first sf object
  combined_sf <- sf_list[[1]]

  # Left-join the remaining objects without their geometry
  if (length(sf_list) > 1) {
    for (i in 2:length(sf_list)) {
      combined_sf <- dplyr::left_join(
        combined_sf,
        sf_list[[i]] %>% sf::st_drop_geometry(),
        by = join_key
      )
    }
  }

  # Make sure the geometry is MULTIPOLYGON
  combined_sf <- sf::st_cast(combined_sf, "MULTIPOLYGON")

  return(combined_sf)
}
