#' Combine extracted numeric, categorical, and landscape metrics into a single sf
#'
#' This function merges three pre-computed sf objects—numeric raster values,
#' categorical raster proportions, and landscape metrics—into a single sf object.
#' The resulting sf has all variables as columns and geometry as MULTIPOLYGON.
#'
#' @name h3sdm_predictors
#'
#' @title Merge numeric, categorical, and landscape metric sf into one
#'
#' @description
#' This function allows the combination of numeric raster extractions
#' (from `h3sdm_extract_num`), categorical raster proportions
#' (from `h3sdm_extract_cat`), and landscape metrics
#' (from `h3sdm_calculate_it_metrics`) into a single `sf` object.
#' It ensures that the geometry remains a `MULTIPOLYGON` and performs
#' joins by the `h3_address` or `ID` column.
#'
#' @usage h3sdm_predictors(num_sf, cat_sf, it_sf)
#'
#' @param num_sf An `sf` object with numeric raster values.
#'               Typically the output of `h3sdm_extract_num()`.
#' @param cat_sf An `sf` object with categorical raster proportions.
#'               Typically the output of `h3sdm_extract_cat()`.
#' @param it_sf An `sf` object with landscape metrics.
#'              Typically the output of `h3sdm_calculate_it_metrics()`.
#'
#' @details
#' The function performs a left join of the categorical and numeric sf objects
#' and then joins the landscape metrics. The geometry column is preserved
#' and cast to `MULTIPOLYGON` to ensure compatibility with mapping and spatial analyses.
#'
#' @return An `sf` object of class MULTIPOLYGON with all variables from the
#'         three input sf objects combined as columns.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Suppose you already have a hex grid:
#' hex_grid_sf <- h3sdm_get_grid(aoi_sf, res = 7)
#'
#' # Extract numeric raster values
#' num_sf <- h3sdm_extract_num(bio, hex_grid_sf)
#'
#' # Extract categorical raster proportions
#' cat_sf <- h3sdm_extract_cat(lulc, hex_grid_sf)
#'
#' # Calculate landscape metrics
#' it_sf <- h3sdm_calculate_it_metrics(lulc, hex_grid_sf)
#'
#' # Combine everything
#' predictors_sf <- h3sdm_predictors(num_sf, cat_sf, it_sf)
#' }
#'
h3sdm_predictors <- function(num_sf, cat_sf, it_sf) {

  # Validate inputs
  if (!all(c("sf", "sfc") %in% class(num_sf))) stop("num_sf must be an sf object")
  if (!all(c("sf", "sfc") %in% class(cat_sf))) stop("cat_sf must be an sf object")
  if (!all(c("sf", "sfc") %in% class(it_sf))) stop("it_sf must be an sf object")

  # Determine join key
  join_key <- if ("h3_address" %in% names(num_sf)) "h3_address" else "ID"

  # Join categorical and numeric
  combined_sf <- dplyr::left_join(num_sf, cat_sf %>% dplyr::select(-geometry), by = join_key)

  # Join landscape metrics
  combined_sf <- dplyr::left_join(combined_sf, it_sf %>% dplyr::select(-geometry), by = join_key)

  # Ensure geometry is MULTIPOLYGON
  combined_sf$geometry <- sf::st_cast(combined_sf$geometry, "MULTIPOLYGON")

  return(combined_sf)
}
