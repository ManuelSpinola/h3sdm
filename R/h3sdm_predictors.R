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

  # --- 1️⃣ Robust Input Validation ---
  # Check if the objects INHERIT the 'sf' class. This is the standard, reliable way.
  if (!inherits(num_sf, "sf")) stop("num_sf must be an sf object (inherits 'sf').")
  if (!inherits(cat_sf, "sf")) stop("cat_sf must be an sf object (inherits 'sf').")
  if (!inherits(it_sf, "sf")) stop("it_sf must be an sf object (inherits 'sf').")

  # --- 2️⃣ Determine Join Key ---
  # Use 'h3_address' if it exists, otherwise fall back to 'ID'.
  join_key <- if ("h3_address" %in% names(num_sf)) "h3_address" else "ID"

  # Ensure the join key is present in ALL data frames before proceeding
  required_keys <- c(join_key)
  if (!all(required_keys %in% names(num_sf)) ||
      !all(required_keys %in% names(cat_sf)) ||
      !all(required_keys %in% names(it_sf))) {
    stop(paste("The chosen join key ('", join_key, "') is not present in all input data frames.", sep=""))
  }

  # --- 3️⃣ Join Data ---

  # Join categorical and numeric
  # We select all columns *except* geometry from the right-hand side (cat_sf) to avoid conflicts.
  combined_sf <- dplyr::left_join(num_sf,
                                  cat_sf %>% sf::st_drop_geometry(),
                                  by = join_key)

  # Join landscape metrics
  # Drop geometry from it_sf again, ensuring we keep num_sf's geometry column
  combined_sf <- dplyr::left_join(combined_sf,
                                  it_sf %>% sf::st_drop_geometry(),
                                  by = join_key)

  # --- 4️⃣ Final Cleanup ---

  # Ensure geometry is MULTIPOLYGON (safer for downstream processes)
  combined_sf <- sf::st_cast(combined_sf, "MULTIPOLYGON")

  return(combined_sf)
}
