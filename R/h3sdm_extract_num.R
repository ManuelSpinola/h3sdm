#' @name h3sdm_extract_num
#' @title Extract numeric raster values for a hexagonal grid
#' @description Extracts area-weighted mean values of numeric raster layers
#'   for each polygon in a hexagonal grid.
#' @param spat_raster_multi A `SpatRaster` object containing one or more numeric layers.
#' @param aoi_sf An `sf` object defining the area of interest.
#' @param res H3 resolution (integer) for the hexagonal grid.
#' @param expand_factor Numeric factor to expand the AOI bbox before generating the grid (default 0.1).
#' @param clip_to_aoi Logical, if TRUE hexagons are clipped to the AOI (default FALSE).
#' @return An `sf` object (`MULTIPOLYGON`) with raster values added as columns.
#' @examples
#' \donttest{
#' hex_grid <- h3sdm_extract_num(bio_raster, aoi_sf, res = 7)
#' }
#' @export

h3sdm_extract_num <- function(spat_raster_multi, aoi_sf, res = 7,
                              expand_factor = 0.1, clip_to_aoi = FALSE) {

  hex_grid <- h3sdm_get_grid(aoi_sf, res = res,
                             expand_factor = expand_factor,
                             clip_to_aoi = clip_to_aoi)

  extracted_df <- exactextractr::exact_extract(
    x = spat_raster_multi,
    y = hex_grid,
    fun = "weighted_mean",
    weights = "area",
    force_df = TRUE
  )

  colnames(extracted_df) <- names(spat_raster_multi)[seq_len(ncol(extracted_df))]

  result_sf <- dplyr::bind_cols(hex_grid, extracted_df)
  result_sf <- sf::st_make_valid(result_sf)
  result_sf <- sf::st_cast(result_sf, "MULTIPOLYGON")

  return(result_sf)
}
