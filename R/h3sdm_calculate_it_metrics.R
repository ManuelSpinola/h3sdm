#' @name h3sdm_calculate_it_metrics
#' @title Calculate 5 information theory landscape metrics for hexagonal grid
#' @description Calculates the landscape metrics `condent`, `ent`, `joinent`,
#'   `mutinf`, and `relmutinf` for a categorical raster over each hexagon.
#' @param landscape_raster A categorical `SpatRaster`.
#' @param aoi_sf An `sf` object defining the area of interest.
#' @param res H3 resolution (integer) for the hexagonal grid.
#' @param expand_factor Numeric factor to expand the AOI bbox before generating the grid (default 0.1).
#' @param clip_to_aoi Logical, if TRUE hexagons are clipped to the AOI (default FALSE).
#' @return An `sf` object (`MULTIPOLYGON`) with IT landscape metrics as columns.
#' @note This is a wrapper for `landscapemetrics::sample_lsm()`.
#' @references
#' Hesselbarth et al., 2019. *landscapemetrics: an open‐source R tool to calculate landscape metrics*, Ecography, 42:1648-1657.
#' Nowosad & Stepinski, 2019. *Information theory as a consistent framework for quantification and classification of landscape patterns*, https://doi.org/10.1007/s10980-019-00830-x
#' @examples
#' \donttest{
#' it_metrics_sf <- h3sdm_calculate_it_metrics(landcover_raster, aoi_sf, res = 7)
#' }
#' @export
h3sdm_calculate_it_metrics <- function(landscape_raster, aoi_sf, res = 7,
                                       expand_factor = 0.1, clip_to_aoi = FALSE) {

  hex_grid <- h3sdm_get_grid(aoi_sf, res = res,
                             expand_factor = expand_factor,
                             clip_to_aoi = clip_to_aoi)

  it_metrics <- landscapemetrics::sample_lsm(
    landscape_raster,
    hex_grid,
    level = "landscape",
    type = "complexity metric"
  )

  it_metrics_w <- it_metrics |>
    dplyr::distinct() |>
    tidyr::pivot_wider(id_cols = .data$plot_id,
                       names_from = .data$metric,
                       values_from = .data$value)

  result_sf <- cbind(hex_grid, it_metrics_w)
  result_sf <- sf::st_make_valid(result_sf)
  result_sf <- sf::st_cast(result_sf, "MULTIPOLYGON")

  return(result_sf)
}
