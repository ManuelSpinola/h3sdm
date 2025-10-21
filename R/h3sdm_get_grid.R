#' @name h3sdm_get_grid
#' @title Generate H3 hexagonal grid over an AOI
#' @description Generates an H3 hexagonal grid as an sf object over a given area of interest (AOI).
#' Optionally clips the hexagons to the AOI for precise landscape metrics.
#' @param aoi_sf An `sf` object defining the AOI.
#' @param res H3 resolution (integer, 1–16).
#' @param expand_factor Factor to expand the AOI bbox before generating the grid (default 0.1).
#' @param clip_to_aoi Logical; if TRUE, hexagons are clipped to the AOI (slower, precise for landscape metrics). Default FALSE.
#' @return An `sf` object with MULTIPOLYGON geometries representing the hexagonal grid.
#' @examples
#' \donttest{
#' hex_grid_sf <- h3sdm_get_grid(aoi_sf, res = 7, clip_to_aoi = TRUE)
#' }
#' @export

h3sdm_get_grid <- function(aoi_sf, res = 7, expand_factor = 0.1, clip_to_aoi = FALSE) {

  if (!inherits(aoi_sf, "sf")) stop("aoi_sf must be an sf object.")

  # Expand bounding box
  bbox <- sf::st_bbox(aoi_sf)
  x_range <- bbox["xmax"] - bbox["xmin"]
  y_range <- bbox["ymax"] - bbox["ymin"]
  bbox_exp <- bbox + c(-expand_factor * x_range, -expand_factor * y_range,
                       expand_factor * x_range, expand_factor * y_range)
  aoi_exp <- sf::st_as_sfc(bbox_exp) |> sf::st_as_sf()

  # Generate H3 addresses
  h3_addresses <- h3jsr::polygon_to_cells(aoi_exp, res = res)

  if (clip_to_aoi) {
    # Convert to polygons and clip to AOI
    hex_sf <- h3jsr::cell_to_polygon(h3_addresses, simple = FALSE)
    hex_sf <- sf::st_intersection(hex_sf, aoi_sf)
    # Union fragments per hex to ensure MULTIPOLYGON
    hex_sf <- sf::st_union(hex_sf, by_feature = TRUE)
    hex_sf <- sf::st_make_valid(hex_sf)
  } else {
    hex_sf <- h3jsr::cell_to_polygon(h3_addresses, simple = FALSE)
  }

  hex_sf$h3_address <- h3_addresses
  hex_sf <- sf::st_cast(hex_sf, "MULTIPOLYGON")

  return(hex_sf)
}
