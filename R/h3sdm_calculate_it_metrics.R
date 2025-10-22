#' @name h3sdm_calculate_it_metrics
#'
#' @title Calculate Information Theory Landscape Metrics for Hexagonal Grid
#'
#' @description
#' Calculates 5 Information Theory (IT)-based landscape metrics (\code{condent},
#' \code{ent}, \code{joinent}, \code{mutinf}, \code{relmutinf}) by generating an H3
#' hexagonal grid over a categorical raster and using \code{landscapemetrics::sample_lsm()}.
#'
#' @param landscape_raster A categorical \code{SpatRaster}.
#' @param aoi_sf An \code{sf} object defining the Area of Interest (AOI).
#' @param res H3 resolution (integer, e.g., 7 or 8). Determines the size of the hexagons.
#' @param expand_factor Factor to expand the AOI bounding box before H3 grid generation.
#'   (Numeric, e.g., 1.1).
#' @param clip_to_aoi Logical. If \code{TRUE}, the generated hexagons are clipped
#'   to the exact boundary of the \code{aoi_sf}.
#'
#' @return An \code{sf} object containing the generated hexagonal grid with new columns
#'   appended for each calculated landscape metric, plus the \code{h3_address} column.
#'
#' @details
#' This function is a comprehensive wrapper that handles the entire workflow:
#' 1. Generating the H3 grid based on \code{aoi_sf} and \code{res}.
#' 2. Calculating the IT metrics (which are part of the "complexity metric" type in
#'    \code{landscapemetrics}) for each hexagon.
#' 3. Pivoting the results to a wide format for easy use.
#'
#' @references
#' Hesselbarth et al., 2019. landscapemetrics: an open-source R tool to calculate
#' landscape metrics. Ecography 42: 1648–1657.
#'
#' Nowosad & Stepinski, 2019. Information theory as a consistent framework for
#' landscape patterns. \url{https://doi.org/10.1007/s10980-019-00830-x}
#'
#' @examples
#' \donttest{
#' # Assuming 'worldcover' is a SpatRaster and 'aoi_sf' is an sf polygon
#' # result_sf <- h3sdm_calculate_it_metrics(worldcover, aoi_sf, res = 7)
#' # head(result_sf)
#' }
#' @export

h3sdm_calculate_it_metrics <- function(landscape_raster, sf_grid) {

  # --- 1️⃣ Check inputs and Setup ---
  if (!inherits(landscape_raster, "SpatRaster")) {
    stop("The landscape raster must be a 'SpatRaster' object.")
  }
  if (!inherits(sf_grid, "sf")) {
    stop("The grid must be an 'sf' object.")
  }

  # Add a temporary ID to the SF grid based on its row index (which landscapemetrics uses as plot_id)
  sf_grid <- sf_grid |>
    dplyr::mutate(plot_id = seq_len(dplyr::n()))

  # --- 2️⃣ Calculate landscape metrics ---
  # landscapemetrics returns the row index (from sf_grid) as 'plot_id'.
  it_metrics <- landscapemetrics::sample_lsm(
    landscape_raster,
    sf_grid,
    level = "landscape",
    type = "complexity metric"
  )

  # --- 3️⃣ Pivot to wide format ---
  it_metrics_w <- it_metrics |>
    dplyr::distinct() |>
    tidyr::pivot_wider(
      # The ID column is already named 'plot_id' from landscapemetrics
      id_cols = .data$plot_id,
      names_from = .data$metric,
      values_from = .data$value,
      names_prefix = "lsm_"
    )

  # --- 4️⃣ Join results with the grid ---
  # Perform a simple left_join using the 'plot_id' column present in both tables.
  it_metrics_sf <- sf_grid |>
    dplyr::left_join(it_metrics_w, by = "plot_id") |>
    dplyr::select(-.data$plot_id) # Clean up the temporary ID column

  # Note on the Warnings: The warnings about "percentage_inside" are normal
  # when grid cells (buffers) are cut by the AOI boundary or raster boundary.

  return(it_metrics_sf)
}
