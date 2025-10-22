#' @name h3sdm_calculate_it_metrics
#' @title Calculate Information Theory Landscape Metrics for Hexagonal Grid
#' @description
#' Calculates 5 Information Theory (IT)-based landscape metrics (\code{condent},
#' \code{ent}, \code{joinent}, \code{mutinf}, \code{relmutinf}) for each hexagon
#' in a given H3 hexagonal grid.
#'
#' @param landscape_raster A categorical SpatRaster containing land-cover data.
#' @param sf_grid An \code{sf} object containing the hexagonal grid with species or land-cover data.
#'
#' @return An \code{sf} object containing the input hex grid with new columns for each calculated metric.
#' @details
#' This function computes landscape metrics using the \code{landscapemetrics::sample_lsm()} workflow.
#' The results are pivoted to a wide format for easy use.
#'
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
#' # Assuming 'sf_grid' is an sf object with hexagons
#' # result_sf <- h3sdm_calculate_it_metrics(sf_grid)
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
