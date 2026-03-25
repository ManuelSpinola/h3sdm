# ---------------------------------------------
# Costa Rica Continental Outline
# ---------------------------------------------
#' Costa Rica Continental Outline
#'
#' A simplified outline of Costa Rica as an `sf` object.
#'
#' @format An `sf` object containing polygon geometry of Costa Rica.
#' @source Adapted from publicly available geographic data.
#' @examples
#' library(sf)
#' plot(cr_outline_c)
"cr_outline_c"

#' Presence/pseudo-absence records for Silverstoneia flotator
#'
#' A dataset containing presence and pseudo-absence records for the species
#' \emph{Silverstoneia flotator} in Costa Rica, generated using H3 hexagonal
#' grids at resolution 7.
#'
#' @format An \code{sf} object with columns:
#' \describe{
#'   \item{h3_address}{H3 index of the hexagon}
#'   \item{presence}{factor with levels "0" (pseudo-absence) and "1" (presence)}
#'   \item{geometry}{MULTIPOLYGON of each hexagon}
#' }
#' @source Generated using \code{h3sdm_pa()} with occurrence data from GBIF
#' (\url{https://www.gbif.org}).
#' @examples
#' data(records)
#' head(records)
#' table(records$presence)
"records"

#' Current bioclimatic raster
#'
#' A GeoTIFF with current bioclimatic variables for Costa Rica.
#'
#' @details This file is stored in `inst/extdata/` and can be accessed with:
#' \code{terra::rast(system.file("extdata", "bioclim_current.tif", package = "h3sdm"))}
#'
#' @format GeoTIFF file, readable with \code{terra::rast()}.
#' @examples
#' library(terra)
#' bio <- terra::rast(system.file("extdata", "bioclim_current.tif", package = "h3sdm"))
#' @name bioclim_current
NULL


#' Future bioclimatic raster
#'
#' A GeoTIFF with projected bioclimatic variables for Costa Rica.
#'
#' @details This dataset corresponds to the climate projection:
#' - Model: INM-CM4-8
#' - Scenario: SSP1-2.6
#' - Period: 2021–2040
#'
#' The file is stored in `inst/extdata/` and can be accessed with:
#' \code{terra::rast(system.file("extdata", "bioclim_future.tif", package = "h3sdm"))}
#'
#' @format GeoTIFF file, readable with \code{terra::rast()}.
#' @examples
#' library(terra)
#' bio <- terra::rast(system.file("extdata", "bioclim_future.tif", package = "h3sdm"))
#' @name bioclim_future
NULL
