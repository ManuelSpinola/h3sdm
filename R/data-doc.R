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


#' Current bioclimatic raster file
#'
#' A GeoTIFF with current bioclimatic variables.
#'
#' @details This file is stored in `inst/extdata/` and can be accessed with:
#' \code{system.file("extdata", "bioclim_current.tif", package = "h3sdm")}
#'
#' @format GeoTIFF file, readable with \code{terra::rast()}.
"bioclim_current"


#' Future bioclimatic raster file
#'
#' A GeoTIFF with projected bioclimatic variables for Costa Rica.
#'
#' @details This dataset corresponds to the climate projection:
#' - Model: INM-CM4-8
#' - Scenario: SSP1-2.6
#' - Period: 2021–2040
#'
#' The file is stored in `inst/extdata/` as
#' \code{"cr_inm_cm4_8_ssp126_2021_2040.tif"} and can be accessed with:
#' \code{system.file("extdata", "cr_inm_cm4_8_ssp126_2021_2040.tif", package = "h3sdm")}
#'
#' @format GeoTIFF file, readable with \code{terra::rast()}.
"bioclim_future"
