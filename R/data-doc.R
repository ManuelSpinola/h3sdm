# ---------------------------------------------
# Costa Rica Outlines
# ---------------------------------------------

#' @name cr_outline_c
#' @title Costa Rica Continental Outline
#'
#' @description
#' An \code{sf} polygon containing the continental outline of Costa Rica,
#' derived from GADM 4.1. The Isla del Coco and all other minor oceanic
#' islands have been removed, retaining only the largest polygon
#' (the continental landmass).
#'
#' For the full outline including all islands, see \code{\link{cr_outline}}.
#'
#' @format An \code{sf} object with 1 feature and 1 column:
#' \describe{
#'   \item{geometry}{POLYGON in WGS 84 (EPSG:4326) with 30,261 vertices,
#'   representing the continental outline of Costa Rica.}
#' }
#'
#' @details
#' ## Island removal
#' Costa Rica includes the Isla del Coco (~550 km offshore in the Pacific),
#' which is excluded here. The continental polygon is obtained by casting the
#' GADM multipolygon to individual polygons and retaining the one with the
#' largest area. For analyses requiring all national territory use
#' \code{\link{cr_outline}}.
#'
#' ## Reproducibility
#' Generated with \code{data-raw/cr_outline.R}. To regenerate:
#' \preformatted{source("data-raw/cr_outline.R")}
#'
#' @source
#' Global Administrative Areas (GADM) version 4.1.
#' Downloaded via \code{geodata::gadm("CRI", level = 0)}.
#' \url{https://gadm.org}
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{cr_outline}} -- full outline including all islands.
#' }
#'
#' @examples
#' data(cr_outline_c)
#' plot(sf::st_geometry(cr_outline_c), main = "Costa Rica (continental)")
"cr_outline_c"


#' @name cr_outline
#' @title Costa Rica Full Outline (Continental + Islands)
#'
#' @description
#' An \code{sf} multipolygon containing the full outline of Costa Rica,
#' derived from GADM 4.1. Includes the continental landmass, the Isla del
#' Coco (~550 km offshore in the Pacific Ocean), and all other minor
#' oceanic islands.
#'
#' For the continental outline only (without islands), see
#' \code{\link{cr_outline_c}}.
#'
#' @format An \code{sf} object with 1 feature and 1 column:
#' \describe{
#'   \item{geometry}{MULTIPOLYGON in WGS 84 (EPSG:4326) representing the
#'   full national territory of Costa Rica including all islands.}
#' }
#'
#' @details
#' ## When to use this vs cr_outline_c
#' Use \code{cr_outline} when your analysis requires the full national
#' territory. Use \code{\link{cr_outline_c}} for mainland ecological
#' analyses where oceanic islands would distort results (species distribution
#' models, landscape metrics, climate extraction).
#'
#' ## Reproducibility
#' Generated with \code{data-raw/cr_outline.R}. To regenerate:
#' \preformatted{source("data-raw/cr_outline.R")}
#'
#' @source
#' Global Administrative Areas (GADM) version 4.1.
#' Downloaded via \code{geodata::gadm("CRI", level = 0)}.
#' \url{https://gadm.org}
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{cr_outline_c}} -- continental outline only (no islands).
#' }
#'
#' @examples
#' data(cr_outline)
#' plot(sf::st_geometry(cr_outline), main = "Costa Rica (full territory)")
#'
#' \dontrun{
#' # Compare continental vs full
#' par(mfrow = c(1, 2))
#' plot(sf::st_geometry(cr_outline_c), main = "Continental")
#' plot(sf::st_geometry(cr_outline),   main = "Full territory")
#' }
"cr_outline"


# ---------------------------------------------
# Species records
# ---------------------------------------------

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


# ---------------------------------------------
# Bioclimatic rasters
# ---------------------------------------------

#' Current bioclimatic raster
#'
#' A GeoTIFF with current bioclimatic variables for Costa Rica.
#'
#' @details This file is stored in \code{inst/extdata/} and can be accessed with:
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
#' \itemize{
#'   \item Model: INM-CM4-8
#'   \item Scenario: SSP1-2.6
#'   \item Period: 2021-2040
#' }
#'
#' The file is stored in \code{inst/extdata/} and can be accessed with:
#' \code{terra::rast(system.file("extdata", "bioclim_future.tif", package = "h3sdm"))}
#'
#' @format GeoTIFF file, readable with \code{terra::rast()}.
#' @examples
#' library(terra)
#' bio <- terra::rast(system.file("extdata", "bioclim_future.tif", package = "h3sdm"))
#' @name bioclim_future
NULL
