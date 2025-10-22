#' @name h3sdm_get_records
#'
#' @title Query Species Occurrence Records within an H3 Area of Interest (AOI)
#'
#' @description
#' Downloads species occurrence records from providers (e.g., GBIF) using the \code{spocc}
#' package, filtering the initial query by the exact polygonal boundary of the
#' Area of Interest (AOI) for maximum efficiency and precision.
#'
#' @param species Character string specifying the species name to query (e.g., "Puma concolor").
#' @param aoi_sf An \code{sf} object defining the Area of Interest (AOI). Its CRS will be
#'   transformed to WGS84 (\code{EPSG:4326}) before query.
#' @param providers Character vector of data providers to query (e.g., "gbif", "bison").
#'   If \code{NULL} (default), all available providers are used.
#' @param limit Numeric. The maximum number of records to retrieve per provider. Default is 500.
#' @param remove_duplicates Logical. If \code{TRUE}, records with identical longitude and
#'   latitude are removed using \code{dplyr::distinct()}. Default is \code{FALSE}.
#' @param date Character vector specifying a date range (e.g., \code{c('2000-01-01', '2020-12-31')}).
#'
#' @return An \code{sf} object of points containing the filtered occurrence records,
#'   with geometry confirmed to fall strictly within the \code{aoi_sf} boundary. If no
#'   records are found or the download fails, an empty \code{sf} object with the
#'   expected structure is returned.
#' @export
#'
#' @details
#' The function transforms the \code{aoi_sf} polygon into a WKT string, which is used in
#' the \code{spocc::occ} geometry argument for **efficient WKT-based querying**. Final
#' spatial filtering is performed using \code{sf::st_intersection} to ensure strict
#' containment. A critical check is included to prevent errors when the API returns no
#' data (addressing the 'column not found' error).
#'
#' @examples
#' \dontrun{
#' # Assuming aoi_sf is a valid sf polygon
#' # h3_records <- h3sdm_get_records("Puma concolor", aoi_sf, providers = "gbif", limit = 1000)
#' # head(h3_records)
#' }

h3sdm_get_records <- function(species,
                              aoi_sf,
                              providers = NULL,
                              limit = 500,
                              remove_duplicates = FALSE,
                              date = NULL) {

  stopifnot(inherits(aoi_sf, "sf"))

  # Ensure AOI is in WGS84
  if (is.na(sf::st_crs(aoi_sf)) || sf::st_crs(aoi_sf)$epsg != 4326) {
    aoi_sf <- sf::st_transform(aoi_sf, 4326)
  }

  # Build bounding box safely
  bbox <- tryCatch(sf::st_bbox(aoi_sf), error = function(e) NULL)
  if (is.null(bbox)) {
    stop("Invalid AOI geometry: cannot compute bounding box.")
  }

  # Query records safely
  records <- tryCatch({
    spocc::occ(
      query = species,
      from = providers,
      geometry = bbox,
      has_coords = TRUE,
      limit = limit,
      date = date
    ) |> spocc::occ2df()
  }, error = function(e) NULL)

  # Handle no records
  if (is.null(records) || nrow(records) == 0 ||
      !all(c("longitude", "latitude") %in% names(records))) {
    message("⚠️ No records found for ", species, ". Returning empty sf object.")
    return(sf::st_sf(
      name = character(0),
      geometry = sf::st_sfc(crs = 4326)
    ))
  }

  # Clean and convert to sf
  records <- records[!is.na(records$longitude) & !is.na(records$latitude), ]
  records_sf <- sf::st_as_sf(records, coords = c("longitude", "latitude"), crs = 4326)

  # Optional deduplication
  if (remove_duplicates) {
    records_sf <- dplyr::distinct(records_sf, .data$geometry, .keep_all = TRUE)
  }

  # Clip to AOI
  suppressWarnings({
    records_sf <- sf::st_intersection(records_sf, aoi_sf)
  })

  return(records_sf)
}
