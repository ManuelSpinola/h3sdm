#' @name h3sdm_get_records
#' @title Download species occurrence records
#' @description Downloads species occurrence records from GBIF, iNaturalist, or other providers
#'   for a given area of interest (AOI) as an `sf` object with geometries.
#' @usage h3sdm_get_records(species, aoi_sf, providers = c("gbif","inat"), limit = 500,
#'                          remove_duplicates = FALSE, date = NULL)
#' @param species Character string with the species name (e.g., "Agalychnis callidryas").
#' @param aoi_sf An `sf` object defining the area of interest.
#' @param providers Character vector of data providers (default c("gbif","inat")).
#' @param limit Maximum number of records to download per provider (default 500).
#' @param remove_duplicates Logical, whether to remove duplicate coordinates (default FALSE).
#' @param date Character string in format "YYYY-MM-DD,YYYY-MM-DD" to filter by observation date (optional).
#' @return An `sf` object with species occurrence points inside the AOI.
#' @details The function downloads occurrence records from online providers using the `spocc` package,
#'   transforms them to the CRS of the AOI, and optionally removes duplicates.
#' @examples
#' \donttest{
#' library(sf)
#' nc <- st_read(system.file("shape/nc.shp", package="sf"))
#' records <- h3sdm_get_records("Agalychnis callidryas", nc, limit = 100)
#' }
#' @export

h3sdm_get_records <- function(species,
                              aoi_sf,
                              providers = c("gbif","inat"),
                              limit = 500,
                              remove_duplicates = FALSE,
                              date = NULL) {

  stopifnot(inherits(aoi_sf, "sf"))
  if (!is.character(species)) stop("species must be a character vector")

  # Transform AOI to WGS84
  if (sf::st_crs(aoi_sf)$epsg != 4326) {
    aoi_sf <- sf::st_transform(aoi_sf, 4326)
  }

  # Download records using spocc
  records <- spocc::occ(query = species,
                        from = providers,
                        geometry = sf::st_bbox(aoi_sf),
                        has_coords = TRUE,
                        limit = limit,
                        date = date) |>
    spocc::occ2df()

  # ❗ FIX 1: Manejo inmediato de datos sin coordenadas o vacíos
  # Asegurar que 'records' tiene las columnas necesarias antes de manipularlo

  columnas_necesarias <- c("longitude", "latitude")

  # Si records está vacío o le faltan columnas clave, devolver un objeto vacío
  if (nrow(records) == 0 || !all(columnas_necesarias %in% names(records))) {
    warning("No se pudieron obtener coordenadas o registros válidos para la especie: ", species)
    # Devolver un sf vacío y válido para que h3sdm_pa no falle.
    return(sf::st_sf(geometry = sf::st_sfc(), crs = sf::st_crs(aoi_sf)))
  }

  # Keep only records with coordinates
  records <- records[!is.na(records$longitude) & !is.na(records$latitude), ]

  if (nrow(records) == 0) {
    warning("No records found for species: ", species)
    return(sf::st_sf(geometry = sf::st_sfc(), crs = sf::st_crs(aoi_sf)))
  }

  # Convert to sf
  records_sf <- sf::st_as_sf(records, coords = c("longitude","latitude"), crs = 4326)

  # Optionally remove duplicates
  if (remove_duplicates) {
    records_sf <- dplyr::distinct(records_sf, .data$geometry, .keep_all = TRUE)
  }

  # Intersect with AOI
  records_sf <- sf::st_intersection(records_sf, aoi_sf)

  # Devolver solo la geometría (corrección de error CPL anterior)
  records_sf <- records_sf[, c("geometry")]

  return(records_sf)
}
