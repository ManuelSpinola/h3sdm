#' @name h3sdm_get_records
#'
#' @title Query Species Occurrence Records within an H3 Area of Interest (AOI)
#'
#' @description
#' Downloads species occurrence records from providers (e.g., GBIF, iNaturalist,
#' BiodataCR) and filters them by the exact polygonal boundary of the Area of
#' Interest (AOI). Providers supported by \code{spocc} (e.g., \code{"gbif"},
#' \code{"inat"}) are queried via \code{spocc::occ()}. \code{"biodatacr"} is
#' queried via \code{rbiodatacr::bdcr_occurrences()} and its output is
#' standardized to the same \code{sf} format.
#'
#' @param species Character string specifying the species name to query
#'   (e.g., \code{"Puma concolor"}).
#' @param aoi_sf An \code{sf} object defining the Area of Interest (AOI).
#'   Its CRS will be transformed to WGS84 (\code{EPSG:4326}) before query.
#' @param providers Character vector of data providers to query.
#'   Accepted values: any provider supported by \code{spocc} (e.g.,
#'   \code{"gbif"}, \code{"inat"}) plus \code{"biodatacr"} for BiodataCR
#'   (Costa Rica). If \code{NULL} (default), all \code{spocc} providers are used.
#' @param limit Numeric. Maximum number of records to retrieve per provider.
#'   Default is 500.
#' @param remove_duplicates Logical. If \code{TRUE}, records with identical
#'   coordinates are removed. Default is \code{FALSE}.
#' @param date Character vector specifying a date range
#'   (e.g., \code{c('2000-01-01', '2020-12-31')}). Applied to \code{spocc}
#'   providers only.
#'
#' @return An \code{sf} object of points with the filtered occurrence records
#'   whose geometry falls strictly within the \code{aoi_sf} boundary. If no
#'   records are found, an empty \code{sf} object with the expected structure
#'   is returned.
#'
#' @details
#' When \code{"biodatacr"} is included in \code{providers}, the function calls
#' \code{rbiodatacr::bdcr_occurrences()} and standardizes its output
#' (\code{decimalLatitude}/\code{decimalLongitude}) to the same \code{sf}
#' geometry format used by the \code{spocc} providers. Records from all
#' providers are then combined and clipped to the AOI.
#'
#' @examples
#' \donttest{
#'   library(sf)
#'
#'   aoi_sf <- sf::st_as_sf(
#'     data.frame(
#'       lon = c(-84.5, -83.5, -83.5, -84.5, -84.5),
#'       lat = c(9.5, 9.5, 10.5, 10.5, 9.5)
#'     ) |>
#'       {\(d) sf::st_sfc(sf::st_polygon(list(as.matrix(d))), crs = 4326)}(),
#'     data.frame(id = 1)
#'   )
#'
#'   # GBIF only
#'   records <- h3sdm_get_records(
#'     species   = "Puma concolor",
#'     aoi_sf    = aoi_sf,
#'     providers = "gbif",
#'     limit     = 100
#'   )
#'
#'   # GBIF + BiodataCR (Costa Rica)
#'   records_cr <- h3sdm_get_records(
#'     species   = "Agalychnis callidryas",
#'     aoi_sf    = aoi_sf,
#'     providers = c("gbif", "biodatacr"),
#'     limit     = 200
#'   )
#' }
#' @export

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

  # Separate biodatacr from spocc providers
  use_biodatacr  <- "biodatacr" %in% providers
  spocc_providers <- if (is.null(providers))
    NULL
  else
    providers[providers != "biodatacr"]
  if (length(spocc_providers) == 0) spocc_providers <- NULL

  all_sf <- list()

  # ── 1. spocc providers (gbif, inat, etc.) ─────────────────
  if (is.null(spocc_providers) || length(spocc_providers) > 0) {
    bbox <- tryCatch(sf::st_bbox(aoi_sf), error = function(e) NULL)
    if (!is.null(bbox)) {
      records <- tryCatch({
        spocc::occ(
          query      = species,
          from       = spocc_providers,
          geometry   = bbox,
          has_coords = TRUE,
          limit      = limit,
          date       = date
        ) |> spocc::occ2df()
      }, error = function(e) NULL)

      if (!is.null(records) && nrow(records) > 0 &&
          all(c("longitude", "latitude") %in% names(records))) {
        records <- records[!is.na(records$longitude) &
                             !is.na(records$latitude), ]
        if (nrow(records) > 0) {
          sf_spocc <- sf::st_as_sf(records,
                                   coords = c("longitude", "latitude"),
                                   crs = 4326)
          sf_spocc$provider <- records$prov %||% "spocc"
          all_sf[["spocc"]] <- sf_spocc
        }
      }
    }
  }

  # ── 2. BiodataCR ──────────────────────────────────────────
  if (use_biodatacr) {
    if (!requireNamespace("rbiodatacr", quietly = TRUE)) {
      warning("Package 'rbiodatacr' is required for provider 'biodatacr'. ",
              "Install it with: install.packages('rbiodatacr')")
    } else {
      bdcr_raw <- tryCatch(
        rbiodatacr::bdcr_occurrences(taxon = species, rows = limit),
        error = function(e) NULL
      )

      if (!is.null(bdcr_raw) && nrow(bdcr_raw) > 0 &&
          all(c("decimalLatitude", "decimalLongitude") %in% names(bdcr_raw))) {

        bdcr_clean <- bdcr_raw[
          !is.na(bdcr_raw$decimalLatitude) &
            !is.na(bdcr_raw$decimalLongitude), ]

        # Estandarizar nombres al formato spocc/occ2df
        bdcr_clean$name      <- bdcr_clean$scientificName
        bdcr_clean$longitude <- bdcr_clean$decimalLongitude
        bdcr_clean$latitude  <- bdcr_clean$decimalLatitude
        bdcr_clean$provider  <- "biodatacr"

        if (nrow(bdcr_clean) > 0) {
          sf_bdcr <- sf::st_as_sf(bdcr_clean,
                                  coords = c("decimalLongitude", "decimalLatitude"),
                                  crs = 4326)
          all_sf[["biodatacr"]] <- sf_bdcr
        }
      } else {
        message("No BiodataCR records found for ", species, ".")
      }
    }
  }

  # ── 3. Combinar todos los providers ───────────────────────
  if (length(all_sf) == 0) {
    message("WARNING: No records found for ", species,
            ". Returning empty sf object.")
    return(sf::st_sf(
      name     = character(0),
      provider = character(0),
      geometry = sf::st_sfc(crs = 4326)
    ))
  }

  # Unir con columnas comunes para evitar errores de rbind
  records_sf <- do.call(
    dplyr::bind_rows,
    lapply(all_sf, function(x) {
      x[, intersect(names(x),
                    c("name", "provider", "geometry"))]
    })
  )
  records_sf <- sf::st_as_sf(records_sf, crs = 4326)

  # ── 4. Deduplicar ─────────────────────────────────────────
  if (remove_duplicates) {
    records_sf <- dplyr::distinct(records_sf, .data$geometry, .keep_all = TRUE)
  }

  # ── 5. Clip al AOI ────────────────────────────────────────
  suppressWarnings({
    records_sf <- sf::st_intersection(records_sf, aoi_sf)
  })

  return(records_sf)
}

# Operador %||% interno (null-coalescing) si no está disponible
`%||%` <- function(a, b) if (!is.null(a)) a else b
