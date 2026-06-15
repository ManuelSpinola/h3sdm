#' Assign pre-downloaded species presence records to H3 hexagons
#'
#' @description
#' Takes an `sf` object of species occurrence records already downloaded
#' (e.g. from [h3sdm_get_records()]) and assigns them to H3 hexagons,
#' returning only hexagons with at least one presence record.
#'
#' This function extracts the hexagon-assignment logic from
#' [h3sdm_get_records_by_hexagon()] without downloading records internally,
#' making it suitable for workflows where records have already been retrieved.
#'
#' @param records_sf An `sf` object with presence records in any CRS.
#'   Typically the output of [h3sdm_get_records()].
#' @param aoi_sf An `sf` object defining the area of interest.
#' @param res Integer. H3 resolution (0–15). Default is `6`.
#' @param expand_factor Numeric. Expansion factor for the H3 grid beyond the
#'   AOI bounding box. Default is `0.1`.
#'
#' @return An `sf` object with one row per presence hexagon, containing:
#'   \describe{
#'     \item{h3_address}{H3 index of the hexagon.}
#'     \item{n}{Number of records assigned to the hexagon.}
#'     \item{geometry}{MULTIPOLYGON geometry of the hexagon.}
#'   }
#'
#' @details
#' This function is designed to be used in combination with
#' [h3sdm_filter_outliers()] and [h3sdm_pa()] for a balanced
#' presence/pseudo-absence workflow:
#'
#' \preformatted{
#' # 1. Download records
#' records_sf <- h3sdm_get_records("Species name", aoi_sf, providers = c("gbif", "biodatacr"))
#'
#' # 2. Assign to hexagons
#' pres_sf <- h3sdm_pres_from_sf(records_sf, aoi_sf, res = 7)
#'
#' # 3. Filter environmental outliers (only presences)
#' filtro <- h3sdm_filter_outliers(pres_sf_env, vars_cov)
#' pres_clean <- filtro$pa_clean
#'
#' # 4. Generate balanced pseudo-absences (1:1)
#' pa <- h3sdm_pa(pres_clean, predictors_sf, n_pseudoabs = nrow(pres_clean))
#' }
#'
#' @seealso [h3sdm_get_records()], [h3sdm_pa()], [h3sdm_filter_outliers()]
#'
#' @examples
#' \dontrun{
#' data(cr_outline_c, package = "h3sdm")
#'
#' records_sf <- h3sdm_get_records(
#'   species   = "Panthera onca",
#'   aoi_sf    = cr_outline_c,
#'   providers = c("gbif", "biodatacr"),
#'   limit     = 500
#' )
#'
#' pres_sf <- h3sdm_pres_from_sf(records_sf, cr_outline_c, res = 7)
#' nrow(pres_sf)
#' }
#'
#' @export
h3sdm_pres_from_sf <- function(records_sf,
                                aoi_sf,
                                res           = 6,
                                expand_factor = 0.1) {

  # Input validation
  if (!inherits(records_sf, "sf")) {
    cli::cli_abort("{.arg records_sf} must be an {.cls sf} object.")
  }
  if (!inherits(aoi_sf, "sf")) {
    cli::cli_abort("{.arg aoi_sf} must be an {.cls sf} object.")
  }
  if (nrow(records_sf) == 0) {
    cli::cli_abort("{.arg records_sf} contains no records.")
  }

  # Ensure matching CRS
  if (sf::st_crs(records_sf) != sf::st_crs(aoi_sf)) {
    records_sf <- sf::st_transform(records_sf, sf::st_crs(aoi_sf))
  }

  # Estandarizar nombre de columna de geometría
  sf::st_geometry(records_sf) <- "geometry"
  sf::st_geometry(aoi_sf)     <- "geometry"

  # Generate H3 grid over AOI
  hex_grid <- suppressWarnings(
    h3sdm_get_grid(aoi_sf, res = res, expand_factor = expand_factor)
  )
  hex_grid <- hex_grid[, c("h3_address", "geometry")]
  hex_grid <- suppressWarnings(sf::st_cast(hex_grid, "MULTIPOLYGON"))

  # Spatial join: assign records to hexagons
  joined <- suppressWarnings(
    sf::st_join(records_sf[, "geometry"], hex_grid, left = FALSE)
  )

  if (nrow(joined) == 0) {
    cli::cli_abort("No records could be assigned to hexagons. Check that {.arg records_sf} overlaps with {.arg aoi_sf}.")
  }

  # Count records per hexagon
  rec_count <- joined |>
    sf::st_drop_geometry() |>
    dplyr::group_by(h3_address) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop")

  # Return only presence hexagons with count
  pres_sf <- hex_grid[hex_grid$h3_address %in% rec_count$h3_address, ]
  pres_sf <- dplyr::left_join(pres_sf, rec_count, by = "h3_address")

  cli::cli_inform(c(
    "v" = "{nrow(pres_sf)} presence hexagon{?s} at H3 resolution {res}.",
    "i" = "{nrow(records_sf)} record{?s} assigned from {nrow(rec_count)} unique hexagon{?s}."
  ))

  return(pres_sf)
}
