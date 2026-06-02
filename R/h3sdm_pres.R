#' @name h3sdm_pres
#' @title Assign species presence records to H3 hexagons
#' @description
#' Generates a hexagonal grid over the AOI, downloads species occurrence records,
#' and assigns them to hexagons. Returns only hexagons with at least one presence
#' record. This is the first step of a two-stage workflow where pseudo-absences
#' are generated later using \code{h3sdm_pa()} after environmental variables
#' have been extracted with \code{h3sdm_extract_num()} and related functions.
#'
#' @param species `character` Species name (single string) for which records are
#'   requested.
#' @param aoi_sf `sf` AOI (area of interest) polygon.
#' @param res `integer` H3 resolution for the hexagonal grid.
#' @param providers `character` Optional vector of data providers. Accepted
#'   values: any provider supported by \code{spocc} (e.g., \code{"gbif"},
#'   \code{"inat"}) plus \code{"biodatacr"} for BiodataCR (Costa Rica), queried
#'   via the \code{rbiodatacr} package. If \code{NULL} (default), all
#'   \code{spocc} providers are used.
#' @param remove_duplicates `logical` Remove duplicate records at the same
#'   coordinates. Default is \code{FALSE}.
#' @param date `character` Optional date filter for records.
#' @param limit `integer` Maximum number of records to download. Default is
#'   \code{500}.
#' @param expand_factor `numeric` Factor to expand AOI before creating the
#'   hexagonal grid. Default is \code{0.1}.
#'
#' @return `sf` object with columns:
#'   - `h3_address`: H3 index of the hexagon.
#'   - `presence`: integer column with value \code{1} for all returned hexagons.
#'   - `geometry`: MULTIPOLYGON of each hexagon.
#'
#' @details
#' Unlike \code{h3sdm_pa()}, this function does not sample pseudo-absences.
#' It is intended to be used as the first step of a workflow where environmental
#' variables are extracted for the full hexagonal grid before pseudo-absences
#' are selected in environmental space using \code{h3sdm_pa()}.
#'
#' @examples
#' \dontrun{
#' data(cr_outline_c, package = "h3sdm")
#' pres <- h3sdm_pres("Agalychnis callidryas", cr_outline_c, res = 7)
#' }
#' @export

h3sdm_pres <- function(species,
                       aoi_sf,
                       res          = 6,
                       providers    = NULL,
                       remove_duplicates = FALSE,
                       date         = NULL,
                       limit        = 500,
                       expand_factor = 0.1) {

  # --- Validate inputs -------------------------------------------------------
  if (!inherits(aoi_sf, "sf"))    stop("aoi_sf must be an sf object")
  if (!is.character(species))     stop("species must be a character string")

  # --- 1. Generate hexagonal grid --------------------------------------------
  hex_grid <- suppressWarnings(
    h3sdm_get_grid(aoi_sf, res = res, expand_factor = expand_factor)
  )
  hex_grid <- hex_grid[, c("h3_address", "geometry")]
  hex_grid <- sf::st_cast(hex_grid, "MULTIPOLYGON")

  # --- 2. Download occurrence records ----------------------------------------
  sp_sf <- suppressWarnings(
    h3sdm_get_records(species, aoi_sf,
                      providers         = providers,
                      remove_duplicates = remove_duplicates,
                      date              = date,
                      limit             = limit)
  )

  # --- 3. Return early if no records found -----------------------------------
  if (nrow(sp_sf) == 0) {
    warning("No records found for species: ", species)
    return(sf::st_sf(
      h3_address = character(0),
      presence   = integer(0),
      geometry   = sf::st_sfc(crs = sf::st_crs(hex_grid))
    ))
  }

  # --- 4. Assign records to hexagons -----------------------------------------
  sp_sf_clean <- sp_sf %>% dplyr::select(geometry)
  sp_sf_clean <- sf::st_transform(sp_sf_clean, sf::st_crs(hex_grid))

  joined <- suppressWarnings(
    sf::st_join(sp_sf_clean, hex_grid, left = FALSE)
  )

  rec_count <- joined %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(hex_id = h3_address) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop")

  # --- 5. Return only presence hexagons --------------------------------------
  pres_hex <- hex_grid[hex_grid$h3_address %in% rec_count$hex_id, ]
  pres_hex$presence <- 1L

  return(pres_hex)
}
