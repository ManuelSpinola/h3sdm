#' @name h3sdm_pa_from_records
#' @title Generate presence/pseudo-absence dataset from user-provided records
#' @description
#' Adapts a user-provided dataset with presence records (from personal fieldwork,
#' BiodataCR, or any other source) into a hexagonal presence/pseudo-absence dataset
#' ready for analysis with h3sdm. The input can be a \code{data.frame} with coordinate
#' columns or an \code{sf} object. Coordinates are assumed to be in WGS84 (EPSG:4326).
#'
#' @param records \code{data.frame} or \code{sf} object containing presence records.
#' @param aoi_sf \code{sf} AOI (area of interest) polygon.
#' @param res \code{integer} H3 resolution for the hexagonal grid.
#' @param n_pseudoabs \code{integer} Number of pseudo-absence hexagons to sample.
#' @param expand_factor \code{numeric} Factor to expand AOI before creating hex grid.
#' @param lon_col \code{character} Name of the longitude column. Ignored if \code{records}
#'   is already an \code{sf} object.
#' @param lat_col \code{character} Name of the latitude column. Ignored if \code{records}
#'   is already an \code{sf} object.
#' @param species_col \code{character} Optional. Name of the column containing the species
#'   name. If provided, the column is retained in the output as metadata.
#' @param geospatial_filter \code{logical} If \code{TRUE} (default) and the input contains
#'   a \code{geospatialKosher} column, records with \code{geospatialKosher == FALSE} are
#'   removed before processing. Ignored if the column is absent.
#'
#' @return \code{sf} object with columns:
#'   \describe{
#'     \item{h3_address}{H3 index of the hexagon.}
#'     \item{presence}{Factor with levels \code{"0"} (pseudo-absence) and \code{"1"} (presence).}
#'     \item{species}{Species name (only if \code{species_col} is provided).}
#'     \item{geometry}{MULTIPOLYGON of each hexagon.}
#'   }
#'
#' @examples
#' \donttest{
#' data(cr_outline_c, package = "h3sdm")
#'
#' my_records <- data.frame(
#'   lon = c(-84.1, -84.2, -83.9),
#'   lat = c(9.9, 10.1, 9.8),
#'   species = "Agalychnis callidryas"
#' )
#'
#' dataset <- h3sdm_pa_from_records(
#'   records     = my_records,
#'   aoi_sf      = cr_outline_c,
#'   res         = 7,
#'   n_pseudoabs = 100,
#'   lon_col     = "lon",
#'   lat_col     = "lat",
#'   species_col = "species"
#' )
#' }
#' @export
h3sdm_pa_from_records <- function(records,
                                  aoi_sf,
                                  res = 6,
                                  n_pseudoabs = 500,
                                  expand_factor = 0.1,
                                  lon_col = "lon",
                                  lat_col = "lat",
                                  species_col = NULL,
                                  geospatial_filter = TRUE) {

  # 1. Validate inputs
  if (!inherits(aoi_sf, "sf")) stop("aoi_sf must be an sf object")

  if (!inherits(records, c("data.frame", "sf"))) {
    stop("records must be a data.frame or sf object")
  }

  # 2. Convert to sf if input is a data.frame
  if (!inherits(records, "sf")) {
    if (!lon_col %in% names(records)) {
      stop("Column '", lon_col, "' not found in records")
    }
    if (!lat_col %in% names(records)) {
      stop("Column '", lat_col, "' not found in records")
    }
    records <- sf::st_as_sf(
      records,
      coords = c(lon_col, lat_col),
      crs    = 4326,
      remove = FALSE
    )
  }

  # 3. Filter by geospatialKosher if column is present
  if (geospatial_filter && "geospatialKosher" %in% names(records)) {
    n_before <- nrow(records)
    records <- records[records$geospatialKosher == TRUE, ]
    n_after <- nrow(records)
    if (n_before > n_after) {
      message(n_before - n_after, " record(s) removed due to geospatialKosher == FALSE")
    }
  }

  # 4. Extract species name if species_col is provided
  species_name <- NULL
  if (!is.null(species_col)) {
    if (!species_col %in% names(records)) {
      stop("Column '", species_col, "' not found in records")
    }
    species_name <- unique(records[[species_col]])
    if (length(species_name) > 1) {
      warning(
        "More than one species found in '", species_col,
        "'. Using the first: ", species_name[1]
      )
      species_name <- species_name[1]
      records <- records[records[[species_col]] == species_name, ]
    }
  }

  # 5. Generate hexagonal grid
  hex_grid <- suppressWarnings(
    h3sdm_get_grid(aoi_sf, res = res, expand_factor = expand_factor)
  )
  hex_grid <- hex_grid[, c("h3_address", "geometry")]
  hex_grid <- sf::st_cast(hex_grid, "MULTIPOLYGON")

  # 6. Return early if no records remain after filtering
  if (nrow(records) == 0) {
    warning("No records found after filtering")
    hex_grid$presence <- factor(0, levels = c("0", "1"))
    if (!is.null(species_name)) hex_grid$species <- species_name
    return(hex_grid)
  }

  # 7. Spatial join: assign records to hexagons
  sp_sf_clean <- records[, "geometry"]
  joined <- suppressWarnings(
    sf::st_join(sp_sf_clean, hex_grid, left = FALSE)
  )

  rec_count <- joined %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(hex_id = h3_address) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop")

  # 8. Create presence column
  hex_grid$presence <- 0
  hex_grid$presence[hex_grid$h3_address %in% rec_count$hex_id] <- 1

  # 9. Split into presence and absence hexagons
  pos_hex <- hex_grid[hex_grid$presence == 1, ]
  neg_hex <- hex_grid[hex_grid$presence == 0, ]

  # 10. Sample pseudo-absences
  n_sample <- min(n_pseudoabs, nrow(neg_hex))
  if (n_sample > 0) {
    neg_hex <- neg_hex[sample(seq_len(nrow(neg_hex)), n_sample), ]
  }

  # 11. Combine and convert presence to factor
  dataset_sf <- rbind(pos_hex, neg_hex)
  dataset_sf$presence <- factor(dataset_sf$presence, levels = c("0", "1"))

  # 12. Add species name if provided
  if (!is.null(species_name)) {
    dataset_sf$species <- species_name
  }

  return(dataset_sf)
}
