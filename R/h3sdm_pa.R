#' @name h3sdm_pa
#' @title Sample Presence-Absence Data for a Species in Hexagonal Grid
#' @description
#' Generates presence-absence points for a species over a specified AOI using H3 hexagons.
#' Retrieves occurrence records, aggregates them by hexagon, and samples pseudo-absences.
#'
#' @param species Character. Species name.
#' @param aoi_sf `sf` polygon of the area of interest (AOI).
#' @param res Integer. H3 resolution (1–16). Default 6.
#' @param n_neg Integer. Number of pseudo-absence hexagons to sample. Default 500.
#' @param providers Character vector of data sources (e.g., "gbif").
#' @param remove_duplicates Logical. Remove duplicate geometries. Default FALSE.
#' @param date Character vector c("start","end") to filter by date.
#' @param limit Integer. Maximum number of occurrence records to download. Default 500.
#' @param expand_factor Numeric. Expand AOI bbox for full hex coverage. Default 0.1.
#' @return `sf` object with columns `h3_address`, `presence` (factor 0/1), and `geometry`.
#' @importFrom paisaje get_records_by_hexagon
#' @examples
#' \dontrun{
#' cr <- sf::st_read(system.file("shape/nc.shp", package="sf"))
#' rec_sf <- h3sdm_pa("Lynx rufus", cr, res = 6, n_neg = 300, limit = 1000)
#' }
#' @export

h3sdm_pa <- function(species,
                     aoi_sf,
                     res = 6,
                     n_neg = 500,
                     providers = NULL,
                     remove_duplicates = FALSE,
                     date = NULL,
                     limit = 500,
                     expand_factor = 0.1) {

  # hex grid + records
  rec <- get_records_by_hexagon(
    species = species,
    aoi_sf = aoi_sf,
    res = res,
    providers = providers,
    remove_duplicates = remove_duplicates,
    date = date,
    limit = limit,
    expand_factor = expand_factor
  )

  # presence variable
  col_name <- gsub(" ", "_", species)
  rec <- rec %>%
    dplyr::mutate(presence = ifelse(.data[[col_name]] > 0, 1, 0)) %>%
    dplyr::select(h3_address, presence, geometry)

  # Pseudo-absences
  n_total_neg <- nrow(dplyr::filter(rec, presence == 0))
  neg_hex <- rec %>%
    dplyr::filter(presence == 0) %>%
    dplyr::slice_sample(n = min(n_neg, n_total_neg)) %>%
    dplyr::mutate(presence = 0)

  pos_hex <- rec %>% dplyr::filter(presence == 1)

  dataset_sf <- dplyr::bind_rows(pos_hex, neg_hex)

  # Convert presence to factor
  dataset_sf$presence <- factor(dataset_sf$presence, levels = c("0", "1"))

  return(dataset_sf)
}
