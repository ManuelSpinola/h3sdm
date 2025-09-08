#' @name h3sdm_pa
#' @title Generate presence-absence datasets using H3 hexagons
#' @description
#' Generates a spatial presence-absence dataset for a given species using H3 hexagons.
#' Records are obtained from one or more data providers, aggregated into hexagons, and pseudo-absence hexagons are sampled.
#'
#' @param species Character. Species name (e.g., "Panthera onca").
#' @param aoi_sf An `sf` polygon defining the area of interest.
#' @param res Integer. H3 resolution for the hexagonal grid (default 6).
#' @param n_neg Integer. Number of pseudo-absence hexagons to sample (default 500).
#' @param providers Optional. Data providers for `get_records_by_hexagon()`.
#' @param remove_duplicates Logical. Whether to remove duplicate occurrence records (default FALSE).
#' @param date Optional. Date filter for species records (default NULL).
#' @param expand_factor Numeric. Buffer factor to expand the grid beyond the AOI (default 0.1).
#'
#' @return An `sf` object with:
#' \describe{
#'   \item{h3_address}{H3 hexagon index.}
#'   \item{presence}{Factor with levels "0" (absence) and "1" (presence).}
#'   \item{geometry}{Hexagon geometry.}
#' }
#'
#' @examples
#' \dontrun{
#' library(sf)
#' my_aoi <- st_read("my_aoi.shp")
#' pa_data <- h3sdm_pa(
#'   species = "Panthera onca",
#'   aoi_sf  = my_aoi,
#'   res     = 6,
#'   n_neg   = 500
#' )
#' }
#'
#' @importFrom dplyr mutate filter slice_sample bind_rows
#' @importFrom sf st_as_sf
#' @export

h3sdm_pa <- function(species,
                     aoi_sf,
                     res = 6,
                     n_neg = 500,
                     providers = NULL,
                     remove_duplicates = FALSE,
                     date = NULL,
                     expand_factor = 0.1) {

  # hex grid + records
  rec <- get_records_by_hexagon(species_names = species,
                                aoi_sf = aoi_sf,
                                res = res,
                                providers = providers,
                                remove_duplicates = remove_duplicates,
                                expand_factor = expand_factor)

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

  # The key change: convert the 'presence' variable to a factor
  dataset_sf$presence <- factor(dataset_sf$presence, levels = c("0", "1"))

  return(dataset_sf)
}
