#' @name h3sdm_pa
#' @title Generate presence-absence dataset by H3 hexagons
#'
#' @param species Character. Species name (e.g. "Panthera onca")
#' @param aoi_sf sf polygon. Area of interest
#' @param res Integer. H3 resolution
#' @param n_neg Integer. Number of pseudo-absence hexagons to sample
#' @param providers Optional. Data providers for get_records()
#' @param remove_duplicates Logical. Remove duplicate records
#' @param expand_factor Numeric. Buffer factor for grid expansion
#'
#' @return sf object with presence (1/0) column
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
