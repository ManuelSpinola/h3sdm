#' @name h3sdm_pa
#' @title Generate presence-absence dataset on H3 hexagonal grid
#'
#' @description
#' Downloads species records within an AOI, assigns them to an H3 hexagonal grid,
#' and generates a presence-absence dataset with optional pseudo-absences.
#'
#' @param species Character string with the species name.
#' @param aoi_sf `sf` object defining the area of interest.
#' @param res H3 resolution (integer, default 6).
#' @param n_neg Number of pseudo-absence hexagons to sample (default 500).
#' @param providers Optional vector of data providers (GBIF, iNaturalist, etc.).
#' @param remove_duplicates Logical, whether to remove duplicate records (default FALSE).
#' @param date Optional date filter for records.
#' @param limit Maximum number of records to download (default 500).
#' @param expand_factor Fraction to expand AOI when generating grid (default 0.1).
#'
#' @return `sf` object with columns:
#'   - `h3_address`: H3 hexagon identifier
#'   - `presence`: factor (0 = absence / pseudo-absence, 1 = presence)
#'   - `geometry`: MULTIPOLYGON of the hexagon
#'
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

  if (!inherits(aoi_sf, "sf")) stop("aoi_sf must be an sf object")
  if (!is.character(species)) stop("species must be a character vector")

  # 1️⃣ Generate hexagonal grid
  hex_grid <- h3sdm_get_grid(aoi_sf, res = res, expand_factor = expand_factor)
  hex_grid <- hex_grid[, c("h3_address", "geometry")]
  hex_grid <- sf::st_cast(hex_grid, "MULTIPOLYGON")

  # 2️⃣ Download species records
  sp_sf <- get_records(
    species = species,
    aoi_sf = aoi_sf,
    providers = providers,
    remove_duplicates = remove_duplicates,
    date = date,
    limit = limit
  )

  # If no records found, return all absences
  if (nrow(sp_sf) == 0) {
    hex_grid$presence <- factor(0, levels = c("0", "1"))
    return(hex_grid)
  }

  # 3️⃣ Assign species to hexagons using st_intersects
  hits <- sf::st_intersects(hex_grid, sp_sf)
  hex_grid$presence <- as.integer(lengths(hits) > 0)

  # 4️⃣ Sample pseudo-absences
  pos_hex <- hex_grid[hex_grid$presence == 1, ]
  neg_hex <- hex_grid[hex_grid$presence == 0, ]
  if (nrow(neg_hex) > 0) {
    n_sample <- min(n_neg, nrow(neg_hex))
    neg_hex <- neg_hex[sample(seq_len(nrow(neg_hex)), n_sample), ]
    neg_hex$presence <- 0
  }

  # Combine presence + pseudo-absence
  dataset_sf <- rbind(pos_hex, neg_hex)
  dataset_sf$presence <- factor(dataset_sf$presence, levels = c("0", "1"))

  return(dataset_sf)
}
