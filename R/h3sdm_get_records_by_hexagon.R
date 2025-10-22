#' Download Species Records and Count Occurrences per H3 Hexagon
#'
#' @name h3sdm_get_records_by_hexagon
#' @title Download Species Records and Count Occurrences per H3 Hexagon
#'
#' @description
#' This function downloads occurrence records for one or more species and counts
#' the number of records falling inside each H3 hexagon covering the specified Area
#' of Interest (AOI).
#'
#' @param species Character vector of species names to query (e.g., \code{c("Puma concolor", "Panthera onca")}).
#' @param aoi_sf An \code{sf} polygon defining the Area of Interest (AOI).
#' @param res Numeric. H3 resolution level (default 6), determining hexagon size.
#' @param providers Character vector of data providers (e.g., "gbif"). If \code{NULL}, all providers are used.
#' @param remove_duplicates Logical. If \code{TRUE}, duplicate coordinates are removed before counting. Default is \code{FALSE}.
#' @param date Character vector specifying a date range (e.g., \code{c('2000-01-01','2020-12-31')}).
#' @param expand_factor Numeric. Factor to expand the AOI bounding box before generating the H3 grid. Default is 0.1.
#' @param limit Numeric. Maximum number of records to retrieve per species per provider. Default is 500.
#'
#' @return An \code{sf} object containing the H3 hexagonal grid (\code{MULTIPOLYGON}) with
#'   additional integer columns for each species (spaces replaced by underscores) showing
#'   the count of occurrence records in each hexagon. Hexagons with no records have 0.
#'
#' @details
#' For each species:
#' \enumerate{
#'   \item An H3 grid is generated across the AOI using \code{h3sdm_get_grid()}.
#'   \item Occurrence records are downloaded using \code{h3sdm_get_records()}.
#'   \item Points are joined to the hexagonal grid with \code{sf::st_join()}.
#'   \item Counts of points per hexagon are calculated.
#'   \item Counts are merged into the main hex grid.
#' }
#' The function ensures column names derived from species names are safe in R
#' by replacing spaces with underscores and handles API failures gracefully.
#'
#' @seealso \code{\link[h3sdm]{h3sdm_get_grid}}, \code{\link[h3sdm]{h3sdm_get_records}}
#'
#' @examples
#' \dontrun{
#' # Assuming aoi_sf is a valid sf polygon
#' # species_list <- c("Agalychnis callidryas", "Smilisca baudinii")
#' # hex_counts <- h3sdm_get_records_by_hexagon(species_list, aoi_sf, res = 7, limit = 1000)
#' # head(hex_counts)
#' }
#'
#' @export
h3sdm_get_records_by_hexagon <- function(species,
                                         aoi_sf,
                                         res = 6,
                                         providers = NULL,
                                         remove_duplicates = FALSE,
                                         date = NULL,
                                         expand_factor = 0.1,
                                         limit = 500) {

  # --- 1️⃣ Validations ---
  if (!inherits(aoi_sf, "sf")) stop("`aoi_sf` must be an sf object.")
  if (!is.character(species)) stop("`species` must be a character vector.")
  if (any(is.na(species))) stop("`species` names cannot contain NA values.")

  # --- 2️⃣ Clean species names for safe column names ---
  # Replace spaces with underscores
  species_clean <- gsub(" ", "_", species)

  # --- 3️⃣ Create H3 grid over AOI ---
  # suppressWarnings is used to handle common sf warnings during casting/clipping
  # Assumes h3sdm_get_grid function is available.
  hex_grid <- suppressWarnings(
    h3sdm_get_grid(aoi_sf, res = res, expand_factor = expand_factor)
  )
  # Keep only essential columns and cast to safe geometry type
  hex_grid <- hex_grid[, c("h3_address", "geometry")]
  hex_grid <- suppressWarnings(sf::st_cast(hex_grid, "MULTIPOLYGON"))

  # --- 4️⃣ Initialize species count columns ---
  for (sp in species_clean) {
    hex_grid[[sp]] <- 0 # Initialize with zero count
  }

  # --- 5️⃣ Iterate over each species ---
  for (i in seq_along(species)) {
    sp_name <- species[i]
    sp_col <- species_clean[i]

    # Get species records using the robust helper function (h3sdm_get_records)
    sp_sf <- tryCatch({
      h3sdm_get_records(
        species = sp_name,
        aoi_sf = aoi_sf,
        providers = providers,
        remove_duplicates = remove_duplicates,
        date = date,
        limit = limit
      )
    }, error = function(e) {
      # Catch and warn about API/download failures
      warning("Failed to get records for ", sp_name, ": ", e$message)
      # Return an empty object to skip processing
      return(sf::st_sf(geometry = sf::st_sfc(crs = 4326)))
    })

    # Skip if no data was returned successfully (handles h3sdm_get_records empty return)
    if (is.null(sp_sf) || nrow(sp_sf) == 0) {
      message("No records found for species: ", sp_name)
      next
    }

    # --- 6️⃣ Spatial join: assign points to hexes ---
    # Left = FALSE ensures only points that fall inside a hexagon are kept
    joined <- suppressWarnings(sf::st_join(sp_sf, hex_grid, left = FALSE))

    # --- 7️⃣ Count records per hex ---
    rec_count <- joined |>
      sf::st_drop_geometry() |>
      dplyr::group_by(h3_address) |>
      dplyr::summarise(n = dplyr::n(), .groups = "drop")

    # --- 8️⃣ Merge counts into hex grid ---
    # Left join to preserve all hexes (even those with n=0)
    hex_grid <- dplyr::left_join(hex_grid, rec_count, by = "h3_address")

    # Update the initialized species column with new counts
    # Only update where 'n' is NOT NA (where a join occurred)
    hex_grid[[sp_col]][!is.na(hex_grid$n)] <- hex_grid$n[!is.na(hex_grid$n)]

    # Remove the temporary 'n' column before the next iteration
    hex_grid$n <- NULL
  }

  # --- 9️⃣ Return final hex grid ---
  return(hex_grid)
}
