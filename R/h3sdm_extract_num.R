#' @name h3sdm_extract_num
#'
#' @title Extract Area-Weighted Mean from Numeric Raster Stack
#'
#' @description
#' Calculates the **area-weighted mean** value for each layer in a numeric
#' \code{SpatRaster} (or single layer) within each polygon feature of an \code{sf} object.
#' This function is designed to efficiently summarize continuous environmental variables
#' (such as bioclimatic data) for predefined spatial units (e.g., H3 hexagons).
#' It utilizes \code{exactextractr} to ensure highly precise zonal statistics by
#' accounting for sub-pixel coverage fractions.
#'
#' @param spat_raster_multi A \code{SpatRaster} object from the \code{terra} package.
#'   Must contain numeric layers (can be a single layer or a stack/brick).
#' @param sf_hex_grid An \code{sf} object containing polygonal geometries (e.g., H3
#'   hexagons). Must be a valid set of polygons for extraction.
#'
#' @return An \code{sf} object identical to \code{sf_hex_grid}, but with new columns
#'   appended. The new column names match the original \code{SpatRaster} layer names.
#'   The values represent the area-weighted mean for that variable within each polygon.
#'
#' @details
#' The function relies on \code{exactextractr::exact_extract} with \code{fun = "weighted_mean"}
#' and \code{weights = "area"}. This methodology is crucial for maintaining spatial
#' accuracy when polygons are irregular or small relative to the raster resolution.
#' A critical check (\code{nrow} match) is performed before binding columns to ensure
#' data integrity
#' and prevent misalignment errors.
#'
#'
#' @examples
#'   library(sf)
#'   library(terra)
#'
#'   # Create a SpatRaster stack with two numeric layers (e.g., bioclimatic variables)
#'   bio1 <- terra::rast(
#'     nrows = 10, ncols = 10,
#'     xmin = -84.5, xmax = -83.5,
#'     ymin = 9.5,  ymax = 10.5,
#'     crs = "EPSG:4326"
#'   )
#'   bio2 <- bio1
#'   terra::values(bio1) <- runif(terra::ncell(bio1), 15, 30)
#'   terra::values(bio2) <- runif(terra::ncell(bio2), 500, 3000)
#'   names(bio1) <- "bio1_temp"
#'   names(bio2) <- "bio12_precip"
#'   bio <- c(bio1, bio2)
#'
#'   # Create a simple hexagon grid as sf polygons
#'   hex_grid <- sf::st_make_grid(
#'     sf::st_as_sfc(sf::st_bbox(c(
#'       xmin = -84.5, xmax = -83.5,
#'       ymin = 9.5,  ymax = 10.5
#'     ), crs = sf::st_crs(4326))),
#'     n = c(3, 3),
#'     square = FALSE
#'   )
#'   h7 <- sf::st_sf(h3_address = paste0("hex_", seq_along(hex_grid)),
#'                   geometry = hex_grid)
#'
#'   # Extract numeric raster values by hexagon (mean per cell)
#'   bio_p <- h3sdm_extract_num(bio, h7)
#'   head(bio_p)
#'
#' @export

h3sdm_extract_num <- function(spat_raster_multi, sf_hex_grid) {

  # --- 1️⃣ Input Validation and Setup ---
  if (!inherits(spat_raster_multi, "SpatRaster")) {
    stop("The first argument must be a 'SpatRaster' object.")
  }
  if (!inherits(sf_hex_grid, "sf")) {
    stop("The second argument must be an 'sf' object with polygons.")
  }

  sf_hex_grid <- sf::st_make_valid(sf_hex_grid)

  # --- 2️⃣ Extract area-weighted mean values ---
  # ❗ CORRECCIÓN CLAVE: Se añade 'weights = "area"' para satisfacer la solicitud de 'weighted_mean'.
  extracted_df <- exactextractr::exact_extract(
    x = spat_raster_multi,
    y = sf_hex_grid,
    fun = "weighted_mean",
    weights = "area",  # <--- Esto resuelve el error
    force_df = TRUE
  )

  # --- 3️⃣ Security Check and Column Naming ---

  if (nrow(extracted_df) != nrow(sf_hex_grid)) {
    stop("Row count mismatch: Extracted data does not match the number of input polygons. Cannot safely bind columns.")
  }

  original_names <- names(spat_raster_multi)
  num_extracted_cols <- ncol(extracted_df)

  if (num_extracted_cols > 0 && num_extracted_cols <= length(original_names)) {
    colnames(extracted_df) <- original_names[1:num_extracted_cols]
  } else {
    warning("Could not rename columns using raster layer names due to mismatch or empty extraction.")
  }

  # --- 4️⃣ Combine with original sf grid ---
  sf_hex_grid_with_data <- dplyr::bind_cols(sf_hex_grid, extracted_df)

  return(sf_hex_grid_with_data)
}
