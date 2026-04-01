#' @name h3sdm_extract_cat
#'
#' @title Calculate Area Proportions for Categorical Raster Classes
#'
#' @description
#' Extracts and calculates the **area proportion** of each land-use/land-cover (LULC)
#' category found within each input polygon of the \code{sf_hex_grid}. This function
#' is tailored for categorical rasters and ensures accurate, sub-pixel weighted statistics.
#'
#' @param spat_raster_cat A single-layer \code{SpatRaster} object containing categorical
#'   values (e.g., LULC classes).
#' @param sf_hex_grid An \code{sf} object containing polygonal geometries (e.g., H3
#'   hexagons). Must contain a column named \code{h3_address} for joining and grouping.
#' @param proportion Logical. If \code{TRUE} (default), the output values are the
#'   proportion of the polygon area covered by each category (summing to 1 for covered area).
#'   If \code{FALSE}, the output is the raw sum of the coverage fraction (area).
#'
#' @return An \code{sf} object identical to \code{sf_hex_grid}, but with new columns
#'   appended for each categorical value found in the raster. Column names follow the
#'   pattern \code{<layer_name>_prop_<category_value>}. Columns are **numerically ordered**
#'   by the category value.
#'
#' @details
#' The function uses a custom function with \code{exactextractr::exact_extract} to
#' perform three critical steps:
#' 1. **Filtering NA/NaN:** Raster cells with missing values (\code{NA}) are explicitly
#'    excluded from the calculation, preventing the creation of a \code{_prop_NaN} column.
#' 2. **Area Consolidation:** It sums the coverage fractions for all fragments belonging
#'    to the same category within the same hexagon, which is essential when polygons
#'    have been clipped or fragmented.
#' 3. **Numerical Ordering:** The final columns are explicitly sorted based on the
#'    numerical value of the category (e.g., \code{_prop_70} appears before \code{_prop_80})
#'    to correct the default alphanumeric sorting behavior of \code{tidyr::pivot_wider}.
#'
#'
#' @examples
#'   library(sf)
#'   library(terra)
#'
#'   # Create a simple categorical SpatRaster
#'   lulc <- terra::rast(
#'     nrows = 20, ncols = 20,
#'     xmin = -85.0, xmax = -83.0,
#'     ymin = 9.0,  ymax = 11.0,
#'     crs = "EPSG:4326"
#'   )
#'   terra::values(lulc) <- sample(1:4, terra::ncell(lulc), replace = TRUE)
#'   names(lulc) <- "landuse"
#'
#'   # Define categorical levels explicitly
#'   levels(lulc) <- data.frame(
#'     value = 1:4,
#'     class = c("forest", "grassland", "urban", "water")
#'   )
#'
#'   # Create a simple hexagon grid as sf polygons (smaller than raster extent)
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
#'   # Extract categorical raster values by hexagon
#'   lulc_p <- h3sdm_extract_cat(lulc, h7, proportion = TRUE)
#'   head(lulc_p)
#'
#' @export

h3sdm_extract_cat <- function(spat_raster_cat, sf_hex_grid, proportion = TRUE) {

  if (!inherits(spat_raster_cat, "SpatRaster"))
    stop("spat_raster_cat must be a SpatRaster")
  if (!inherits(sf_hex_grid, "sf"))
    stop("sf_hex_grid must be an sf object")

  sf_hex_grid <- sf::st_make_valid(sf_hex_grid)
  layer_name <- names(spat_raster_cat)[1]
  if (!"h3_address" %in% names(sf_hex_grid))
    stop("Input sf_hex_grid must contain a column named 'h3_address'.")

  # --- 1. Detectar datatype ---
  dt <- terra::datatype(spat_raster_cat)
  nodata_value <- switch(dt,
                         "INT1U" = 255,
                         "INT2U" = 65535,
                         "INT4U" = 4294967295,
                         "INT1S" = -128,
                         "INT2S" = -32767,
                         "INT4S" = -2147483647,
                         9999)  # fallback si no coincide

  # --- 2. Reemplazar NA por valor válido ---
  spat_raster_cat[is.na(spat_raster_cat)] <- nodata_value

  # --- 3. Asegurarse de que el nivel exista ---
  if (!nodata_value %in% levels(spat_raster_cat)[[1]]$value) {
    levels(spat_raster_cat)[[1]] <- rbind(
      levels(spat_raster_cat)[[1]],
      data.frame(value = nodata_value, class = "NoData")
    )
  }

  # --- resto de la función como antes ---
  extracted <- exactextractr::exact_extract(
    x = spat_raster_cat,
    y = sf_hex_grid,
    summarize_df = TRUE,
    include_cols = "h3_address",
    fun = function(df) {
      df <- df %>% dplyr::filter(!is.na(.data$value))
      if (nrow(df) == 0) return(NULL)

      df_summary <- df %>%
        dplyr::group_by(h3_address, value) %>%
        dplyr::summarise(sum_coverage = sum(.data$coverage_fraction, na.rm = TRUE), .groups = "drop_last") %>%
        dplyr::mutate(total_area_cell = sum(.data$sum_coverage),
                      freq = if (proportion) .data$sum_coverage/.data$total_area_cell else .data$sum_coverage) %>%
        dplyr::ungroup() %>%
        dplyr::select(h3_address, value, freq)

      return(df_summary)
    },
    progress = TRUE
  )

  if (is.null(extracted) || nrow(extracted) == 0) {
    warning("No raster data extracted or all extractions were empty.")
    return(sf_hex_grid)
  }

  extracted_wide <- tibble::as_tibble(extracted) %>%
    tidyr::pivot_wider(
      id_cols = "h3_address",
      names_from = .data$value,
      values_from = .data$freq,
      names_prefix = paste0(layer_name, "_prop_"),
      values_fill = 0
    )

  result_sf <- sf_hex_grid %>% dplyr::left_join(extracted_wide, by = "h3_address")
  prop_cols <- names(result_sf)[grepl(paste0(layer_name, "_prop_"), names(result_sf))]
  result_sf <- result_sf %>% dplyr::mutate(dplyr::across(dplyr::all_of(prop_cols), ~tidyr::replace_na(.x, 0)))

  if (length(prop_cols) > 0) {
    prop_numbers <- gsub(paste0(layer_name, "_prop_"), "", prop_cols)
    ordered_indices <- order(as.numeric(prop_numbers))
    ordered_prop_cols <- prop_cols[ordered_indices]
    cols_to_select <- c("h3_address", ordered_prop_cols, "geometry",
                        setdiff(names(result_sf), c("h3_address", ordered_prop_cols, "geometry")))
    result_sf <- result_sf %>% dplyr::select(dplyr::all_of(cols_to_select))
  }

  return(sf::st_cast(result_sf, "MULTIPOLYGON"))
}
