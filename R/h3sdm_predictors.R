#' @name h3sdm_predictors
#' @title Extract Predictors by Hexagonal Grid
#' @description Generates an H3 hexagonal grid over an AOI and extracts numeric, categorical,
#'   and landscape predictors for each hexagon using functions from the 'paisaje' package.
#' @param aoi_sf An sf object (POLYGON or MULTIPOLYGON) defining the AOI.
#' @param res H3 resolution (default = 6).
#' @param num_rasters A SpatRaster or list of SpatRasters with numeric variables (optional).
#' @param cat_rasters A named list of SpatRasters with categorical variables (optional).
#' @param landscape_raster A SpatRaster with landscape categories (optional, for metrics).
#' @param expand_factor Factor to expand the AOI when creating grid (default = 0.1).
#' @return An sf object (hexagonal grid) with predictor columns added.
#' @examples
#' \dontrun{
#' library(terra)
#' library(sf)
#' bio <- rast("cr_bio/bio.tif")
#' lc  <- rast("landcover.tif")
#' cr  <- cr_outline_c
#'
#' pred_sf <- H3SDM_predictors(
#'   aoi_sf = cr,
#'   res = 6,
#'   num_rasters = bio,
#'   cat_rasters = list(landcover = lc)
#' )
#' }
#' @export

h3sdm_predictors <- function(aoi_sf,
                       res = 6,
                       num_rasters = NULL,
                       cat_rasters = NULL,
                       landscape_raster = NULL,
                       expand_factor = 0.1) {

  # 1️⃣ Crear grilla H3
  grid_sf <- get_h3_grid(aoi_sf, resolution = res, expand_factor = expand_factor)
  grid_sf$plot_id <- seq_len(nrow(grid_sf))

  # Convertir la grilla inicial a SpatVector de terra
  grid_sv <- terra::vect(grid_sf)

  # Función para procesar y unir los datos
  join_and_clean <- function(extracted_sf, current_sv) {
    # Convertir a SpatVector y limpiar las columnas duplicadas
    extracted_sv <- terra::vect(extracted_sf)
    extracted_sv <- extracted_sv[, c("plot_id", names(extracted_sv)[!names(extracted_sv) %in% c("h3_address", "plot_id")])]
    # Unir los SpatVectors
    merged_sv <- terra::merge(current_sv, extracted_sv, by = "plot_id")
    return(merged_sv)
  }

  # 2️⃣ Extraer rasters numéricos
  if (!is.null(num_rasters)) {
    if (!is.list(num_rasters)) {
      num_rasters <- list(num_rasters)
      names(num_rasters) <- "raster_num"
    }
    for (i in seq_along(num_rasters)) {
      r <- num_rasters[[i]]
      extracted_sf <- extract_num_raster(r, grid_sf)
      grid_sv <- join_and_clean(extracted_sf, grid_sv)
    }
  }

  # 3️⃣ Extraer rasters categóricos
  if (!is.null(cat_rasters)) {
    if (!is.list(cat_rasters)) {
      cat_rasters <- list(cat_rasters)
      names(cat_rasters) <- "raster_cat"
    }
    for (name in names(cat_rasters)) {
      r <- cat_rasters[[name]]
      extracted_sf <- extract_cat_raster(r, grid_sf)
      grid_sv <- join_and_clean(extracted_sf, grid_sv)
    }
  }

  # 4️⃣ Extraer métricas de paisaje (IT metrics)
  if (!is.null(landscape_raster)) {
    extracted_sf <- calculate_it_metrics(landscape_raster, grid_sf)
    grid_sv <- join_and_clean(extracted_sf, grid_sv)
  }

  # 5️⃣ Convertir el SpatVector final a sf y forzar MULTIPOLYGON
  grid_sf_final <- sf::st_as_sf(grid_sv)
  grid_sf_final <- sf::st_make_valid(grid_sf_final)       # asegurar geometrías válidas
  grid_sf_final <- sf::st_cast(grid_sf_final, "MULTIPOLYGON")  # forzar tipo

  # 6️⃣ Borrar columna plot_id
  grid_sf_final$plot_id <- NULL

  return(grid_sf_final)
}
