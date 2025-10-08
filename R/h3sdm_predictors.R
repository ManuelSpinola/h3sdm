#' @name h3sdm_predictors
#' @title Extract Predictors for Hexagonal Grids
#' @description
#' Generates an H3 hexagonal grid over a specified area of interest (AOI) and extracts
#' numeric, categorical, and landscape predictors for each hexagon. Uses functions
#' from the 'paisaje' package to calculate landscape metrics when `landscape_raster` is provided.
#' If `landscape_raster` is not supplied, only numeric and categorical predictors will be extracted.
#'
#' @param aoi_sf An `sf` object (POLYGON or MULTIPOLYGON) defining the area of interest.
#' @param res Integer. H3 resolution (default = 6).
#' @param num_rasters A `SpatRaster` or list of `SpatRaster`s with numeric variables to extract (optional).
#' @param cat_rasters A named list of `SpatRaster`s with categorical variables to extract (optional).
#' @param landscape_raster A `SpatRaster` for calculating landscape metrics (optional).
#' @param expand_factor Numeric. Factor to expand the AOI when creating the H3 grid (default = 0.1).
#'
#' @return An `sf` object (MULTIPOLYGON) representing the H3 grid, with predictor columns added.
#'
#' @examples
#' \dontrun{
#' # Example: Extract numeric and categorical predictors for a study area
#' library(terra)
#' library(sf)
#'
#' bio <- rast("cr_bio/bio.tif")           # numeric raster
#' lc  <- rast("landcover.tif")            # categorical raster
#' cr  <- cr_outline_c                      # area of interest (sf POLYGON/MULTIPOLYGON)
#'
#' pred_sf <- h3sdm_predictors(
#'   aoi_sf = cr,
#'   res = 6,
#'   num_rasters = bio,
#'   cat_rasters = list(landcover = lc)
#' )
#' }
#'
#' @importFrom sf st_as_sf st_make_valid st_cast
#' @importFrom terra vect merge
#' @importFrom paisaje get_h3_grid extract_num_raster extract_cat_raster calculate_it_metrics
#' @export


h3sdm_predictors <- function(aoi_sf,
                             res = 6,
                             num_rasters = NULL,
                             cat_rasters = NULL,
                             landscape_raster = NULL,
                             expand_factor = 0.1) {

  # 1️⃣ Crear grilla H3
  grid_sf <- get_h3_grid(aoi_sf, resolution = res, expand_factor = expand_factor)

  # Asegurar que plot_id exista
  if(!"plot_id" %in% names(grid_sf)) {
    grid_sf$plot_id <- seq_len(nrow(grid_sf))
  }

  # Convertir a SpatVector de terra
  grid_sv <- terra::vect(grid_sf)

  # Función interna para procesar y unir datos
  join_and_clean <- function(extracted_sf, current_sv) {
    extracted_sv <- terra::vect(extracted_sf)

    # Asegurar que plot_id esté presente en el SpatVector extraído
    if(!"plot_id" %in% names(extracted_sv)) {
      extracted_sv$plot_id <- seq_len(nrow(extracted_sv))
    }

    # Seleccionar columnas para merge
    col_to_keep <- c("plot_id", setdiff(names(extracted_sv), c("h3_address", "plot_id")))
    extracted_sv <- extracted_sv[, col_to_keep]

    # Merge con SpatVector actual
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

      # Asegurar plot_id en extracted_sf
      if(!"plot_id" %in% names(extracted_sf)) {
        extracted_sf$plot_id <- grid_sf$plot_id
      }

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

      if(!"plot_id" %in% names(extracted_sf)) {
        extracted_sf$plot_id <- grid_sf$plot_id
      }

      grid_sv <- join_and_clean(extracted_sf, grid_sv)
    }
  }

  # 4️⃣ Extraer métricas de paisaje (IT metrics)
  if (!is.null(landscape_raster)) {
    extracted_sf <- calculate_it_metrics(landscape_raster, grid_sf)

    if(!"plot_id" %in% names(extracted_sf)) {
      extracted_sf$plot_id <- grid_sf$plot_id
    }

    grid_sv <- join_and_clean(extracted_sf, grid_sv)
  }

  # 5️⃣ Convertir el SpatVector final a sf y forzar MULTIPOLYGON
  grid_sf_final <- sf::st_as_sf(grid_sv)
  grid_sf_final <- sf::st_make_valid(grid_sf_final)
  grid_sf_final <- sf::st_cast(grid_sf_final, "MULTIPOLYGON")

  # 6️⃣ Borrar columna plot_id
  grid_sf_final$plot_id <- NULL

  return(grid_sf_final)
}
