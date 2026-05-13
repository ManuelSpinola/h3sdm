#' @name h3sdm_count_from_records
#' @title Generate species richness or abundance count dataset from records
#' @description
#' Takes a user-provided dataset with presence records (from Excel, fieldwork,
#' acoustic detections, camera traps, or any other source) and generates a
#' hexagonal grid with counts (species richness, total detections, or individuals)
#' ready for analysis with h3sdm. The input can be a \code{data.frame} with
#' coordinate columns or an \code{sf} object. Coordinates are assumed to be in
#' WGS84 (EPSG:4326).
#'
#' @param records \code{data.frame} or \code{sf} object containing records.
#' @param aoi_sf \code{sf} AOI (area of interest) polygon.
#' @param res \code{integer} H3 resolution for the hexagonal grid. Default \code{7}.
#' @param expand_factor \code{numeric} Factor to expand AOI before creating hex grid.
#'   Default \code{0.1}.
#' @param lon_col \code{character} Name of the longitude column. Ignored if
#'   \code{records} is already an \code{sf} object. Default \code{"x"}.
#' @param lat_col \code{character} Name of the latitude column. Ignored if
#'   \code{records} is already an \code{sf} object. Default \code{"y"}.
#' @param species_col \code{character} Name of the column containing species names.
#'   Required when \code{count_type = "richness"}.
#' @param count_type \code{character} One of \code{"richness"} (number of unique
#'   species per hexagon), \code{"detections"} (total number of records per hexagon),
#'   or \code{"individuals"} (sum of a numeric abundance column per hexagon).
#'   Default \code{"richness"}.
#' @param presence_col \code{character} Optional. Name of the column indicating
#'   presence (1) or absence (0). If provided, only records with value == 1 are used.
#' @param abundance_col \code{character} Required when \code{count_type = "individuals"}.
#'   Name of the column with individual counts to sum per hexagon.
#' @param confidence_col \code{character} Optional. Name of the column with detection
#'   confidence scores (numeric between 0 and 1). Useful for acoustic detection data
#'   (e.g. BirdNET output).
#' @param confidence_threshold \code{numeric} Optional. Minimum confidence score to
#'   retain a record. Ignored if \code{confidence_col} is \code{NULL}.
#' @param date_col \code{character} Optional. Name of the date column. The column
#'   must be of class \code{Date}. If your dates are stored as Excel numeric values,
#'   convert them first with \code{as.Date(datos$Fecha, origin = "1899-12-30")}.
#' @param date_min \code{character} or \code{Date} Optional. Minimum date to retain
#'   records (inclusive). Format \code{"YYYY-MM-DD"}.
#' @param date_max \code{character} or \code{Date} Optional. Maximum date to retain
#'   records (inclusive). Format \code{"YYYY-MM-DD"}.
#'
#' @return \code{sf} object with columns:
#'   \describe{
#'     \item{h3_address}{H3 index of the hexagon.}
#'     \item{count}{Numeric count per hexagon (richness, detections, or individuals).}
#'     \item{geometry}{MULTIPOLYGON of each hexagon.}
#'   }
#'
#' @examples
#' \donttest{
#' data(cr_outline_c, package = "h3sdm")
#'
#' my_records <- data.frame(
#'   x         = c(-84.1, -84.2, -83.9, -84.0, -84.1),
#'   y         = c(9.9, 10.1, 9.8, 9.95, 10.0),
#'   Especie   = c("Ara macao", "Ara macao", "Pharomachrus mocinno",
#'                 "Tapirus bairdii", "Ara macao"),
#'   Presencia = c(1, 1, 1, 1, 0)
#' )
#'
#' richness_hex <- h3sdm_count_from_records(
#'   records      = my_records,
#'   aoi_sf       = cr_outline_c,
#'   res          = 7,
#'   lon_col      = "x",
#'   lat_col      = "y",
#'   species_col  = "Especie",
#'   count_type   = "richness",
#'   presence_col = "Presencia"
#' )
#' }
#' @export
h3sdm_count_from_records <- function(
    records,
    aoi_sf,
    res                  = 7,
    expand_factor        = 0.1,
    lon_col              = "x",
    lat_col              = "y",
    species_col          = NULL,
    count_type           = c("richness", "detections", "individuals"),
    presence_col         = NULL,
    abundance_col        = NULL,
    confidence_col       = NULL,
    confidence_threshold = NULL,
    date_col             = NULL,
    date_min             = NULL,
    date_max             = NULL
) {

  count_type <- match.arg(count_type)

  # 1. Validaciones
  if (!inherits(aoi_sf, "sf")) stop("aoi_sf must be an sf object")

  if (!inherits(records, c("data.frame", "sf"))) {
    stop("records must be a data.frame or sf object")
  }

  if (count_type == "richness" && is.null(species_col)) {
    stop("species_col is required when count_type = 'richness'")
  }

  if (count_type == "individuals" && is.null(abundance_col)) {
    stop("abundance_col is required when count_type = 'individuals'")
  }

  # 2. Convertir a sf si es data.frame
  if (!inherits(records, "sf")) {
    if (!lon_col %in% names(records)) stop("Column '", lon_col, "' not found in records")
    if (!lat_col %in% names(records)) stop("Column '", lat_col, "' not found in records")
    records <- sf::st_as_sf(
      records,
      coords = c(lon_col, lat_col),
      crs    = 4326,
      remove = FALSE
    )
  }

  # 3. Filtrar por confianza
  if (!is.null(confidence_col) && !is.null(confidence_threshold)) {
    if (!confidence_col %in% names(records)) {
      stop("Column '", confidence_col, "' not found in records")
    }
    records[[confidence_col]] <- as.numeric(as.character(records[[confidence_col]]))
    n_before <- nrow(records)
    records <- records[records[[confidence_col]] >= confidence_threshold, ]
    message(n_before - nrow(records), " record(s) removed due to confidence < ",
            confidence_threshold)
  }

  # 4. Filtrar por presencia
  if (!is.null(presence_col)) {
    if (!presence_col %in% names(records)) {
      stop("Column '", presence_col, "' not found in records")
    }
    records <- records[records[[presence_col]] == 1, ]
  }

  # 5. Filtrar por fecha
  if (!is.null(date_col)) {
    if (!date_col %in% names(records)) {
      stop("Column '", date_col, "' not found in records")
    }
    if (!inherits(records[[date_col]], "Date")) {
      stop("Column '", date_col, "' must be of class Date. ",
           "Convert it first with as.Date()")
    }
    if (!is.null(date_min)) {
      records <- records[records[[date_col]] >= as.Date(date_min), ]
    }
    if (!is.null(date_max)) {
      records <- records[records[[date_col]] <= as.Date(date_max), ]
    }
    if (nrow(records) == 0) {
      stop("No records remaining after date filter")
    }
  }

  if (nrow(records) == 0) stop("No records remaining after filtering")

  # 6. Generar grid hexagonal
  hex_grid <- suppressWarnings(
    h3sdm_get_grid(aoi_sf, res = res, expand_factor = expand_factor)
  )
  hex_grid <- hex_grid[, c("h3_address", "geometry")]
  hex_grid <- sf::st_cast(hex_grid, "MULTIPOLYGON")

  # 7. Seleccionar columnas relevantes para el join
  cols_keep <- "geometry"
  if (count_type == "richness")    cols_keep <- c(species_col,   cols_keep)
  if (count_type == "individuals") cols_keep <- c(abundance_col, cols_keep)

  joined <- suppressWarnings(
    sf::st_join(
      records[, cols_keep],
      hex_grid,
      left = FALSE
    )
  )

  # 8. Calcular conteo según tipo
  count_df <- switch(count_type,

                     "richness" = joined %>%
                       sf::st_drop_geometry() %>%
                       dplyr::group_by(h3_address) %>%
                       dplyr::summarise(
                         count = dplyr::n_distinct(.data[[species_col]]),
                         .groups = "drop"
                       ),

                     "detections" = joined %>%
                       sf::st_drop_geometry() %>%
                       dplyr::group_by(h3_address) %>%
                       dplyr::summarise(
                         count = dplyr::n(),
                         .groups = "drop"
                       ),

                     "individuals" = joined %>%
                       sf::st_drop_geometry() %>%
                       dplyr::group_by(h3_address) %>%
                       dplyr::summarise(
                         count = sum(.data[[abundance_col]], na.rm = TRUE),
                         .groups = "drop"
                       )
  )

  # 9. Unir al grid completo (hexágonos sin registros = 0)
  dataset_sf <- dplyr::left_join(hex_grid, count_df, by = "h3_address")
  dataset_sf$count[is.na(dataset_sf$count)] <- 0L

  return(dataset_sf)
}
