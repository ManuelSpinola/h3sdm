#' @name h3sdm_classify
#' @title Classify predictions based on an optimal threshold
#' @description
#' Converts continuous probability predictions into binary presence/absence
#' based on a specified threshold.
#'
#' @param predictions_sf An `sf` object containing a numeric column named
#'   `prediction`, typically produced by [h3sdm_predict()].
#' @param threshold A numeric value representing the probability threshold
#'   (e.g., `0.45`) above which predictions are classified as presence (`1`).
#'
#' @return
#' An `sf` object with the same geometry and all original columns, plus a new
#' integer column `predicted_presence` with values `0` (absence) or `1` (presence).
#'
#' @details
#' This function is useful for converting continuous probability outputs into
#' binary presence/absence data for mapping or model evaluation purposes.
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(dplyr)
#'
#' # Crear un sf de ejemplo
#' df <- data.frame(
#'   id = 1:5,
#'   prediction = c(0.2, 0.6, 0.45, 0.8, 0.3),
#'   lon = c(-75, -74, -73, -72, -71),
#'   lat = c(10, 11, 12, 13, 14)
#' )
#'
#' df_sf <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326)
#'
#' # Clasificar usando un umbral
#' classified_sf <- h3sdm_classify(df_sf, threshold = 0.5)
#'
#' # Revisar resultados
#' print(classified_sf)
#' }
#'
#' @importFrom dplyr mutate if_else
#' @export

h3sdm_classify <- function(predictions_sf, threshold) {
  # Validations for CRAN safety
  if (!inherits(predictions_sf, "sf")) {
    stop("`predictions_sf` must be an sf object.", call. = FALSE)
  }
  if (!"prediction" %in% names(predictions_sf)) {
    stop("`predictions_sf` must contain a column named 'prediction'.", call. = FALSE)
  }
  if (!is.numeric(threshold) || length(threshold) != 1) {
    stop("`threshold` must be a single numeric value.", call. = FALSE)
  }

  predictions_sf %>%
    dplyr::mutate(
      predicted_presence = dplyr::if_else(
        prediction >= threshold,
        1L,
        0L
      )
    )
}
