#' @name h3sdm_classify
#' @title Classify predictions based on an optimal threshold
#' @description Converts continuous probability predictions into binary
#'   presence/absence based on a specified threshold.
#' @param predictions_sf An sf object with a 'prediction' column from h3sdm_predict().
#' @param threshold The optimal probability threshold (e.g., 0.45).
#' @return An sf object with the same geometry as `predictions_sf` and a new integer column `predicted_presence` with values 0 (absence) or 1 (presence).
#' @importFrom dplyr mutate if_else
#' @export

h3sdm_classify <- function(predictions_sf, threshold) {
  predictions_sf %>%
    dplyr::mutate(
      predicted_presence = dplyr::if_else(
        prediction >= threshold,
        1,
        0
      )
    )
}
