#' @name h3sdm_spatial_cv
#' @title Create a spatial-aware cross-validation split for H3 data
#' @description This function creates a spatial-aware data split using `spatialsample`,
#'   which is crucial for avoiding inflated model performance due to spatial autocorrelation.
#' @param data An `sf` object, typically the result of `h3sdm_data()`.
#' @param method A character string specifying the spatial sampling method.
#'   Options are "block" for `spatial_block_cv` or "cluster" for `spatial_cluster_cv`.
#' @param v The number of folds for cross-validation.
#' @param ... Additional arguments passed to the underlying `spatialsample` function.
#' @return A `rsplit` object from `rsample` with a spatial data split.
#' @importFrom spatialsample spatial_block_cv spatial_clustering_cv
#' @examples
#' # Use block cross-validation
#' # spatial_cv_block <- h3sdm_spatial_cv(combined_data, method = "block")
#' # Use cluster cross-validation
#' # spatial_cv_cluster <- h3sdm_spatial_cv(combined_data, method = "cluster", v = 10)
#' @export

h3sdm_spatial_cv <- function(data, method = "block", v = 5, ...) {
  if (method == "block") {
    split <- spatialsample::spatial_block_cv(data, v = v, ...)
  } else if (method == "cluster") {
    split <- spatialsample::spatial_cluster_cv(data, v = v, ...)
  } else {
    stop("Invalid method. Choose 'block' or 'cluster'.")
  }
  return(split)
}



