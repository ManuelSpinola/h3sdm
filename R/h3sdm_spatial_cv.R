#' @name h3sdm_spatial_cv
#' @title Create a spatial-aware cross-validation split for H3 data
#' @description
#' Generates a spatially aware cross-validation split for species distribution modeling
#' using H3 hexagonal grids. This helps avoid inflated model performance estimates
#' caused by spatial autocorrelation, producing more robust model evaluation.
#'
#' @param data An `sf` object, typically the output of `h3sdm_data()`.
#' @param method Character. The spatial resampling method to use:
#'   \describe{
#'     \item{"block"}{Use `spatialsample::spatial_block_cv()` for block-based spatial CV.}
#'     \item{"cluster"}{Use `spatialsample::spatial_clustering_cv()` for cluster-based spatial CV.}
#'   }
#' @param v Integer. Number of folds (default = 5).
#' @param ... Additional arguments passed to the underlying `spatialsample` function.
#'
#' @details
#' Spatial cross-validation avoids overly optimistic performance estimates
#' by ensuring that training and testing data are spatially separated.
#' - `"block"`: Divides the spatial domain into contiguous blocks.
#' - `"cluster"`: Groups locations into spatial clusters before splitting.
#'
#' @return An `rsplit` object (from `rsample`) representing the spatial CV folds.
#'
#' @examples
#' \dontrun{
#' # Example: Create spatial cross-validation splits for H3 data
#'
#' # Block spatial CV with default folds
#' spatial_cv_block <- h3sdm_spatial_cv(combined_data, method = "block")
#'
#' # Cluster spatial CV with 10 folds
#' spatial_cv_cluster <- h3sdm_spatial_cv(combined_data, method = "cluster", v = 10)
#' }
#'
#' @importFrom spatialsample spatial_block_cv spatial_clustering_cv
#'
#' @export

h3sdm_spatial_cv <- function(data, method = "block", v = 5, ...) {
  if (method == "block") {
    split <- spatialsample::spatial_block_cv(data, v = v, ...)
  } else if (method == "cluster") {
    split <- spatialsample::spatial_clustering_cv(data, v = v, ...)
  } else {
    stop("Invalid method. Choose 'block' or 'cluster'.")
  }
  return(split)
}



