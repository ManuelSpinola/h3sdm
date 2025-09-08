#' @name h3sdm_spatial_cv
#' @title Create a spatial-aware cross-validation split for H3 data
#' @description
#' Generates a spatially aware data split using the `spatialsample` package.
#' This helps avoid inflated model performance estimates due to spatial autocorrelation.
#'
#' @param data An `sf` object, typically the output of `h3sdm_data()`.
#' @param method Character. The spatial resampling method to use:
#'   \describe{
#'     \item{"block"}{Use `spatial_block_cv` for block-based spatial CV.}
#'     \item{"cluster"}{Use `spatial_cluster_cv` for cluster-based spatial CV.}
#'   }
#' @param v Integer. Number of folds (default = 5).
#' @param ... Additional arguments passed to the underlying `spatialsample` function.
#'
#' @return An `rsplit` object (from `rsample`) representing the spatial CV folds.
#'
#' @examples
#' \dontrun{
#' # Block spatial cross-validation
#' spatial_cv_block <- h3sdm_spatial_cv(combined_data, method = "block")
#'
#' # Cluster spatial cross-validation with 10 folds
#' spatial_cv_cluster <- h3sdm_spatial_cv(combined_data, method = "cluster", v = 10)
#' }
#'
#' @importFrom spatialsample spatial_block_cv spatial_cluster_cv
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



