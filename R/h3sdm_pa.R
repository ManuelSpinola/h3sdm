#' @name h3sdm_pa
#' @title Generate presence/pseudo-absence dataset stratified in environmental space
#' @description
#' Combines presence hexagons with pseudo-absences sampled in environmental
#' space. Pseudo-absences are selected by clustering the environmental
#' conditions of hexagons without presence records using k-means, then choosing
#' the hexagon closest to each cluster centroid. This ensures pseudo-absences
#' cover the full range of environmental conditions available in the AOI,
#' reducing bias from spatially clustered occurrence records.
#'
#' @param pres_sf `sf` Presence hexagons returned by \code{h3sdm_pres()}.
#' @param predictors_sf `sf` Full hexagonal grid with extracted environmental
#'   variables, returned by \code{h3sdm_predictors()}.
#' @param n_pseudoabs `integer` Number of pseudo-absence hexagons to sample.
#'   If larger than the number of available hexagons without presence, all
#'   available hexagons are used. Default is \code{500}.
#' @param buffer_k `integer` Number of H3 grid rings to exclude around each
#'   presence hexagon when building the pseudo-absence candidate pool. Hexagons
#'   within \code{buffer_k} rings of any presence are removed before sampling,
#'   preventing pseudo-absences from being placed in areas likely occupied but
#'   not yet recorded. Default is \code{1}. Set to \code{0} to disable.
#'
#' @return `sf` object with columns:
#'   - `h3_address`: H3 index of the hexagon.
#'   - `presence`: factor with levels \code{"0"} (pseudo-absence) and
#'     \code{"1"} (presence).
#'   - `geometry`: MULTIPOLYGON of each hexagon.
#'
#' @details
#' The function scales all numeric predictor columns before clustering.
#' Non-numeric columns and columns with zero variance are excluded from
#' clustering. Pseudo-absences are selected as the hexagon nearest to each
#' k-means centroid in scaled environmental space (Euclidean distance).
#'
#' This function is designed to be used after \code{h3sdm_pres()} and
#' \code{h3sdm_predictors()} in the following workflow:
#' \preformatted{
#' pres        <- h3sdm_pres("Species name", aoi_sf, res = 7)
#' num_vars    <- h3sdm_extract_num(raster_stack, grid)
#' predictors  <- h3sdm_predictors(num_vars)
#' pa          <- h3sdm_pa(pres, predictors, n_pseudoabs = 500)
#' }
#'
#' @examples
#' \dontrun{
#' data(cr_outline_c, package = "h3sdm")
#' pres       <- h3sdm_pres("Agalychnis callidryas", cr_outline_c, res = 7)
#' grid       <- h3sdm_get_grid(cr_outline_c, res = 7)
#' num_vars   <- h3sdm_extract_num(bio, grid)
#' predictors <- h3sdm_predictors(num_vars)
#' pa         <- h3sdm_pa(pres, predictors, n_pseudoabs = 300)
#' }
#' @export

h3sdm_pa <- function(pres_sf,
                     predictors_sf,
                     n_pseudoabs = 500,
                     buffer_k = 1L) {

  # --- Validate inputs -------------------------------------------------------
  if (!inherits(pres_sf, "sf"))
    stop("pres_sf must be an sf object returned by h3sdm_pres().")
  if (!inherits(predictors_sf, "sf"))
    stop("predictors_sf must be an sf object returned by h3sdm_predictors().")
  if (!"h3_address" %in% names(pres_sf))
    stop("pres_sf must contain an 'h3_address' column.")
  if (!"h3_address" %in% names(predictors_sf))
    stop("predictors_sf must contain an 'h3_address' column.")
  if (!is.numeric(buffer_k) || length(buffer_k) != 1L || buffer_k < 0)
    stop("buffer_k must be a non-negative integer.")
  buffer_k <- as.integer(buffer_k)

  # --- 1. Identify absence hexagons ------------------------------------------
  pres_ids <- pres_sf$h3_address

  # Expand exclusion zone using H3 disk rings around each presence hexagon
  if (buffer_k > 0L) {
    disk_ids <- unique(unlist(
      h3jsr::get_disk(pres_ids, ring_size = buffer_k)
    ))
    exclude_ids <- union(pres_ids, disk_ids)
  } else {
    exclude_ids <- pres_ids
  }

  neg_sf   <- predictors_sf[!predictors_sf$h3_address %in% exclude_ids, ]

  if (nrow(neg_sf) == 0)
    stop("No hexagons without presence records found in predictors_sf.")

  n_sample <- min(n_pseudoabs, nrow(neg_sf))

  # --- 2. Prepare environmental matrix for clustering ------------------------
  neg_df <- sf::st_drop_geometry(neg_sf)

  # Keep only numeric predictor columns (exclude h3_address and identifiers)
  num_cols <- names(neg_df)[sapply(neg_df, is.numeric)]

  # Remove columns with zero variance (kmeans cannot handle them)
  zero_var <- sapply(neg_df[, num_cols, drop = FALSE],
                     function(x) stats::var(x, na.rm = TRUE) == 0 | is.na(stats::var(x, na.rm = TRUE)))
  num_cols  <- num_cols[!zero_var]

  if (length(num_cols) == 0)
    stop("No numeric predictor columns with non-zero variance found in predictors_sf.")

  # Scale environmental variables
  env_mat <- scale(as.matrix(neg_df[, num_cols, drop = FALSE]))

  # Handle any remaining NAs after scaling (e.g. from constant columns)
  env_mat[is.na(env_mat)] <- 0

  # --- 3. Cluster absence hexagons in environmental space --------------------
  set.seed(42L)
  km <- stats::kmeans(env_mat, centers = n_sample, nstart = 5, iter.max = 100)

  # --- 4. Select hexagon nearest to each cluster centroid --------------------
  centroids  <- km$centers  # n_sample x length(num_cols) matrix
  selected_idx <- vapply(seq_len(n_sample), function(k) {
    cluster_members <- which(km$cluster == k)
    if (length(cluster_members) == 1L) return(cluster_members)
    member_mat <- matrix(env_mat[cluster_members, ],
                         nrow = length(cluster_members))
    centroid   <- matrix(centroids[k, ], nrow = 1)
    dists      <- rowSums((member_mat -
                             centroid[rep(1, nrow(member_mat)), ])^2)
    cluster_members[which.min(dists)]
  }, integer(1L))

  neg_selected <- neg_sf[selected_idx, c("h3_address", "geometry")]
  neg_selected$presence <- 0L

  # --- 5. Combine presences and pseudo-absences ------------------------------
  pres_out <- pres_sf[, c("h3_address", "geometry")]
  pres_out$presence <- 1L

  dataset_sf <- rbind(pres_out, neg_selected)
  dataset_sf$presence <- factor(dataset_sf$presence, levels = c("0", "1"))

  return(dataset_sf)
}
