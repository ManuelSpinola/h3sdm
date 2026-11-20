#' @name h3sdm_pa
#' @title Generate presence/pseudo-absence dataset
#' @description
#' Combines presence hexagons with pseudo-absences selected from hexagons
#' without presence records. Two selection methods are available:
#'
#' - `"environmental"` (default): pseudo-absences are stratified in
#'   environmental space. Hexagons without presence are clustered with
#'   k-means on their environmental conditions, and the hexagon closest to
#'   each cluster centroid is selected. This ensures pseudo-absences cover the
#'   full range of environmental conditions available in the AOI, reducing
#'   bias from spatially clustered occurrence records.
#' - `"random"`: pseudo-absences are drawn at random from the candidate
#'   hexagons. When `n_pseudoabs` is equal to or larger than the number of
#'   candidates, all candidates are returned, so the user can apply their own
#'   sampling or balancing strategy afterwards.
#'
#' @param pres_sf `sf` Presence hexagons returned by \code{h3sdm_pres()}.
#' @param predictors_sf `sf` Full hexagonal grid with extracted environmental
#'   variables, returned by \code{h3sdm_predictors()}.
#' @param n_pseudoabs `integer` Number of pseudo-absence hexagons to select.
#'   If equal to or larger than the number of available hexagons without
#'   presence, all available hexagons are used (no clustering or sampling is
#'   performed). Default is \code{500}.
#' @param buffer_k `integer` Number of H3 grid rings to exclude around each
#'   presence hexagon when building the pseudo-absence candidate pool. Hexagons
#'   within \code{buffer_k} rings of any presence are removed before selection,
#'   preventing pseudo-absences from being placed in areas likely occupied but
#'   not yet recorded. Default is \code{1}. Set to \code{0} to disable.
#' @param method `character` Pseudo-absence selection method: either
#'   \code{"environmental"} (default, k-means stratification in environmental
#'   space) or \code{"random"} (random sampling). See Description.
#'
#' @return `sf` object with columns:
#'   - `h3_address`: H3 index of the hexagon.
#'   - `presence`: factor with levels \code{"0"} (pseudo-absence) and
#'     \code{"1"} (presence).
#'   - `geometry`: MULTIPOLYGON of each hexagon.
#'
#' @details
#' For \code{method = "environmental"}, the function scales all numeric
#' predictor columns before clustering. Non-numeric columns and columns with
#' zero variance are excluded from clustering. Pseudo-absences are selected as
#' the hexagon nearest to each k-means centroid in scaled environmental space
#' (Euclidean distance). If the number of requested pseudo-absences exceeds the
#' number of distinct environmental conditions among the candidates, the number
#' of clusters is reduced to that number, since k-means cannot create more
#' clusters than distinct points. The clustering uses a fixed internal seed so
#' results are reproducible.
#'
#' For \code{method = "random"}, sampling uses R's random number generator, so
#' call \code{set.seed()} beforehand for reproducible results. When all
#' candidates are returned, they keep the order of \code{predictors_sf}.
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
#'
#' # Environmental stratification (default)
#' pa_env <- h3sdm_pa(pres, predictors, n_pseudoabs = 300)
#'
#' # Random sampling
#' set.seed(123)
#' pa_rnd <- h3sdm_pa(pres, predictors, n_pseudoabs = 300, method = "random")
#'
#' # All hexagons without presence (to balance later)
#' pa_all <- h3sdm_pa(pres, predictors, n_pseudoabs = nrow(predictors),
#'                    buffer_k = 0, method = "random")
#' }
#' @export

h3sdm_pa <- function(pres_sf,
                     predictors_sf,
                     n_pseudoabs = 500,
                     buffer_k = 1L,
                     method = c("environmental", "random")) {

  method <- match.arg(method)

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
  if (!is.numeric(n_pseudoabs) || length(n_pseudoabs) != 1L || n_pseudoabs < 1)
    stop("n_pseudoabs must be a positive integer.")
  buffer_k    <- as.integer(buffer_k)
  n_pseudoabs <- as.integer(n_pseudoabs)

  # Standardize the geometry column name
  sf::st_geometry(pres_sf)       <- "geometry"
  sf::st_geometry(predictors_sf) <- "geometry"

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

  neg_sf <- predictors_sf[!predictors_sf$h3_address %in% exclude_ids, ]

  if (nrow(neg_sf) == 0)
    stop("No hexagons without presence records found in predictors_sf.")

  n_sample <- min(n_pseudoabs, nrow(neg_sf))

  # --- 2. Select pseudo-absences ---------------------------------------------
  if (n_sample >= nrow(neg_sf)) {

    # All candidates requested: return them all, no clustering or sampling
    selected_idx <- seq_len(nrow(neg_sf))

  } else if (method == "random") {

    selected_idx <- sort(sample.int(nrow(neg_sf), n_sample))

  } else {

    selected_idx <- .h3sdm_pa_environmental(neg_sf, n_sample)

  }

  neg_selected <- neg_sf[selected_idx, c("h3_address", "geometry")]
  neg_selected$presence <- 0L

  # --- 3. Combine presences and pseudo-absences ------------------------------
  pres_out <- pres_sf[, c("h3_address", "geometry")]
  pres_out$presence <- 1L

  dataset_sf <- rbind(pres_out, neg_selected)
  dataset_sf$presence <- factor(dataset_sf$presence, levels = c("0", "1"))

  return(dataset_sf)
}


# Internal: select pseudo-absences by k-means stratification in environmental
# space. Returns the row indices of neg_sf nearest to each cluster centroid.
.h3sdm_pa_environmental <- function(neg_sf, n_sample) {

  neg_df <- sf::st_drop_geometry(neg_sf)

  # Keep only numeric predictor columns (exclude h3_address and identifiers)
  num_cols <- names(neg_df)[sapply(neg_df, is.numeric)]

  # Remove columns with zero variance (kmeans cannot handle them)
  zero_var <- sapply(neg_df[, num_cols, drop = FALSE], function(x) {
    v <- stats::var(x, na.rm = TRUE)
    is.na(v) || v == 0
  })
  num_cols <- num_cols[!zero_var]

  if (length(num_cols) == 0)
    stop("No numeric predictor columns with non-zero variance found in predictors_sf.")

  # Scale environmental variables
  env_mat <- scale(as.matrix(neg_df[, num_cols, drop = FALSE]))

  # Handle any remaining NAs after scaling
  env_mat[is.na(env_mat)] <- 0

  # k-means cannot create more clusters than distinct points
  n_distinct <- nrow(unique(env_mat))
  if (n_sample > n_distinct) {
    warning(sprintf(
      "Only %d distinct environmental conditions among candidates; using %d pseudo-absences instead of %d.",
      n_distinct, n_distinct, n_sample
    ))
    n_sample <- n_distinct
  }

  # Cluster absence hexagons in environmental space
  set.seed(42L)
  km <- stats::kmeans(env_mat, centers = n_sample, nstart = 5, iter.max = 100)

  # Select hexagon nearest to each cluster centroid
  centroids <- km$centers
  vapply(seq_len(n_sample), function(k) {
    cluster_members <- which(km$cluster == k)
    if (length(cluster_members) == 1L) return(cluster_members)
    member_mat <- matrix(env_mat[cluster_members, ],
                         nrow = length(cluster_members))
    centroid   <- matrix(centroids[k, ], nrow = 1)
    dists      <- rowSums((member_mat -
                             centroid[rep(1, nrow(member_mat)), ])^2)
    cluster_members[which.min(dists)]
  }, integer(1L))
}
