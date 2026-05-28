#' Area of Applicability (AOA) of spatial prediction models
#'
#' Estimates the Dissimilarity Index (DI) and the Area of Applicability
#' (AOA) for new data given the training data and a fitted model. This
#' function is designed to be applied directly to the output of
#' [h3sdm_predict()], so that both the predicted values and the AOA
#' are available in a single \code{sf} object ready for mapping.
#'
#' The algorithm follows Meyer & Pebesma (2021). Predictor variables are
#' extracted automatically from the model formula inside
#' \code{fit_object}. They are standardized using z-score scaling
#' computed from \code{train}, then optionally weighted by variable
#' importance. The mean nearest-neighbor distance among training points
#' (\code{trainDist_avrgmean}) is used to normalize DI values. The AOA
#' threshold is the maximum cross-validated training DI after removing
#' outliers with Tukey's rule (Q3 + 1.5 * IQR). Locations with
#' \code{DI <= threshold} are inside the AOA; locations above the
#' threshold should be interpreted with caution.
#'
#' Variable importance is extracted automatically for \code{ranger} and
#' \code{xgboost} models via \code{vip::vi()}. For GAM models, or when
#' importance cannot be extracted, all variables receive equal weight.
#'
#' @param newdata An \code{sf} object, typically the direct output of
#'   [h3sdm_predict()], containing the predictor variables and a
#'   \code{prediction} column.
#' @param train An \code{sf} object, typically the output of
#'   [h3sdm_data()], containing the training observations with the same
#'   predictor variables used in \code{fit_object}.
#' @param fit_object The list returned by [h3sdm_fit_model()], used to
#'   extract the model formula (predictor variable names) and variable
#'   importances when available.
#' @param cv An \code{rset} object returned by [h3sdm_spatial_cv()],
#'   used to extract cross-validation fold assignments. If \code{NULL}
#'   (default), Leave-One-Out (LOO) cross-validation is used.
#' @param verbose Logical. Should progress messages be printed?
#'   Default \code{TRUE}.
#'
#' @return The input \code{newdata} \code{sf} object with two additional
#'   columns:
#'   \describe{
#'     \item{\code{DI}}{Numeric. Dissimilarity Index for each
#'       observation. Values near 0 indicate high similarity to the
#'       training data; larger values indicate increasing dissimilarity.}
#'     \item{\code{AOA}}{Integer. \code{1} = inside the AOA;
#'       \code{0} = outside the AOA.}
#'   }
#'
#' @references
#' Meyer, H., Pebesma, E. (2021): Predicting into unknown space?
#' Estimating the area of applicability of spatial prediction models.
#' \emph{Methods in Ecology and Evolution} 12: 1620--1633.
#' \doi{10.1111/2041-210X.13650}
#'
#' @seealso [h3sdm_predict()], [h3sdm_fit_model()], [h3sdm_spatial_cv()],
#'   [h3sdm_data()]
#'
#' @examples
#' \dontrun{
#' cv      <- h3sdm_spatial_cv(dat, method = "block")
#' fit     <- h3sdm_fit_model(workflow, cv)
#' pred    <- h3sdm_predict(fit, new_data = h7)
#' result  <- h3sdm_aoa(pred, train = dat, fit_object = fit, cv = cv)
#' }
#'
#' @importFrom workflows extract_fit_parsnip
#' @importFrom rsample assessment
#' @importFrom sf st_drop_geometry st_sf st_geometry
#' @importFrom stats sd quantile setNames
#' @export
h3sdm_aoa <- function(newdata, train, fit_object, cv = NULL, verbose = TRUE) {

  # --- 1. Extract predictor variable names from model formula -----------
  fit       <- workflows::extract_fit_parsnip(fit_object$final_model)
  variables <- setdiff(
    all.vars(fit$fit$formula),
    c("presence", "count", "x", "y")
  )

  if (length(variables) == 0L) {
    stop("No predictor variables found in the model inside 'fit_object'.")
  }

  # --- 2. Extract variable importance -----------------------------------
  weights <- .h3sdm_extract_weights(fit_object$final_model, variables, verbose)

  # --- 3. Prepare training matrix ---------------------------------------
  train_df <- sf::st_drop_geometry(train)
  X        <- as.matrix(train_df[, variables, drop = FALSE])
  n        <- nrow(X)
  p        <- ncol(X)

  if (n < 2L) stop("'train' must have at least 2 observations.")

  # --- 4. Z-score scaling -----------------------------------------------
  center        <- colMeans(X, na.rm = TRUE)
  std           <- apply(X, 2, stats::sd, na.rm = TRUE)
  std[std == 0] <- 1
  X_sc          <- sweep(sweep(X, 2, center, "-"), 2, std, "/")

  # --- 5. Apply weights -------------------------------------------------
  w   <- .h3sdm_parse_weights(weights, variables, p)
  X_w <- sweep(X_sc, 2, w, "*")

  # --- 6. Mean nearest-neighbor distance among training points ----------
  if (verbose) {
    message("h3sdm_aoa: computing pairwise training distances (n = ", n, ")...")
  }

  trainDist_avrgmean <- mean(.h3sdm_knn1_dist_excl(X_w))

  if (trainDist_avrgmean == 0) {
    stop(
      "trainDist_avrgmean is 0. Check that predictor variables are not ",
      "constant or identical after scaling."
    )
  }

  # --- 7. Cross-validated training DI -----------------------------------
  if (verbose) message("h3sdm_aoa: computing cross-validated DI...")

  cv_folds <- .h3sdm_extract_folds(cv, n)

  fold_ids <- unique(cv_folds)
  train_di <- rep(NA_real_, n)

  for (fold in fold_ids) {
    test_idx  <- which(cv_folds == fold)
    train_idx <- which(cv_folds != fold)
    if (length(train_idx) == 0L) next
    d <- .h3sdm_knn1_dist(
      X_w[train_idx, , drop = FALSE],
      X_w[test_idx,  , drop = FALSE]
    )
    train_di[test_idx] <- d / trainDist_avrgmean
  }

  # --- 8. AOA threshold -------------------------------------------------
  threshold <- .h3sdm_aoa_threshold(train_di)

  if (verbose) message("h3sdm_aoa: AOA threshold = ", round(threshold, 4))

  # --- 9. Compute DI for newdata ----------------------------------------
  is_sf <- inherits(newdata, "sf")
  if (is_sf) {
    geom    <- sf::st_geometry(newdata)
    newdata <- sf::st_drop_geometry(newdata)
  }

  X_new    <- as.matrix(newdata[, variables, drop = FALSE])
  X_new_sc <- sweep(sweep(X_new, 2, center, "-"), 2, std, "/")
  X_new_w  <- sweep(X_new_sc, 2, w, "*")
  m        <- nrow(X_new_w)

  if (verbose) message("h3sdm_aoa: computing DI for ", m, " observations...")

  di       <- .h3sdm_knn1_dist(X_w, X_new_w) / trainDist_avrgmean
  aoa_mask <- as.integer(di <= threshold)

  if (verbose) {
    n_in <- sum(aoa_mask)
    message(sprintf(
      "h3sdm_aoa: inside AOA = %d/%d (%.1f%%)  |  outside AOA = %d/%d (%.1f%%)",
      n_in, m, 100 * n_in / m, m - n_in, m, 100 * (m - n_in) / m
    ))
  }

  newdata[["DI"]]  <- di
  newdata[["AOA"]] <- aoa_mask

  if (is_sf) newdata <- sf::st_sf(newdata, geometry = geom)

  newdata
}


# ---------------------------------------------------------------
# Internal helpers (not exported)
# ---------------------------------------------------------------

# Extract variable importance from a fitted tidymodels workflow.
# Returns NULL if importances cannot be extracted (e.g. GAM).
.h3sdm_extract_weights <- function(final_model, variables, verbose) {
  tryCatch({
    fit    <- workflows::extract_fit_parsnip(final_model)
    engine <- fit$spec$engine
    if (engine %in% c("ranger", "xgboost")) {
      imp <- vip::vi(fit$fit)
      w   <- stats::setNames(imp$Importance, imp$Variable)
      w   <- w[variables]
      if (verbose) message("h3sdm_aoa: variable importance extracted from '", engine, "'.")
      return(w)
    }
    if (verbose) {
      message("h3sdm_aoa: importance not available for '", engine, "'. Using equal weights.")
    }
    NULL
  }, error = function(e) {
    if (verbose) message("h3sdm_aoa: could not extract importance. Using equal weights.")
    NULL
  })
}

# Extract fold assignments from an rset object (spatialsample / rsample).
# Returns a LOO vector if cv is NULL.
.h3sdm_extract_folds <- function(cv, n) {
  if (is.null(cv)) return(seq_len(n))
  folds <- rep(NA_integer_, n)
  for (i in seq_along(cv$splits)) {
    test_rows      <- rsample::complement(cv$splits[[i]])
    folds[test_rows] <- i
  }
  if (anyNA(folds)) folds[is.na(folds)] <- 0L
  folds
}

# Normalize a weight vector to sum to one.
.h3sdm_parse_weights <- function(weights, variables, p) {
  if (is.null(weights)) return(rep(1, p))
  w     <- if (!is.null(names(weights))) weights[variables] else weights
  w     <- as.numeric(w)
  if (length(w) != p) {
    stop(
      "Length of 'weights' (", length(w), ") does not match the ",
      "number of predictor variables (", p, ")."
    )
  }
  total <- sum(w, na.rm = TRUE)
  if (total > 0) w / total else w
}

# Euclidean distance from each row of X_query to its nearest neighbor
# in X_ref. Returns a numeric vector of length nrow(X_query).
.h3sdm_knn1_dist <- function(X_ref, X_query) {
  apply(X_query, 1L, function(q) {
    min(sqrt(rowSums(sweep(X_ref, 2L, q, "-")^2)))
  })
}

# Euclidean distance from each training point to its nearest neighbor,
# excluding itself. Returns a numeric vector of length nrow(X).
.h3sdm_knn1_dist_excl <- function(X) {
  n <- nrow(X)
  vapply(seq_len(n), function(i) {
    min(sqrt(rowSums(sweep(X[-i, , drop = FALSE], 2L, X[i, ], "-")^2)))
  }, numeric(1L))
}

# AOA threshold: max DI after removing Tukey outliers (Q3 + 1.5 * IQR).
.h3sdm_aoa_threshold <- function(di) {
  q1          <- stats::quantile(di, 0.25, na.rm = TRUE)
  q3          <- stats::quantile(di, 0.75, na.rm = TRUE)
  upper_fence <- q3 + 1.5 * (q3 - q1)
  clean       <- di[!is.na(di) & di <= upper_fence]
  if (length(clean) == 0L) clean <- di
  max(clean)
}
