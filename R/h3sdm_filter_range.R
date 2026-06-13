#' Filter predictions outside the univariate range of training data
#'
#' @description
#' Adds a `range_filter` column to `newdata` indicating whether each
#' observation falls within the univariate range of the training data
#' for all specified variables. This function is complementary to
#' [h3sdm_aoa()] and Mahalanobis distance filtering, as it detects
#' extrapolation at the margins of individual variables that multivariate
#' methods may not capture.
#'
#' @param newdata An `sf` object with predictions (output of [h3sdm_predict()]
#'   or [h3sdm_aoa()]).
#' @param train An `sf` object with the training data used to fit the model.
#' @param variables A character vector of variable names to check. Should
#'   match the covariates used in the model.
#'
#' @return The `newdata` object with an additional integer column `range_filter`:
#'   \describe{
#'     \item{1}{Observation is within the training range for all variables.}
#'     \item{0}{Observation is outside the training range for at least one variable.}
#'   }
#'
#' @details
#' This function implements univariate range filtering as a quality control
#' step for spatial predictions. It complements multivariate methods such as
#' the Area of Applicability (AOA) and Mahalanobis distance, which may not
#' detect extrapolation when a prediction point lies at the margin of a single
#' variable but within the multivariate space of the training data.
#'
#' The three methods detect different types of extrapolation:
#' \itemize{
#'   \item \strong{Range filter}: extrapolation in individual variables.
#'   \item \strong{Mahalanobis distance}: points far from the multivariate centroid.
#'   \item \strong{AOA}: combinations without analogues in training data.
#' }
#'
#' @examples
#' \dontrun{
#' aoa_result <- h3sdm_aoa(
#'   newdata    = prediccion_especies,
#'   train      = dat_modelo,
#'   fit_object = mod_gam,
#'   cv         = scv
#' )
#'
#' aoa_result <- h3sdm_filter_range(
#'   newdata   = aoa_result,
#'   train     = dat_modelo,
#'   variables = c("bio12", "bio17", "bio8", "class_prop_1", "class_prop_11")
#' )
#'
#' # Inspect filtered hexagons
#' table(aoa_result$range_filter)
#'
#' # Mask predictions outside range
#' aoa_result <- aoa_result |>
#'   dplyr::mutate(prediction = ifelse(range_filter == 0, NA, prediction))
#' }
#'
#' @export
h3sdm_filter_range <- function(newdata, train, variables) {

  # Input validation
  if (!inherits(newdata, "sf")) {
    cli::cli_abort("{.arg newdata} must be an {.cls sf} object.")
  }

  if (!inherits(train, "sf")) {
    cli::cli_abort("{.arg train} must be an {.cls sf} object.")
  }

  if (!is.character(variables) || length(variables) == 0) {
    cli::cli_abort("{.arg variables} must be a non-empty character vector.")
  }

  missing_newdata <- setdiff(variables, names(newdata))
  if (length(missing_newdata) > 0) {
    cli::cli_abort(
      "The following variables are not found in {.arg newdata}: {.val {missing_newdata}}"
    )
  }

  missing_train <- setdiff(variables, names(train))
  if (length(missing_train) > 0) {
    cli::cli_abort(
      "The following variables are not found in {.arg train}: {.val {missing_train}}"
    )
  }

  # Compute training ranges
  train_ranges <- train |>
    sf::st_drop_geometry() |>
    dplyr::summarise(dplyr::across(
      dplyr::all_of(variables),
      list(min = min, max = max),
      .names = "{.col}_{.fn}"
    ))

  # Build a logical mask: TRUE = within range for all variables
  within_range <- rep(TRUE, nrow(newdata))

  for (var in variables) {
    min_val <- train_ranges[[paste0(var, "_min")]]
    max_val <- train_ranges[[paste0(var, "_max")]]

    var_values <- sf::st_drop_geometry(newdata)[[var]]
    within_range <- within_range & (var_values >= min_val & var_values <= max_val)
  }

  # Add range_filter column (1 = within range, 0 = outside range)
  newdata <- newdata |>
    dplyr::mutate(range_filter = as.integer(within_range))

  n_outside <- sum(newdata$range_filter == 0)
  pct_outside <- round(n_outside / nrow(newdata) * 100, 1)

  cli::cli_inform(c(
    "v" = "Range filter applied over {length(variables)} variable{?s}.",
    "i" = "{n_outside} hexagon{?s} ({pct_outside}%) outside the training range."
  ))

  return(newdata)
}
