#' @name h3sdm_recipe_gam
#'
#' @title Creates a 'recipe' object for Generalized Additive Models (GAM) in SDM.
#'
#' @description
#' This function prepares an \code{sf} (Simple Features) object for use in a
#' Species Distribution Model (SDM) workflow with the 'mgcv' GAM engine
#' within the 'tidymodels' ecosystem.
#'
#' The crucial step is extracting the coordinates (x, y) from the geometry and
#' assigning them the **predictor** role so they can be used in the GAM's
#' spatial smooth term (\code{s(x, y, bs = "tp")}). It also assigns special
#' roles to the 'presence' and 'h3_address' variables.
#'
#' @param data An \code{sf} (Simple Features) object containing the species
#'   presence/absence/abundance data, environmental variables (e.g., bioclimatic),
#'   and the geometry (e.g., H3 centroids or points).
#'
#' @return A \code{recipe} object of class \code{h3sdm_recipe_gam},
#'   ready to be chained with additional preprocessing steps (e.g., normalization).
#'
#' @details
#' \strong{Assigned Roles:}
#' \itemize{
#'   \item \code{outcome}: "presence" (or the column containing the response variable).
#'   \item \code{id}: "h3_address" (cell identifier, not used for modeling).
#'   \item \code{predictor}: All other variables, including **x** and **y**
#'     for the GAM's smoothing function.
#' }
#'
#' \strong{Note on x and y:} The \code{x} and \code{y} coordinates are added to the
#' recipe's internal data frame and are defined as **predictor** to meet the
#' requirements of the \code{mgcv} engine.
#'
#' @family h3sdm_tools
#' @importFrom sf st_centroid st_geometry st_coordinates st_drop_geometry
#' @importFrom dplyr mutate
#' @importFrom recipes recipe
#' @examples
#' \dontrun{
#' # Assuming 'data_sf' is your sf object with 'presence' and 'h3_address'
#' # gam_rec <- h3sdm_recipe_gam(data_sf)
#' # Add normalization only to bio variables, excluding x and y
#' # final_rec <- gam_rec %>% step_normalize(starts_with("bio"))
#' }
#'
#' @export
h3sdm_recipe_gam <- function(data) {
  if (!inherits(data, "sf")) stop("data must be an sf object.")

  # 1. Coordinate Extraction and Preparation
  centroids <- sf::st_centroid(sf::st_geometry(data))
  coords <- sf::st_coordinates(centroids)

  data_no_geom <- data %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(
      x = coords[, 1],
      y = coords[, 2]
    )

  # 2. Role Definition (Key Adjustment for GAM)
  all_vars <- names(data_no_geom)
  roles <- rep("predictor", length(all_vars))

  # Specialized roles:
  roles[all_vars == "presence"] <- "outcome"
  roles[all_vars == "h3_address"] <- "id"
  # NOTE: x and y are kept as 'predictor' by default, which is correct for mgcv.

  # 3. Base Recipe Creation
  rec <- recipes::recipe(
    x = data_no_geom,
    vars = all_vars,
    roles = roles,
    strings_as_factors = FALSE
  )

  class(rec) <- c("h3sdm_recipe_gam", class(rec))
  return(rec)
}
