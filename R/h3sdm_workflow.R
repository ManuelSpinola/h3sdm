#' @name h3sdm_workflow
#' @title Create a tidymodels workflow for H3-based SDMs
#' @description
#' Combines a model specification and a prepared recipe into a single `tidymodels`
#' workflow. This workflow is suitable for species distribution modeling using H3
#' hexagonal grids and can be directly fitted or cross-validated.
#'
#' @param model_spec A `tidymodels` model specification (e.g., `logistic_reg()`,
#'   `rand_forest()`, or `boost_tree()`), describing the model type and engine
#'   to use for fitting. Use `set_mode("classification")` for presence/absence
#'   models and `set_mode("regression")` for count-based models (species richness,
#'   detections, or individuals).
#' @param recipe A `tidymodels` recipe object, typically created with
#'   `h3sdm_recipe()`, which preprocesses the data and defines predictor/response
#'   roles. Use `response_col = "count"` in `h3sdm_recipe()` when working with
#'   count data.
#'
#' @details
#' The function creates a `workflow` that combines preprocessing and modeling
#' steps. This encapsulation allows consistent model training and evaluation
#' with `tidymodels` functions like `fit()` or `fit_resamples()`, and is
#' particularly useful when applying multiple models in parallel.
#'
#' \strong{Choosing the model mode:}
#' \itemize{
#'   \item For \strong{presence/absence} data: use \code{set_mode("classification")}.
#'   \item For \strong{count} data (species richness, detections, individuals):
#'     use \code{set_mode("regression")}.
#' }
#'
#' @return A `workflow` object ready to be used for model fitting with `fit()`
#'   or cross-validation.
#'
#' @examples
#' \dontrun{
#' library(parsnip)
#'
#' # --- Presence/absence model ---
#' rf_spec_pa <- rand_forest() %>%
#'   set_engine("ranger") %>%
#'   set_mode("classification")
#'
#' rec_pa <- h3sdm_recipe(combined_data)
#'
#' wf_pa <- h3sdm_workflow(model_spec = rf_spec_pa, recipe = rec_pa)
#'
#' # --- Count-based model ---
#' rf_spec_count <- rand_forest() %>%
#'   set_engine("ranger") %>%
#'   set_mode("regression")
#'
#' rec_count <- h3sdm_recipe(combined_data, response_col = "count")
#'
#' wf_count <- h3sdm_workflow(model_spec = rf_spec_count, recipe = rec_count)
#' }
#'
#' @importFrom workflows workflow add_model add_recipe
#'
#' @export
h3sdm_workflow <- function(model_spec, recipe) {
  if (!inherits(model_spec, "model_spec")) {
    stop("model_spec must be a parsnip model specification")
  }
  wf <- workflows::workflow() %>%
    workflows::add_model(model_spec) %>%
    workflows::add_recipe(recipe)
  return(wf)
}
