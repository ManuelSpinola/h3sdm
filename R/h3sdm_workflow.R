#' @name h3sdm_workflow
#' @title Create a tidymodels workflow for H3-based SDMs
#' @description
#' Combines a model specification and a prepared recipe into a single `tidymodels` workflow.
#' This workflow is suitable for species distribution modeling using H3 hexagonal grids
#' and can be directly fitted or cross-validated.
#'
#' @param model_spec A `tidymodels` model specification (e.g., `logistic_reg()`, `rand_forest()`, or `boost_tree()`),
#'   describing the model type and engine to use for fitting.
#' @param recipe A `tidymodels` recipe object, typically created with `h3sdm_recipe()`,
#'   which preprocesses the data and defines predictor/response roles.
#'
#' @details
#' The function creates a `workflow` that combines preprocessing and modeling
#' steps. This encapsulation allows consistent model training and evaluation
#' with `tidymodels` functions like `fit()` or `fit_resamples()`, and is
#' particularly useful when applying multiple models in parallel.
#'
#' @return A `workflow` object ready to be used for model fitting with `fit()` or cross-validation.
#'
#' @examples
#' \dontrun{
#' library(parsnip)
#' # Example: Create a tidymodels workflow for H3-based species distribution modeling
#'
#' # Step 1: Define model specification
#' my_model_spec <- logistic_reg() %>%
#'   set_mode("classification") %>%
#'   set_engine("glm")
#'
#' # Step 2: Create recipe
#' my_recipe <- h3sdm_recipe(combined_data)
#'
#' # Step 3: Combine into workflow
#' sdm_wf <- h3sdm_workflow(model_spec = my_model_spec, sdm_recipe = my_recipe)
#' }
#'
#' @importFrom workflows workflow add_model add_recipe
#'
#' @export

h3sdm_workflow <- function(model_spec, recipe) {
  if (!inherits(model_spec, "model_spec")) stop("model_spec debe ser un objeto <model_spec>")
  wf <- workflows::workflow() %>%
    workflows::add_model(model_spec) %>%
    workflows::add_recipe(sdm_recipe)
  return(wf)
}

