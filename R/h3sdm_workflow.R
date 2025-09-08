#' @name h3sdm_workflow
#' @title Create a tidymodels workflow for H3-based SDMs
#' @description
#' Combines a model specification and a prepared recipe into a single `tidymodels` workflow
#' suitable for species distribution modeling with H3 hexagonal grids.
#'
#' @param model_spec A `tidymodels` model specification (e.g., `logistic_reg()`, `rand_forest()`, or `boost_tree()`).
#' @param sdm_recipe A `tidymodels` recipe object, typically created with `h3sdm_recipe()`.
#'
#' @return A `workflow` object ready to be used for model fitting with `fit()` or cross-validation.
#'
#' @examples
#' \dontrun{
#' library(parsnip)
#' # Create model specification
#' my_model_spec <- logistic_reg() %>% set_mode("classification") %>% set_engine("glm")
#'
#' # Prepare recipe
#' my_recipe <- h3sdm_recipe(combined_data)
#'
#' # Create workflow
#' sdm_wf <- h3sdm_workflow(model_spec = my_model_spec, sdm_recipe = my_recipe)
#' }
#'
#' @importFrom workflows workflow add_model add_recipe
#' @export

h3sdm_workflow <- function(model_spec, sdm_recipe) {
  if (!inherits(model_spec, "model_spec")) stop("model_spec debe ser un objeto <model_spec>")
  wf <- workflows::workflow() %>%
    workflows::add_model(model_spec) %>%
    workflows::add_recipe(sdm_recipe)
  return(wf)
}

