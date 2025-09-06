#' @name h3sdm_workflow
#' @title Create a tidymodels workflow for H3-based SDMs
#' @description This function combines a model specification with a prepared recipe
#'   into a single tidymodels workflow.
#' @param model_spec A `tidymodels` model specification (e.g., `logistic_reg()`).
#' @param sdm_recipe A `tidymodels` recipe object, typically from `h3sdm_recipe()`.
#' @return A `workflow` object.
#' @importFrom workflows workflow
#' @examples
#' # Assuming a model spec and recipe are defined
#' # my_model_spec <- logistic_reg()
#' # my_recipe <- h3sdm_recipe(...)
#' # sdm_wf <- h3sdm_workflow(my_model_spec, my_recipe)
#' @export

h3sdm_workflow <- function(model_spec, sdm_recipe) {
  if (!inherits(model_spec, "model_spec")) stop("model_spec debe ser un objeto <model_spec>")
  wf <- workflows::workflow() %>%
    workflows::add_model(model_spec) %>%
    workflows::add_recipe(sdm_recipe)
  return(wf)
}

