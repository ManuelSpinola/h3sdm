#' @name h3sdm_workflows
#' @title Create multiple tidymodels workflows for H3-based SDMs
#' @description
#' Takes a named list of `tidymodels` model specifications and a prepared recipe,
#' returning a list of workflows ready for fitting or cross-validation. Useful for
#' comparing multiple models for species distribution modeling with H3 hexagonal grids.
#'
#' @param model_specs A named list of `tidymodels` model specifications
#'   (e.g., `logistic_reg()`, `rand_forest()`, `boost_tree()`).
#' @param sdm_recipe A `tidymodels` recipe object, typically created with `h3sdm_recipe()`.
#'
#' @return A named list of `workflow` objects, one per model specification.
#'
#' @examples
#' \dontrun{
#' # Create model specifications
#' mod_log <- logistic_reg() %>% set_mode("classification") %>% set_engine("glm")
#' mod_rf  <- rand_forest()   %>% set_mode("classification") %>% set_engine("ranger")
#'
#' model_list <- list(logistic = mod_log, rf = mod_rf)
#'
#' # Prepare recipe
#' my_recipe <- h3sdm_recipe(combined_data)
#'
#' # Create multiple workflows
#' workflows <- h3sdm_workflows(model_specs = model_list, sdm_recipe = my_recipe)
#' }
#'
#' @importFrom purrr imap
#' @importFrom workflows workflow add_model add_recipe
#' @export

h3sdm_workflows <- function(model_specs, sdm_recipe) {
  if (!is.list(model_specs)) stop("model_specs debe ser una lista de <model_spec>")
  purrr::imap(model_specs, function(mod, nm) {
    workflows::workflow() %>%
      workflows::add_model(mod) %>%
      workflows::add_recipe(sdm_recipe)
  })
}
