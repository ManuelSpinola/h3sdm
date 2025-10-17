#' @name h3sdm_workflows
#' @title Create multiple tidymodels workflows for H3-based SDMs
#' @description
#' Creates a list of tidymodels workflows from multiple model specifications and a prepared recipe.
#' This is useful for comparing different modeling approaches in species distribution modeling
#' using H3 hexagonal grids. The returned workflows can be used for model fitting and resampling.
#'
#' @param model_specs A named list of `tidymodels` model specifications
#'   (e.g., `logistic_reg()`, `rand_forest()`, `boost_tree()`), where each element
#'   specifies a different modeling approach to be included in the workflow set.
#' @param recipe A `tidymodels` recipe object, typically created with `h3sdm_recipe()`,
#'   which prepares and preprocesses the data for modeling.
#'
#' @details
#' This function automates the creation of workflows for multiple model specifications.
#' Each workflow combines the same preprocessing steps (recipe) with a different
#' modeling method. This facilitates systematic comparison of models and is
#' especially useful in ensemble and stacking approaches.
#'
#' @return A named list of `workflow` objects, one per model specification.
#'
#' @examples
#' \dontrun{
#' # ... (examples are correct as is) ...
#' }
#'
#' @importFrom purrr imap
#' @importFrom workflows workflow add_model add_recipe
#'
#' @export
# Los argumentos deben ser consistentes con la documentación: model_specs
h3sdm_workflows <- function(model_specs, recipe = NULL) {

  # 1. Corrección de la validación: Usar 'model_specs'
  if (!is.list(model_specs)) stop("model_specs debe ser una lista de especificaciones de modelos parsnip")

  # 2. Corrección de purrr::imap: Usar 'model_specs'
  purrr::imap(model_specs, function(mod, nm) {
    wf <- workflows::workflow() %>% workflows::add_model(mod)

    # Agregar la receta a todos los modelos si se proporciona
    if (!is.null(recipe)) {
      wf <- wf %>% workflows::add_recipe(recipe)
    }

    wf
  })
}
