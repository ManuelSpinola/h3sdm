#' @name h3sdm_workflows
#' @title Create multiple tidymodels workflows for H3-based SDMs
#' @description
#' Creates a list of tidymodels workflows from multiple model specifications
#' and a prepared recipe. This is useful for comparing different modeling
#' approaches in species distribution modeling using H3 hexagonal grids.
#'
#' @param model_specs A named list of `tidymodels` model specifications
#'   (e.g., `logistic_reg()`, `rand_forest()`, `boost_tree()`), where each
#'   element specifies a different modeling approach. All specifications must
#'   use the same mode: `set_mode("classification")` for presence/absence models
#'   or `set_mode("regression")` for count-based models.
#' @param recipe A `tidymodels` recipe object, typically created with
#'   `h3sdm_recipe()`, which prepares and preprocesses the data for modeling.
#'   Use `response_col = "count"` in `h3sdm_recipe()` when working with
#'   count data.
#'
#' @details
#' This function automates the creation of workflows for multiple model
#' specifications. Each workflow combines the same preprocessing steps (recipe)
#' with a different modeling method, facilitating systematic comparison of models.
#'
#' \strong{Choosing the model mode:}
#' \itemize{
#'   \item For \strong{presence/absence} data: use \code{set_mode("classification")}
#'     for all model specifications.
#'   \item For \strong{count} data (species richness, detections, individuals):
#'     use \code{set_mode("regression")} for all model specifications.
#' }
#'
#' \strong{Variable importance for `ranger` models:} any `model_spec` in
#' `model_specs` that uses the `"ranger"` engine without an importance mode
#' (i.e. without \code{set_engine("ranger", importance = "impurity")},
#' `"impurity_corrected"`, or `"permutation"`) triggers a warning naming
#' that list element. This does not affect model fitting, but
#' [h3sdm_aoa()] relies on native variable importance to weight the Area
#' of Applicability, and will silently fall back to equal weights for
#' that model's predictors if importance was not configured here.
#'
#' @return A named list of `workflow` objects, one per model specification.
#'
#' @examples
#' \dontrun{
#' library(parsnip)
#'
#' # --- Presence/absence models ---
#' # 'importance = "impurity"' on the ranger spec is recommended so that
#' # h3sdm_aoa() can weight the Area of Applicability by native importance.
#' rf_spec_pa <- rand_forest() %>%
#'   set_engine("ranger", importance = "impurity") %>%
#'   set_mode("classification")
#'
#' glm_spec_pa <- logistic_reg() %>%
#'   set_engine("glm") %>%
#'   set_mode("classification")
#'
#' specs_pa <- list(rf = rf_spec_pa, glm = glm_spec_pa)
#'
#' rec_pa <- h3sdm_recipe(combined_data)
#'
#' wfs_pa <- h3sdm_workflows(model_specs = specs_pa, recipe = rec_pa)
#'
#' # --- Count-based models ---
#' rf_spec_count <- rand_forest() %>%
#'   set_engine("ranger", importance = "impurity") %>%
#'   set_mode("regression")
#'
#' xgb_spec_count <- boost_tree() %>%
#'   set_engine("xgboost") %>%
#'   set_mode("regression")
#'
#' specs_count <- list(rf = rf_spec_count, xgb = xgb_spec_count)
#'
#' rec_count <- h3sdm_recipe(combined_data, response_col = "count")
#'
#' wfs_count <- h3sdm_workflows(model_specs = specs_count, recipe = rec_count)
#' }
#'
#' @importFrom purrr imap
#' @importFrom workflows workflow add_model add_recipe
#' @importFrom rlang eval_tidy
#'
#' @export
h3sdm_workflows <- function(model_specs, recipe) {
  if (!is.list(model_specs)) {
    stop("model_specs must be a named list of parsnip model specifications")
  }
  purrr::imap(model_specs, function(mod, nm) {
    .h3sdm_check_ranger_importance(mod, label = nm)

    wf <- workflows::workflow() %>%
      workflows::add_model(mod)
    if (!is.null(recipe)) {
      wf <- wf %>% workflows::add_recipe(recipe)
    }
    wf
  })
}
