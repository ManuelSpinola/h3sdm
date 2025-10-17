#' @name h3sdm_workflow_gam
#'
#' @title Creates a tidymodels workflow for Generalized Additive Models (GAM).
#'
#' @description
#' This function constructs a \code{workflow} object by combining a GAM model
#' specification (\code{gen_additive_mod} with the \code{mgcv} engine) with either
#' a \code{recipe} object or an explicit model formula.
#'
#' It is optimized for Species Distribution Models (SDM) that use smooth splines,
#' ensuring that the specialized GAM formula (containing \code{s()} terms) is
#' correctly passed to the model, even when a recipe is provided for general
#' data preprocessing.
#'
#' @param gam_spec A \code{parsnip} model specification of type
#'   \code{gen_additive_mod()}, configured with \code{set_engine("mgcv")}.
#' @param recipe (Optional) A \code{recipes} package \code{recipe} object (e.g.,
#'   the output of \code{\link[h3sdm]{h3sdm_recipe_gam}}). Used for general data
#'   preprocessing like normalization or dummy variable creation.
#' @param formula (Optional) A \code{formula} object that defines the structure
#'   of the GAM, including smooth terms (e.g., \code{y ~ s(x1) + s(x, y)}).
#'   If provided alongside \code{recipe}, this formula overrides the recipe's
#'   implicit formula for the final model fit.
#'
#' @return A \code{workflow} object, ready for fitting with \code{fit()} or
#'   resampling with \code{fit_resamples()} or \code{tune_grid()}.
#'
#' @details
#' \strong{Formula Priority:}
#' \itemize{
#'   \item If \strong{only \code{recipe}} is provided, the workflow uses the
#'     recipe's implicit formula (e.g., \code{outcome ~ .}).
#'   \item If \strong{\code{recipe} and \code{formula}} are provided, the workflow
#'     uses the \code{recipe} for data preprocessing but explicitly passes the
#'     \code{formula} to the \code{mgcv} engine for fitting, enabling the use
#'     of specialized terms like \code{s(x, y)}.
#' }
#'
#' @family h3sdm_tools
#'
#' @importFrom workflows workflow add_model add_recipe
#' @examples
#' \dontrun{
#' library(parsnip)
#' # 1. Define the model specification
#' gam_spec <- gen_additive_mod() %>%
#'   set_engine("mgcv") %>%
#'   set_mode("classification")
#'
#' # 2. Define a specialized GAM formula
#' gam_formula <- presence ~ s(bio1) + s(x, y, bs = "tp")
#'
#' # 3. Define a base recipe (assuming 'data' exists)
#' # base_rec <- h3sdm_recipe_gam(data)
#'
#' # 4. Create the combined workflow
#' # h3sdm_wf <- h3sdm_workflow_gam(
#' #   gam_spec = gam_spec,
#' #   recipe = base_rec,
#' #   formula = gam_formula
#' # )
#' }
#'
#' @export

h3sdm_workflow_gam <- function(gam_spec, recipe = NULL, formula = NULL) {
  # Input validations
  if (!inherits(gam_spec, "model_spec")) stop("gam_spec must be a parsnip model specification")
  if (is.null(recipe) && is.null(formula)) stop("Either a formula or a recipe must be provided")
  if (!is.null(formula) && !inherits(formula, "formula")) stop("formula must be a formula object")

  wf <- workflow()

  # 1. Add the Recipe (if provided) for preprocessing
  if (!is.null(recipe)) {
    wf <- wf %>% add_recipe(recipe)
  }

  # 2. Add the Model, prioritizing the explicit GAM formula for fitting.
  if (!is.null(formula)) {
    # The explicit formula (e.g., with s() terms) is passed to the model argument
    wf <- wf %>% add_model(gam_spec, formula = formula)
  } else {
    # If no explicit formula, the model attempts to use the recipe's formula
    wf <- wf %>% add_model(gam_spec)
  }

  wf
}
