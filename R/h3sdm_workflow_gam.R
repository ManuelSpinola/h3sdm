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
#'   Use \code{set_mode("classification")} for presence/absence models and
#'   \code{set_mode("regression")} for count-based models.
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
#' \strong{Choosing the model mode and family:}
#' \itemize{
#'   \item For \strong{presence/absence} data: use \code{set_mode("classification")}.
#'     The \code{mgcv} engine uses \code{binomial()} family by default.
#'   \item For \strong{count} data (species richness, detections, individuals):
#'     use \code{set_mode("regression")} and specify
#'     \code{set_engine("mgcv", family = poisson())}.
#' }
#'
#' @family h3sdm_tools
#'
#' @importFrom workflows workflow add_model add_recipe
#'
#' @examples
#' \dontrun{
#' library(parsnip)
#'
#' # --- Presence/absence model (binomial) ---
#' gam_spec_pa <- gen_additive_mod() %>%
#'   set_engine("mgcv") %>%
#'   set_mode("classification")
#'
#' gam_formula_pa <- presence ~ s(bio1) + s(bio12) + s(x, y, bs = "tp")
#'
#' base_rec_pa <- h3sdm_recipe_gam(data)
#'
#' h3sdm_wf_pa <- h3sdm_workflow_gam(
#'   gam_spec = gam_spec_pa,
#'   recipe   = base_rec_pa,
#'   formula  = gam_formula_pa
#' )
#'
#' # --- Count-based model (Poisson) ---
#' gam_spec_count <- gen_additive_mod() %>%
#'   set_engine("mgcv", family = poisson()) %>%
#'   set_mode("regression")
#'
#' gam_formula_count <- count ~ s(bio1) + s(bio12) + s(x, y, bs = "tp")
#'
#' base_rec_count <- h3sdm_recipe_gam(data, response_col = "count")
#'
#' h3sdm_wf_count <- h3sdm_workflow_gam(
#'   gam_spec = gam_spec_count,
#'   recipe   = base_rec_count,
#'   formula  = gam_formula_count
#' )
#' }
#'
#' @export
h3sdm_workflow_gam <- function(gam_spec, recipe = NULL, formula = NULL) {

  # Input validations
  if (!inherits(gam_spec, "model_spec")) {
    stop("gam_spec must be a parsnip model specification")
  }
  if (is.null(recipe) && is.null(formula)) {
    stop("Either a formula or a recipe must be provided")
  }
  if (!is.null(formula) && !inherits(formula, "formula")) {
    stop("formula must be a formula object")
  }

  wf <- workflows::workflow()

  # 1. Add the recipe for preprocessing
  if (!is.null(recipe)) {
    wf <- wf %>% workflows::add_recipe(recipe)
  }

  # 2. Add the model, prioritizing the explicit GAM formula for fitting
  if (!is.null(formula)) {
    wf <- wf %>% workflows::add_model(gam_spec, formula = formula)
  } else {
    wf <- wf %>% workflows::add_model(gam_spec)
  }

  wf
}
