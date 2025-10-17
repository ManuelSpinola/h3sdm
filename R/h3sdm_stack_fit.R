#' @name h3sdm_stack_fit
#'
#' @title Creates and fully fits an ensemble model (Stack).
#'
#' @description
#' This function combines the process of creating the model stack, optimizing the
#' weights (\code{blend_predictions}), and fitting the base models to the complete
#' training set (\code{fit_members()}) into a single step.
#'
#' \strong{Warning:} It does not follow the canonical tidymodels flow but is convenient.
#' It requires that the fitting results were generated using \code{h3sdm_fit_model(..., for_stacking = TRUE)}.
#'
#' @param ... List objects that are the result of \code{h3sdm_fit_model()}.
#'   Each object must contain the \code{cv_model} element (result of fit_resamples).
#' @param non_negative Logical. If \code{TRUE} (default), forces the candidate
#'   model weights to be non-negative.
#' @param metric The metric used to optimize the combination of weights.
#'
#' @return A list containing two elements: \code{blended_model} (the stack after blending)
#'   and \code{final_model} (a fully \code{fitted} \code{model_stack} object).
#'   The \code{final_model} is ready for direct prediction with \code{predict()}.
#'
#' @family h3sdm_tools
#' @export
#'
#' @importFrom stacks stacks add_candidates blend_predictions fit_members

h3sdm_stack_fit <- function(..., non_negative = TRUE, metric = NULL) {

  # 1. Get the candidates (pure tune_results objects are expected)
  candidate_list <- list(...)

  if (length(candidate_list) < 2) {
    stop("At least two models are required to create a stack.")
  }

  # 2. Assign NAMES to the arguments (CRUCIAL for add_candidates)
  nombres <- names(candidate_list)
  if (is.null(nombres) || any(nombres == "")) {
    nombres_originales <- as.character(substitute(list(...)))[-1]
    names(candidate_list) <- nombres_originales
  }

  # 3. Initialize the stack and add candidates (Logic that DOES work)
  primer_nombre <- names(candidate_list)[1]
  primer_candidato <- candidate_list[[1]]

  model_stack <- stacks::stacks() %>%
    stacks::add_candidates(primer_candidato, name = primer_nombre)

  # 4. Add the rest of the candidates one by one
  if (length(candidate_list) > 1) {
    for (i in 2:length(candidate_list)) {
      nombre_actual <- names(candidate_list)[i]
      candidato_actual <- candidate_list[[i]]

      model_stack <- model_stack %>%
        stacks::add_candidates(candidato_actual, name = nombre_actual)
    }
  }

  # 5. Train the weights (Blending)
  ensemble_model_blended <- model_stack %>%
    stacks::blend_predictions(
      non_negative = non_negative,
      metric = metric
    )

  # 6. Fit the Members
  final_ensemble_model <- ensemble_model_blended %>%
    stacks::fit_members()

  # 7. RETURN BOTH MODELS IN A LIST
  return(list(
    blended_model = ensemble_model_blended,
    final_model = final_ensemble_model
  ))
}
