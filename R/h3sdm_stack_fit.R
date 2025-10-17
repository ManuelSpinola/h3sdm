#' @title Crea y ajusta completamente un ensemble de modelos (Stack).
#'
#' @description
#' Esta función combina el proceso de crear la pila de modelos, optimizar los
#' pesos (\code{blend_predictions}), y ajustar los modelos base al conjunto de
#' entrenamiento completo (\code{fit_members()}) en un solo paso.
#'
#' \strong{Advertencia:} No sigue el flujo canónico de tidymodels, pero es conveniente.
#' Requiere que los resultados de ajuste se generaran con \code{h3sdm_fit_model(..., for_stacking = TRUE)}.
#'
#' @param ... Objetos de lista que son el resultado de \code{h3sdm_fit_model()}.
#'   Cada objeto debe contener el elemento \code{cv_model} (resultado de fit_resamples).
#' @param non_negative Lógico. Si es \code{TRUE} (por defecto), fuerza a que los
#'   pesos de los modelos candidatos sean no negativos.
#' @param metric La métrica utilizada para optimizar la combinación de pesos.
#'
#' @return Un objeto \code{model_stack} completamente ajustado (\code{fitted}).
#'   Está listo para hacer predicciones directamente con \code{predict()}.
#'
#' @family h3sdm_tools
#' @export
#'
#' @importFrom stacks stacks add_candidates blend_predictions fit_members

h3sdm_stack_fit <- function(..., non_negative = TRUE, metric = NULL) {

  # 1. Preparación y Validación
  candidate_list <- list(...)

  if (length(candidate_list) < 2) {
    stop("Se requieren al menos dos resultados de h3sdm_fit_model para crear un stack.")
  }

  model_stack <- stacks::stacks()

  # 2. Añadir los Candidatos (Mismo proceso que antes)
  for (name in names(candidate_list)) {
    fit_result <- candidate_list[[name]]

    if (!inherits(fit_result, "list") || is.null(fit_result$cv_model)) {
      warning(paste("El argumento", name, "no es un resultado válido de h3sdm_fit_model. Omitiendo."))
      next
    }

    cv_model_res <- fit_result$cv_model

    model_stack <- model_stack %>%
      stacks::add_candidates(cv_model_res, name = name)
  }

  if (is.null(model_stack$candidate)) {
    stop("No se encontraron candidatos válidos (cv_model) para el stacking.")
  }

  # 3. Entrenar los Pesos (Blending)
  ensemble_model_blended <- model_stack %>%
    stacks::blend_predictions(
      non_negative = non_negative,
      metric = metric
    )

  # 4. PASO EXTRA: Ajustar los Miembros (Fit Members)
  # Este es el paso que diferencia esta función de h3sdm_models_stack
  final_ensemble_model <- ensemble_model_blended %>%
    stacks::fit_members() # Ajuste final de los modelos base a los datos completos

  return(final_ensemble_model)
}
