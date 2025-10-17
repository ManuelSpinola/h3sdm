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

  # 1. Obtener los candidatos (se esperan objetos tune_results puros)
  candidate_list <- list(...)

  if (length(candidate_list) < 2) {
    stop("Se requieren al menos dos modelos para crear un stack.")
  }

  # 2. Asignar NOMBRES a los argumentos (CRUCIAL para add_candidates)
  nombres <- names(candidate_list)
  if (is.null(nombres) || any(nombres == "")) {
    nombres_originales <- as.character(substitute(list(...)))[-1]
    names(candidate_list) <- nombres_originales
  }

  # 3. Inicializar el stack y añadir candidatos (Lógica que SÍ FUNCIONA)
  primer_nombre <- names(candidate_list)[1]
  primer_candidato <- candidate_list[[1]]

  model_stack <- stacks::stacks() %>%
    stacks::add_candidates(primer_candidato, name = primer_nombre)

  # 4. Añadir el resto de candidatos uno por uno
  if (length(candidate_list) > 1) {
    for (i in 2:length(candidate_list)) {
      nombre_actual <- names(candidate_list)[i]
      candidato_actual <- candidate_list[[i]]

      model_stack <- model_stack %>%
        stacks::add_candidates(candidato_actual, name = nombre_actual)
    }
  }

  # 5. Entrenar los pesos (Blending)
  ensemble_model_blended <- model_stack %>%
    stacks::blend_predictions(
      non_negative = non_negative,
      metric = metric
    )

  # 6. Ajustar los Miembros (Fit Members)
  final_ensemble_model <- ensemble_model_blended %>%
    stacks::fit_members()

  # 7. DEVOLVER AMBOS MODELOS EN UNA LISTA
  return(list(
    blended_model = ensemble_model_blended,
    final_model = final_ensemble_model
  ))
}
