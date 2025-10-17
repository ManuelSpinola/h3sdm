#' @title Crea un ensemble de modelos (Stack) y ajusta sus pesos.
#'
#' @description
#' Esta función toma los resultados de ajuste de múltiples modelos (candidatos),
#' los combina en una pila (\code{data_stack}) y ajusta los pesos óptimos
#' (\code{blend_predictions}) para crear un modelo de ensemble.
#'
#' Esta función asume que los resultados de ajuste se generaron con
#' \code{h3sdm_fit_model(..., for_stacking = TRUE)}.
#'
#' @param ... Objetos de lista que son el resultado de \code{h3sdm_fit_model()}.
#'   Cada objeto de lista debe contener el elemento \code{cv_model} (resultado de
#'   \code{fit_resamples}). Se recomienda usar argumentos con nombre (ej., \code{rf = rf_fit}).
#' @param non_negative Lógico. Si es \code{TRUE} (por defecto), fuerza a que los
#'   pesos de los modelos candidatos sean no negativos (contribución positiva).
#' @param metric La métrica utilizada para optimizar la combinación de pesos
#'   durante el blending. Por defecto, usa la primera métrica definida en los
#'   candidatos.
#'
#' @return Un objeto \code{model_stack} ajustado con los pesos óptimos (\code{blended}).
#'   Está listo para el paso final de ajuste a los datos completos (\code{fit_members()}).
#'
#' @family h3sdm_tools
#' @export
#'
#' @importFrom stacks stacks add_candidates blend_predictions
#'
#' @examples
#' \dontrun{
#' # Asumiendo que 'rf_fit' y 'gam_fit' son los resultados de h3sdm_fit_model()
#' # ensemble_mod <- h3sdm_stack_blend(RF = rf_fit, GAM = gam_fit)
#'
#' # Paso final de ajuste (Fuera de la función)
#' # final_ensemble <- fit_members(ensemble_mod)
#' }

h3sdm_stack_blend <- function(..., non_negative = TRUE, metric = NULL) {

  # 1. Obtener los resultados de los argumentos (...)
  candidate_list <- list(...)

  if (length(candidate_list) < 2) {
    stop("Se requieren al menos dos resultados de h3sdm_fit_model para crear un stack.")
  }

  # 2. Inicializar la pila
  model_stack <- stacks::stacks()

  # 3. Añadir los candidatos
  for (name in names(candidate_list)) {
    fit_result <- candidate_list[[name]]

    # Validación básica y extracción de cv_model
    if (!inherits(fit_result, "list") || is.null(fit_result$cv_model)) {
      warning(paste("El argumento", name, "no es un resultado válido de h3sdm_fit_model. Omitiendo."))
      next
    }

    # Extraer el objeto cv_model (resultado de fit_resamples)
    cv_model_res <- fit_result$cv_model

    # Añadir al stack. Usamos el nombre del argumento para nombrar al candidato.
    model_stack <- model_stack %>%
      stacks::add_candidates(cv_model_res, name = name)
  }

  # Validación de que se añadieron candidatos válidos
  if (is.null(model_stack$candidate)) {
    stop("No se encontraron candidatos válidos (cv_model) para el stacking.")
  }

  # 4. Entrenar los pesos (Blending)
  ensemble_model <- model_stack %>%
    stacks::blend_predictions(
      non_negative = non_negative,
      metric = metric
    )

  return(ensemble_model)
}
