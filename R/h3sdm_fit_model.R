#' @title Ajusta un workflow SDM a los datos usando resampling y prepara para stacking.
#'
#' @description
#' Ajusta un workflow de Modelos de Distribución de Especies (SDM) a los datos de
#' resampling (cross-validation). Esta función es el paso principal de entrenamiento
#' y opcionalmente configura los resultados para ser usados con el paquete 'stacks'.
#'
#' @param workflow Un objeto 'workflow' de tidymodels (ej., el GAM o Random Forest).
#' @param data_split Un objeto 'rsplit' o 'rset' (ej. resultado de vfold_cv o spatial_block_cv).
#' @param presence_data (Opcional) Datos originales de presencia (usados para métricas extendidas).
#' @param truth_col Columna de la variable respuesta (por defecto, "presence").
#' @param pred_col Columna de predicción de la clase de interés (por defecto, ".pred_1").
#' @param for_stacking Lógico. Si es \code{TRUE}, usa \code{control_stack_resamples()}
#'   para guardar toda la información del workflow necesaria para el paquete 'stacks'.
#'   Si es \code{FALSE}, usa el control estándar con \code{save_pred = TRUE}.
#'
#' @return Una lista con tres elementos:
#' \itemize{
#'   \item \code{cv_model}: El resultado de \code{fit_resamples()}.
#'   \item \code{final_model}: El modelo ajustado a todo el conjunto de entrenamiento (primer split).
#'   \item \code{metrics}: Métricas de evaluación extendidas (si \code{presence_data} es provisto).
#' }
#' @export
#'
#' @importFrom yardstick metric_set roc_auc accuracy sens spec f_meas kap
#' @importFrom tune fit_resamples control_resamples
#' @importFrom stacks control_stack_resamples
#' @importFrom workflows fit
#' @importFrom rsample analysis
#' @importFrom sf st_drop_geometry


h3sdm_fit_model <- function(workflow, data_split, presence_data = NULL,
                            truth_col = "presence", pred_col = ".pred_1",
                            for_stacking = FALSE, ...) {

  sdm_metrics <- yardstick::metric_set(roc_auc, accuracy, sens, spec, f_meas, kap)

  # 1. Configuración de control dinámica
  if (for_stacking) {
    # Control requerido por el paquete stacks
    resamples_control <- stacks::control_stack_resamples()
  } else {
    # Control estándar: Guarda predicciones para evaluación
    resamples_control <- tune::control_resamples(save_pred = TRUE)
  }

  # 2. Cross-validation
  cv_model <- tune::fit_resamples(
    object    = workflow,
    resamples = data_split,
    metrics   = sdm_metrics,
    control   = resamples_control
  )

  # 3. Retorno para STACKING
  if (for_stacking) {
    # Devuelve el objeto cv_model PURO (¡solución para stacks!)
    return(cv_model)
  }

  # --- Flujo Normal (NO Stacking): Retorno de Lista Completa ---

  # Inicialización (FIX: evita el error 'object final_metrics not found')
  final_metrics <- NULL

  # 4. Ajuste del modelo final
  final_model <- workflows::fit(workflow, data = rsample::analysis(data_split$splits[[1]]))

  # 5. Cálculo de Métricas Finales
  if (!is.null(presence_data)) {
    # Asegura que la data no tenga geometría para el cálculo
    if (inherits(presence_data, "sf")) {
      presence_data <- sf::st_drop_geometry(presence_data)
    }

    # Asume que h3sdm_eval_metrics está definida y usa cv_model
    final_metrics <- h3sdm_eval_metrics(
      fitted_model  = cv_model,
      presence_data = presence_data,
      truth_col     = truth_col,
      pred_col      = pred_col
    )
  }

  # 6. Retornar la lista completa (para análisis/inspección)
  return(list(
    cv_model    = cv_model,
    final_model = final_model,
    metrics     = final_metrics
  ))
}
