#' @title Ajusta un workflow SDM a los datos usando resampling y prepara para stacking.
#'
#' @description
#' Ajusta un workflow de Modelos de Distribución de Especies (SDM) a los datos de
#' resampling (cross-validation). Esta función es el paso principal de entrenamiento
#' y opcionalmente configura los resultados para ser usados con el paquete 'stacks'.
#'
#' @param sdm_workflow Un objeto 'workflow' de tidymodels (ej., el GAM o Random Forest).
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


h3sdm_fit_model <- function(sdm_workflow, data_split, presence_data = NULL,
                            truth_col = "presence", pred_col = ".pred_1",
                            for_stacking = FALSE) {

  sdm_metrics <- yardstick::metric_set(roc_auc, accuracy, sens, spec, f_meas, kap)

  # Configuración de control dinámica para Stacking
  if (for_stacking) {
    # Control especial requerido por el paquete stacks
    resamples_control <- stacks::control_stack_resamples()
  } else {
    # Control estándar, solo guarda las predicciones
    resamples_control <- tune::control_resamples(save_pred = TRUE)
  }

  # Cross-validation
  cv_model <- tune::fit_resamples(
    object    = sdm_workflow,
    resamples = data_split,
    metrics   = sdm_metrics,
    control   = resamples_control # Usando el control ajustado
  )

  # Modelo final sobre los datos de entrenamiento del primer split
  # Nota: Usar el primer split como sustituto de los 'datos de entrenamiento completos'
  final_model <- workflows::fit(sdm_workflow, data = rsample::analysis(data_split$splits[[1]]))

  # Convertir sf a data.frame si es necesario para el cálculo de métricas
  if (!is.null(presence_data) && inherits(presence_data, "sf")) {
    presence_data <- sf::st_drop_geometry(presence_data)
  }

  # Métricas extendidas
  final_metrics <- NULL
  if (!is.null(presence_data)) {
    # Asume que h3sdm_eval_metrics está definida en tu paquete
    final_metrics <- h3sdm_eval_metrics(
      fitted_model  = cv_model,
      presence_data = presence_data,
      truth_col     = truth_col,
      pred_col      = pred_col
    )
  }

  return(list(
    cv_model    = cv_model,
    final_model = final_model,
    metrics     = final_metrics
  ))
}
