# R/utils.R

#' Utilities for CRAN checks
#'
#' Imports and global variable declarations to avoid check NOTES.
#'
#' @name h3sdm_utils
#' @keywords internal
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @importFrom stats predict
NULL

# Declare global variables used in pipelines or tibbles
utils::globalVariables(c(
  ".metric", ".estimator", "accuracy", "roc_auc",
  "std_err", "conf_low", "conf_high", "f_meas",
  "kap", "sens", "spec", "prediction",
  "presence", "h3_address", "geometry",
  "value", "freq", "sum_coverage", "total_area_cell"
))
