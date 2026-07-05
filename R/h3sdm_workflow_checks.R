# ---------------------------------------------------------------
# Internal helper (not exported)
# Shared by h3sdm_workflow() and h3sdm_workflows().
# ---------------------------------------------------------------

# Warn early if a 'ranger' model_spec was not configured with an
# importance mode. h3sdm_aoa() relies on native variable importance to
# weight the Area of Applicability; without it, AOA silently falls back
# to equal weights. Rather than only surfacing that at h3sdm_aoa() time
# (which may run in a later session, disconnected from the workflow
# definition), we warn as soon as the workflow is built.
#
# This only warns; it never modifies model_spec, so the user's engine
# configuration is always respected as written.
.h3sdm_check_ranger_importance <- function(model_spec, label = NULL) {
  if (!inherits(model_spec, "model_spec")) return(invisible(NULL))
  if (is.null(model_spec$engine) || model_spec$engine != "ranger") {
    return(invisible(NULL))
  }

  imp_arg <- model_spec$eng_args$importance
  imp_val <- if (is.null(imp_arg)) {
    NA_character_
  } else {
    tryCatch(rlang::eval_tidy(imp_arg), error = function(e) NA_character_)
  }

  valid_modes <- c("impurity", "impurity_corrected", "permutation")

  if (is.na(imp_val) || !imp_val %in% valid_modes) {
    prefix <- if (is.null(label)) "h3sdm_workflow" else paste0("h3sdm_workflow ('", label, "')")
    warning(
      prefix, ": this 'ranger' model spec was not set up with a variable ",
      "importance mode. h3sdm_aoa() weights the Area of Applicability by ",
      "native variable importance; without it, AOA will silently fall ",
      "back to equal weights for all predictors. To enable it, use:\n",
      "  set_engine(\"ranger\", importance = \"impurity\")\n",
      "(or \"impurity_corrected\" / \"permutation\"). This warning does not ",
      "affect model fitting; it only concerns downstream use with h3sdm_aoa().",
      call. = FALSE
    )
  }

  invisible(NULL)
}
