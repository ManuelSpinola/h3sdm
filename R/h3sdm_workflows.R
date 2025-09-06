

h3sdm_workflows <- function(model_specs, sdm_recipe) {
  if (!is.list(model_specs)) stop("model_specs debe ser una lista de <model_spec>")
  purrr::imap(model_specs, function(mod, nm) {
    workflows::workflow() %>%
      workflows::add_model(mod) %>%
      workflows::add_recipe(sdm_recipe)
  })
}
