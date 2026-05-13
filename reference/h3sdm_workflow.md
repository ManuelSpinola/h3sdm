# Create a tidymodels workflow for H3-based SDMs

Combines a model specification and a prepared recipe into a single
`tidymodels` workflow. This workflow is suitable for species
distribution modeling using H3 hexagonal grids and can be directly
fitted or cross-validated.

## Usage

``` r
h3sdm_workflow(model_spec, recipe)
```

## Arguments

- model_spec:

  A `tidymodels` model specification (e.g.,
  [`logistic_reg()`](https://parsnip.tidymodels.org/reference/logistic_reg.html),
  [`rand_forest()`](https://parsnip.tidymodels.org/reference/rand_forest.html),
  or
  [`boost_tree()`](https://parsnip.tidymodels.org/reference/boost_tree.html)),
  describing the model type and engine to use for fitting. Use
  `set_mode("classification")` for presence/absence models and
  `set_mode("regression")` for count-based models (species richness,
  detections, or individuals).

- recipe:

  A `tidymodels` recipe object, typically created with
  [`h3sdm_recipe()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_recipe.md),
  which preprocesses the data and defines predictor/response roles. Use
  `response_col = "count"` in
  [`h3sdm_recipe()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_recipe.md)
  when working with count data.

## Value

A `workflow` object ready to be used for model fitting with
[`fit()`](https://generics.r-lib.org/reference/fit.html) or
cross-validation.

## Details

The function creates a `workflow` that combines preprocessing and
modeling steps. This encapsulation allows consistent model training and
evaluation with `tidymodels` functions like
[`fit()`](https://generics.r-lib.org/reference/fit.html) or
`fit_resamples()`, and is particularly useful when applying multiple
models in parallel.

**Choosing the model mode:**

- For **presence/absence** data: use `set_mode("classification")`.

- For **count** data (species richness, detections, individuals): use
  `set_mode("regression")`.

## Examples

``` r
if (FALSE) { # \dontrun{
library(parsnip)

# --- Presence/absence model ---
rf_spec_pa <- rand_forest() %>%
  set_engine("ranger") %>%
  set_mode("classification")

rec_pa <- h3sdm_recipe(combined_data)

wf_pa <- h3sdm_workflow(model_spec = rf_spec_pa, recipe = rec_pa)

# --- Count-based model ---
rf_spec_count <- rand_forest() %>%
  set_engine("ranger") %>%
  set_mode("regression")

rec_count <- h3sdm_recipe(combined_data, response_col = "count")

wf_count <- h3sdm_workflow(model_spec = rf_spec_count, recipe = rec_count)
} # }
```
