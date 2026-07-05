# Create multiple tidymodels workflows for H3-based SDMs

Creates a list of tidymodels workflows from multiple model
specifications and a prepared recipe. This is useful for comparing
different modeling approaches in species distribution modeling using H3
hexagonal grids.

## Usage

``` r
h3sdm_workflows(model_specs, recipe)
```

## Arguments

- model_specs:

  A named list of `tidymodels` model specifications (e.g.,
  [`logistic_reg()`](https://parsnip.tidymodels.org/reference/logistic_reg.html),
  [`rand_forest()`](https://parsnip.tidymodels.org/reference/rand_forest.html),
  [`boost_tree()`](https://parsnip.tidymodels.org/reference/boost_tree.html)),
  where each element specifies a different modeling approach. All
  specifications must use the same mode: `set_mode("classification")`
  for presence/absence models or `set_mode("regression")` for
  count-based models.

- recipe:

  A `tidymodels` recipe object, typically created with
  [`h3sdm_recipe()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_recipe.md),
  which prepares and preprocesses the data for modeling. Use
  `response_col = "count"` in
  [`h3sdm_recipe()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_recipe.md)
  when working with count data.

## Value

A named list of `workflow` objects, one per model specification.

## Details

This function automates the creation of workflows for multiple model
specifications. Each workflow combines the same preprocessing steps
(recipe) with a different modeling method, facilitating systematic
comparison of models.

**Choosing the model mode:**

- For **presence/absence** data: use `set_mode("classification")` for
  all model specifications.

- For **count** data (species richness, detections, individuals): use
  `set_mode("regression")` for all model specifications.

**Variable importance for `ranger` models:** any `model_spec` in
`model_specs` that uses the `"ranger"` engine without an importance mode
(i.e. without `set_engine("ranger", importance = "impurity")`,
`"impurity_corrected"`, or `"permutation"`) triggers a warning naming
that list element. This does not affect model fitting, but
[`h3sdm_aoa()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_aoa.md)
relies on native variable importance to weight the Area of
Applicability, and will silently fall back to equal weights for that
model's predictors if importance was not configured here.

## Examples

``` r
if (FALSE) { # \dontrun{
library(parsnip)

# --- Presence/absence models ---
# 'importance = "impurity"' on the ranger spec is recommended so that
# h3sdm_aoa() can weight the Area of Applicability by native importance.
rf_spec_pa <- rand_forest() %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")

glm_spec_pa <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")

specs_pa <- list(rf = rf_spec_pa, glm = glm_spec_pa)

rec_pa <- h3sdm_recipe(combined_data)

wfs_pa <- h3sdm_workflows(model_specs = specs_pa, recipe = rec_pa)

# --- Count-based models ---
rf_spec_count <- rand_forest() %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("regression")

xgb_spec_count <- boost_tree() %>%
  set_engine("xgboost") %>%
  set_mode("regression")

specs_count <- list(rf = rf_spec_count, xgb = xgb_spec_count)

rec_count <- h3sdm_recipe(combined_data, response_col = "count")

wfs_count <- h3sdm_workflows(model_specs = specs_count, recipe = rec_count)
} # }
```
