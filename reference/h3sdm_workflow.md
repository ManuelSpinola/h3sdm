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
  describing the model type and engine to use for fitting.

- recipe:

  A `tidymodels` recipe object, typically created with
  [`h3sdm_recipe()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_recipe.md),
  which preprocesses the data and defines predictor/response roles.

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

## Examples

``` r
if (FALSE) { # \dontrun{
library(parsnip)
# Example: Create a tidymodels workflow for H3-based species distribution modeling

# Step 1: Define model specification
my_model_spec <- logistic_reg() %>%
  set_mode("classification") %>%
  set_engine("glm")

# Step 2: Create recipe
my_recipe <- h3sdm_recipe(combined_data)

# Step 3: Combine into workflow
sdm_wf <- h3sdm_workflow(model_spec = my_model_spec, sdm_recipe = my_recipe)
} # }
```
