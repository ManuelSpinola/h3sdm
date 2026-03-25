# Create multiple tidymodels workflows for H3-based SDMs

Creates a list of tidymodels workflows from multiple model
specifications and a prepared recipe. This is useful for comparing
different modeling approaches in species distribution modeling using H3
hexagonal grids. The returned workflows can be used for model fitting
and resampling.

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
  where each element specifies a different modeling approach to be
  included in the workflow set.

- recipe:

  A `tidymodels` recipe object, typically created with
  [`h3sdm_recipe()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_recipe.md),
  which prepares and preprocesses the data for modeling.

## Value

A named list of `workflow` objects, one per model specification.

## Details

This function automates the creation of workflows for multiple model
specifications. Each workflow combines the same preprocessing steps
(recipe) with a different modeling method. This facilitates systematic
comparison of models and is especially useful in ensemble and stacking
approaches.

## Examples

``` r
if (FALSE) { # \dontrun{
# ... (examples are correct as is) ...
} # }
```
