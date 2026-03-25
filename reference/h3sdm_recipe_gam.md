# Creates a 'recipe' object for Generalized Additive Models (GAM) in SDM.

This function prepares an `sf` (Simple Features) object for use in a
Species Distribution Model (SDM) workflow with the 'mgcv' GAM engine
within the 'tidymodels' ecosystem.

The crucial step is extracting the coordinates (x, y) from the geometry
and assigning them the **predictor** role so they can be used in the
GAM's spatial smooth term (`s(x, y, bs = "tp")`). It also assigns
special roles to the 'presence' and 'h3_address' variables.

## Usage

``` r
h3sdm_recipe_gam(data)
```

## Arguments

- data:

  An `sf` (Simple Features) object containing the species
  presence/absence/abundance data, environmental variables (e.g.,
  bioclimatic), and the geometry (e.g., H3 centroids or points).

## Value

A `recipe` object of class `h3sdm_recipe_gam`, ready to be chained with
additional preprocessing steps (e.g., normalization).

## Details

**Assigned Roles:**

- `outcome`: "presence" (or the column containing the response
  variable).

- `id`: "h3_address" (cell identifier, not used for modeling).

- `predictor`: All other variables, including **x** and **y** for the
  GAM's smoothing function.

**Note on x and y:** The `x` and `y` coordinates are added to the
recipe's internal data frame and are defined as **predictor** to meet
the requirements of the `mgcv` engine.

## See also

Other h3sdm_tools:
[`h3sdm_stack_fit()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_stack_fit.md),
[`h3sdm_workflow_gam()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_workflow_gam.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming 'data_sf' is your sf object with 'presence' and 'h3_address'
# gam_rec <- h3sdm_recipe_gam(data_sf)
# Add normalization only to bio variables, excluding x and y
# final_rec <- gam_rec %>% step_normalize(starts_with("bio"))
} # }
```
