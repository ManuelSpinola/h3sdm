# Create a tidymodels recipe for H3-based SDMs

Prepares an `sf` object with H3 hexagonal data for modeling with the
`tidymodels` ecosystem. Extracts centroid coordinates, assigns
appropriate roles to the variables automatically, and returns a
ready-to-use recipe for modeling species distributions.

## Usage

``` r
h3sdm_recipe(data)
```

## Arguments

- data:

  An `sf` object, typically the output of
  [`h3sdm_data()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_data.md),
  including species presence-absence, H3 addresses, and environmental
  predictors. The geometry must be of type `MULTIPOLYGON`.

## Value

A `tidymodels` recipe object (class `"h3sdm_recipe"`) ready for
modeling.

## Details

This function prepares spatial H3 grid data for species distribution
modeling:

- Extracts centroid coordinates (`x` and `y`) from MULTIPOLYGON
  geometries using sf functions.

- Removes the geometry column to create a purely tabular dataset for
  tidymodels.

- Assigns roles to columns:

  - `presence` → `"outcome"` (target variable)

  - `h3_address` → `"id"` (used for joining predictions later)

  - `x` and `y` → `"spatial_predictor"`

- All other columns are assigned `"predictor"` role.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example: Prepare H3 hexagonal SDM data for modeling
# `combined_data` is typically the output of h3sdm_data()
sdm_recipe <- h3sdm_recipe(combined_data)
sdm_recipe  # inspect the recipe object
} # }
```
