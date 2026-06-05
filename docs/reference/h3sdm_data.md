# Combine species and environmental data for SDMs using H3 grids

Combines species presence–absence data with environmental predictors. It
also calculates centroid coordinates (x and y) for each hexagon grid
cell.

## Usage

``` r
h3sdm_data(pa_sf, predictors_sf)
```

## Arguments

- pa_sf:

  An `sf` object from
  [`h3sdm_pa()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_pa.md)
  containing species presence–absence data.

- predictors_sf:

  An `sf` object from
  [`h3sdm_predictors()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_predictors.md)
  containing environmental predictors.

## Value

An `sf` object containing species presence–absence, environmental
predictor variables, and centroid coordinates for each hexagon cell.

## Examples

``` r
if (FALSE) { # \dontrun{
my_species_pa <- h3sdm_pa("Panthera onca", res = 6)
my_predictors <- h3sdm_predictors(my_species_pa)
combined_data <- h3sdm_data(my_species_pa, my_predictors)
} # }
```
