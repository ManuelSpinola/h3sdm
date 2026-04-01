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
# \donttest{
  library(sf)
  library(recipes)
#> Loading required package: dplyr
#> 
#> Attaching package: ‘dplyr’
#> The following object is masked from ‘package:DALEX’:
#> 
#>     explain
#> The following objects are masked from ‘package:terra’:
#> 
#>     intersect, union
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
#> 
#> Attaching package: ‘recipes’
#> The following object is masked from ‘package:terra’:
#> 
#>     update
#> The following object is masked from ‘package:stats’:
#> 
#>     step

  # Create a simple sf object with presence/absence data
  # and simulated environmental variables
  set.seed(42)
  n <- 20

  pts <- sf::st_as_sf(
    data.frame(
      h3_address = paste0("hex_", seq_len(n)),
      presence   = sample(0:1, n, replace = TRUE),
      bio1_temp  = runif(n, 15, 30),
      bio12_precip = runif(n, 500, 3000)
    ),
    geometry = sf::st_sfc(
      lapply(seq_len(n), function(i) {
        sf::st_point(c(runif(1, -84.5, -83.5), runif(1, 9.5, 10.5)))
      }),
      crs = 4326
    )
  )

  # Create a GAM recipe with spatial coordinates as predictors
  gam_rec <- h3sdm_recipe_gam(pts)

  # Optionally add normalization to bioclimatic variables
  final_rec <- gam_rec |>
    recipes::step_normalize(recipes::starts_with("bio"))

  print(final_rec)
#> 
#> ── Recipe ─────────────────────────────────────────────────────────────
#> 
#> ── Inputs 
#> Number of variables by role
#> outcome:   1
#> predictor: 4
#> id:        1
#> 
#> ── Operations 
#> • Centering and scaling for: recipes::starts_with("bio")
# }
```
