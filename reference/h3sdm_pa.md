# Generate presence/pseudo-absence dataset stratified in environmental space

Combines presence hexagons with pseudo-absences sampled in environmental
space. Pseudo-absences are selected by clustering the environmental
conditions of hexagons without presence records using k-means, then
choosing the hexagon closest to each cluster centroid. This ensures
pseudo-absences cover the full range of environmental conditions
available in the AOI, reducing bias from spatially clustered occurrence
records.

## Usage

``` r
h3sdm_pa(pres_sf, predictors_sf, n_pseudoabs = 500, buffer_k = 1L)
```

## Arguments

- pres_sf:

  `sf` Presence hexagons returned by
  [`h3sdm_pres()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_pres.md).

- predictors_sf:

  `sf` Full hexagonal grid with extracted environmental variables,
  returned by
  [`h3sdm_predictors()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_predictors.md).

- n_pseudoabs:

  `integer` Number of pseudo-absence hexagons to sample. If larger than
  the number of available hexagons without presence, all available
  hexagons are used. Default is `500`.

- buffer_k:

  `integer` Number of H3 grid rings to exclude around each presence
  hexagon when building the pseudo-absence candidate pool. Hexagons
  within `buffer_k` rings of any presence are removed before sampling,
  preventing pseudo-absences from being placed in areas likely occupied
  but not yet recorded. Default is `1`. Set to `0` to disable.

## Value

`sf` object with columns:

- `h3_address`: H3 index of the hexagon.

- `presence`: factor with levels `"0"` (pseudo-absence) and `"1"`
  (presence).

- `geometry`: MULTIPOLYGON of each hexagon.

## Details

The function scales all numeric predictor columns before clustering.
Non-numeric columns and columns with zero variance are excluded from
clustering. Pseudo-absences are selected as the hexagon nearest to each
k-means centroid in scaled environmental space (Euclidean distance).

This function is designed to be used after
[`h3sdm_pres()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_pres.md)
and
[`h3sdm_predictors()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_predictors.md)
in the following workflow:


    pres        <- h3sdm_pres("Species name", aoi_sf, res = 7)
    num_vars    <- h3sdm_extract_num(raster_stack, grid)
    predictors  <- h3sdm_predictors(num_vars)
    pa          <- h3sdm_pa(pres, predictors, n_pseudoabs = 500)

## Examples

``` r
if (FALSE) { # \dontrun{
data(cr_outline_c, package = "h3sdm")
pres       <- h3sdm_pres("Agalychnis callidryas", cr_outline_c, res = 7)
grid       <- h3sdm_get_grid(cr_outline_c, res = 7)
num_vars   <- h3sdm_extract_num(bio, grid)
predictors <- h3sdm_predictors(num_vars)
pa         <- h3sdm_pa(pres, predictors, n_pseudoabs = 300)
} # }
```
