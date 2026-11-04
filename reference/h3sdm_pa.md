# Generate presence/pseudo-absence dataset

Combines presence hexagons with pseudo-absences selected from hexagons
without presence records. Two selection methods are available:

- `"environmental"` (default): pseudo-absences are stratified in
  environmental space. Hexagons without presence are clustered with
  k-means on their environmental conditions, and the hexagon closest to
  each cluster centroid is selected. This ensures pseudo-absences cover
  the full range of environmental conditions available in the AOI,
  reducing bias from spatially clustered occurrence records.

- `"random"`: pseudo-absences are drawn at random from the candidate
  hexagons. When `n_pseudoabs` is equal to or larger than the number of
  candidates, all candidates are returned, so the user can apply their
  own sampling or balancing strategy afterwards.

## Usage

``` r
h3sdm_pa(
  pres_sf,
  predictors_sf,
  n_pseudoabs = 500,
  buffer_k = 1L,
  method = c("environmental", "random")
)
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

  `integer` Number of pseudo-absence hexagons to select. If equal to or
  larger than the number of available hexagons without presence, all
  available hexagons are used (no clustering or sampling is performed).
  Default is `500`.

- buffer_k:

  `integer` Number of H3 grid rings to exclude around each presence
  hexagon when building the pseudo-absence candidate pool. Hexagons
  within `buffer_k` rings of any presence are removed before selection,
  preventing pseudo-absences from being placed in areas likely occupied
  but not yet recorded. Default is `1`. Set to `0` to disable.

- method:

  `character` Pseudo-absence selection method: either `"environmental"`
  (default, k-means stratification in environmental space) or `"random"`
  (random sampling). See Description.

## Value

`sf` object with columns:

- `h3_address`: H3 index of the hexagon.

- `presence`: factor with levels `"0"` (pseudo-absence) and `"1"`
  (presence).

- `geometry`: MULTIPOLYGON of each hexagon.

## Details

For `method = "environmental"`, the function scales all numeric
predictor columns before clustering. Non-numeric columns and columns
with zero variance are excluded from clustering. Pseudo-absences are
selected as the hexagon nearest to each k-means centroid in scaled
environmental space (Euclidean distance). If the number of requested
pseudo-absences exceeds the number of distinct environmental conditions
among the candidates, the number of clusters is reduced to that number,
since k-means cannot create more clusters than distinct points. The
clustering uses a fixed internal seed so results are reproducible.

For `method = "random"`, sampling uses R's random number generator, so
call [`set.seed()`](https://rdrr.io/r/base/Random.html) beforehand for
reproducible results. When all candidates are returned, they keep the
order of `predictors_sf`.

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

# Environmental stratification (default)
pa_env <- h3sdm_pa(pres, predictors, n_pseudoabs = 300)

# Random sampling
set.seed(123)
pa_rnd <- h3sdm_pa(pres, predictors, n_pseudoabs = 300, method = "random")

# All hexagons without presence (to balance later)
pa_all <- h3sdm_pa(pres, predictors, n_pseudoabs = nrow(predictors),
                   buffer_k = 0, method = "random")
} # }
```
