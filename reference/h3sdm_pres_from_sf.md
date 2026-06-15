# Assign pre-downloaded species presence records to H3 hexagons

Takes an `sf` object of species occurrence records already downloaded
(e.g. from
[`h3sdm_get_records()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_get_records.md))
and assigns them to H3 hexagons, returning only hexagons with at least
one presence record.

This function extracts the hexagon-assignment logic from
[`h3sdm_get_records_by_hexagon()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_get_records_by_hexagon.md)
without downloading records internally, making it suitable for workflows
where records have already been retrieved.

## Usage

``` r
h3sdm_pres_from_sf(records_sf, aoi_sf, res = 6, expand_factor = 0.1)
```

## Arguments

- records_sf:

  An `sf` object with presence records in any CRS. Typically the output
  of
  [`h3sdm_get_records()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_get_records.md).

- aoi_sf:

  An `sf` object defining the area of interest.

- res:

  Integer. H3 resolution (0–15). Default is `6`.

- expand_factor:

  Numeric. Expansion factor for the H3 grid beyond the AOI bounding box.
  Default is `0.1`.

## Value

An `sf` object with one row per presence hexagon, containing:

- h3_address:

  H3 index of the hexagon.

- n:

  Number of records assigned to the hexagon.

- geometry:

  MULTIPOLYGON geometry of the hexagon.

## Details

This function is designed to be used in combination with
[`h3sdm_filter_outliers()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_filter_outliers.md)
and
[`h3sdm_pa()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_pa.md)
for a balanced presence/pseudo-absence workflow:


    # 1. Download records
    records_sf <- h3sdm_get_records("Species name", aoi_sf, providers = c("gbif", "biodatacr"))

    # 2. Assign to hexagons
    pres_sf <- h3sdm_pres_from_sf(records_sf, aoi_sf, res = 7)

    # 3. Filter environmental outliers (only presences)
    filtro <- h3sdm_filter_outliers(pres_sf_env, vars_cov)
    pres_clean <- filtro$pa_clean

    # 4. Generate balanced pseudo-absences (1:1)
    pa <- h3sdm_pa(pres_clean, predictors_sf, n_pseudoabs = nrow(pres_clean))

## See also

[`h3sdm_get_records()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_get_records.md),
[`h3sdm_pa()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_pa.md),
[`h3sdm_filter_outliers()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_filter_outliers.md)

## Examples

``` r
if (FALSE) { # \dontrun{
data(cr_outline_c, package = "h3sdm")

records_sf <- h3sdm_get_records(
  species   = "Panthera onca",
  aoi_sf    = cr_outline_c,
  providers = c("gbif", "biodatacr"),
  limit     = 500
)

pres_sf <- h3sdm_pres_from_sf(records_sf, cr_outline_c, res = 7)
nrow(pres_sf)
} # }
```
