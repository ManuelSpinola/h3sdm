# Assign species presence records to H3 hexagons

Generates a hexagonal grid over the AOI, downloads species occurrence
records, and assigns them to hexagons. Returns only hexagons with at
least one presence record. This is the first step of a two-stage
workflow where pseudo-absences are generated later using
[`h3sdm_pa()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_pa.md)
after environmental variables have been extracted with
[`h3sdm_extract_num()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_extract_num.md)
and related functions.

## Usage

``` r
h3sdm_pres(
  species,
  aoi_sf,
  res = 6,
  providers = NULL,
  remove_duplicates = FALSE,
  date = NULL,
  limit = 500,
  expand_factor = 0.1
)
```

## Arguments

- species:

  `character` Species name (single string) for which records are
  requested.

- aoi_sf:

  `sf` AOI (area of interest) polygon.

- res:

  `integer` H3 resolution for the hexagonal grid.

- providers:

  `character` Optional vector of data providers. Accepted values: any
  provider supported by `spocc` (e.g., `"gbif"`, `"inat"`) plus
  `"biodatacr"` for BiodataCR (Costa Rica), queried via the `rbiodatacr`
  package. If `NULL` (default), all `spocc` providers are used.

- remove_duplicates:

  `logical` Remove duplicate records at the same coordinates. Default is
  `FALSE`.

- date:

  `character` Optional date filter for records.

- limit:

  `integer` Maximum number of records to download. Default is `500`.

- expand_factor:

  `numeric` Factor to expand AOI before creating the hexagonal grid.
  Default is `0.1`.

## Value

`sf` object with columns:

- `h3_address`: H3 index of the hexagon.

- `presence`: integer column with value `1` for all returned hexagons.

- `geometry`: MULTIPOLYGON of each hexagon.

## Details

Unlike
[`h3sdm_pa()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_pa.md),
this function does not sample pseudo-absences. It is intended to be used
as the first step of a workflow where environmental variables are
extracted for the full hexagonal grid before pseudo-absences are
selected in environmental space using
[`h3sdm_pa()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_pa.md).

## Examples

``` r
if (FALSE) { # \dontrun{
data(cr_outline_c, package = "h3sdm")
pres <- h3sdm_pres("Agalychnis callidryas", cr_outline_c, res = 7)
} # }
```
