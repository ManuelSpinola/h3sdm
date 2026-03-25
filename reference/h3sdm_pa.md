# Generate presence/pseudo-absence dataset for a species

Generates a hexagonal grid over the AOI, assigns species presence
records to hexagons, and samples pseudo-absences from hexagons with no
records.

## Usage

``` r
h3sdm_pa(
  species,
  aoi_sf,
  res = 6,
  n_pseudoabs = 500,
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

- n_pseudoabs:

  `integer` Number of pseudo-absence hexagons to sample.

- providers:

  `character` Optional vector of data providers (e.g., "gbif", "inat").

- remove_duplicates:

  `logical` Remove duplicate records at the same coordinates.

- date:

  `character` Optional date filter for records.

- limit:

  `integer` Maximum number of records to download.

- expand_factor:

  `numeric` Factor to expand AOI before creating hex grid.

## Value

`sf` object with columns:

- `h3_address`: H3 index of the hexagon.

- `presence`: factor with levels "0" (pseudo-absence) and "1"
  (presence).

- `geometry`: MULTIPOLYGON of each hexagon.

## Examples

``` r
if (FALSE) { # \dontrun{
data(cr_outline_c, package = "h3sdm")
dataset <- h3sdm_pa("Agalychnis callidryas", cr_outline_c, res = 7, n_pseudoabs = 100)
} # }
```
