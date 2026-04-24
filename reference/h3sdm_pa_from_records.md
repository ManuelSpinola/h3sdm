# Generate presence/pseudo-absence dataset from user-provided records

Adapts a user-provided dataset with presence records (from personal
fieldwork, BiodataCR, or any other source) into a hexagonal
presence/pseudo-absence dataset ready for analysis with h3sdm. The input
can be a `data.frame` with coordinate columns or an `sf` object.
Coordinates are assumed to be in WGS84 (EPSG:4326).

## Usage

``` r
h3sdm_pa_from_records(
  records,
  aoi_sf,
  res = 6,
  n_pseudoabs = 500,
  expand_factor = 0.1,
  lon_col = "lon",
  lat_col = "lat",
  species_col = NULL,
  geospatial_filter = TRUE
)
```

## Arguments

- records:

  `data.frame` or `sf` object containing presence records.

- aoi_sf:

  `sf` AOI (area of interest) polygon.

- res:

  `integer` H3 resolution for the hexagonal grid.

- n_pseudoabs:

  `integer` Number of pseudo-absence hexagons to sample.

- expand_factor:

  `numeric` Factor to expand AOI before creating hex grid.

- lon_col:

  `character` Name of the longitude column. Ignored if `records` is
  already an `sf` object.

- lat_col:

  `character` Name of the latitude column. Ignored if `records` is
  already an `sf` object.

- species_col:

  `character` Optional. Name of the column containing the species name.
  If provided, the column is retained in the output as metadata.

- geospatial_filter:

  `logical` If `TRUE` (default) and the input contains a
  `geospatialKosher` column, records with `geospatialKosher == FALSE`
  are removed before processing. Ignored if the column is absent.

## Value

`sf` object with columns:

- h3_address:

  H3 index of the hexagon.

- presence:

  Factor with levels `"0"` (pseudo-absence) and `"1"` (presence).

- species:

  Species name (only if `species_col` is provided).

- geometry:

  MULTIPOLYGON of each hexagon.

## Examples

``` r
# \donttest{
data(cr_outline_c, package = "h3sdm")

my_records <- data.frame(
  lon = c(-84.1, -84.2, -83.9),
  lat = c(9.9, 10.1, 9.8),
  species = "Agalychnis callidryas"
)

dataset <- h3sdm_pa_from_records(
  records     = my_records,
  aoi_sf      = cr_outline_c,
  res         = 7,
  n_pseudoabs = 100,
  lon_col     = "lon",
  lat_col     = "lat",
  species_col = "species"
)
# }
```
