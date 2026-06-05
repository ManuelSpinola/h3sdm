# Query Species Occurrence Records within an H3 Area of Interest (AOI)

Downloads species occurrence records from providers (e.g., GBIF,
iNaturalist, BiodataCR) and filters them by the exact polygonal boundary
of the Area of Interest (AOI). Providers supported by `spocc` (e.g.,
`"gbif"`, `"inat"`) are queried via
[`spocc::occ()`](https://docs.ropensci.org/spocc/reference/occ.html).
`"biodatacr"` is queried via
[`rbiodatacr::bdcr_occurrences()`](https://manuelspinola.github.io/rbiodatacr/reference/bdcr_occurrences.html)
and its output is standardized to the same `sf` format.

## Usage

``` r
h3sdm_get_records(
  species,
  aoi_sf,
  providers = NULL,
  limit = 500,
  remove_duplicates = FALSE,
  date = NULL
)
```

## Arguments

- species:

  Character string specifying the species name to query (e.g.,
  `"Puma concolor"`).

- aoi_sf:

  An `sf` object defining the Area of Interest (AOI). Its CRS will be
  transformed to WGS84 (`EPSG:4326`) before query.

- providers:

  Character vector of data providers to query. Accepted values: any
  provider supported by `spocc` (e.g., `"gbif"`, `"inat"`) plus
  `"biodatacr"` for BiodataCR (Costa Rica). If `NULL` (default), all
  `spocc` providers are used.

- limit:

  Numeric. Maximum number of records to retrieve per provider. Default
  is 500.

- remove_duplicates:

  Logical. If `TRUE`, records with identical coordinates are removed.
  Default is `FALSE`.

- date:

  Character vector specifying a date range (e.g.,
  `c('2000-01-01', '2020-12-31')`). Applied to `spocc` providers only.

## Value

An `sf` object of points with the filtered occurrence records whose
geometry falls strictly within the `aoi_sf` boundary. If no records are
found, an empty `sf` object with the expected structure is returned.

## Details

When `"biodatacr"` is included in `providers`, the function calls
[`rbiodatacr::bdcr_occurrences()`](https://manuelspinola.github.io/rbiodatacr/reference/bdcr_occurrences.html)
and standardizes its output (`decimalLatitude`/`decimalLongitude`) to
the same `sf` geometry format used by the `spocc` providers. Records
from all providers are then combined and clipped to the AOI.

## Examples

``` r
# \donttest{
  library(sf)

  aoi_sf <- sf::st_as_sf(
    data.frame(
      lon = c(-84.5, -83.5, -83.5, -84.5, -84.5),
      lat = c(9.5, 9.5, 10.5, 10.5, 9.5)
    ) |>
      {\(d) sf::st_sfc(sf::st_polygon(list(as.matrix(d))), crs = 4326)}(),
    data.frame(id = 1)
  )

  # GBIF only
  records <- h3sdm_get_records(
    species   = "Puma concolor",
    aoi_sf    = aoi_sf,
    providers = "gbif",
    limit     = 100
  )

  # GBIF + BiodataCR (Costa Rica)
  records_cr <- h3sdm_get_records(
    species   = "Agalychnis callidryas",
    aoi_sf    = aoi_sf,
    providers = c("gbif", "biodatacr"),
    limit     = 200
  )
# }
```
