# Query Species Occurrence Records within an H3 Area of Interest (AOI)

Downloads species occurrence records from providers (e.g., GBIF) using
the `spocc` package, filtering the initial query by the exact polygonal
boundary of the Area of Interest (AOI) for maximum efficiency and
precision.

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

  Character string specifying the species name to query (e.g., "Puma
  concolor").

- aoi_sf:

  An `sf` object defining the Area of Interest (AOI). Its CRS will be
  transformed to WGS84 (`EPSG:4326`) before query.

- providers:

  Character vector of data providers to query (e.g., "gbif", "bison").
  If `NULL` (default), all available providers are used.

- limit:

  Numeric. The maximum number of records to retrieve per provider.
  Default is 500.

- remove_duplicates:

  Logical. If `TRUE`, records with identical longitude and latitude are
  removed using
  [`dplyr::distinct()`](https://dplyr.tidyverse.org/reference/distinct.html).
  Default is `FALSE`.

- date:

  Character vector specifying a date range (e.g.,
  `c('2000-01-01', '2020-12-31')`).

## Value

An `sf` object of points containing the filtered occurrence records,
with geometry confirmed to fall strictly within the `aoi_sf` boundary.
If no records are found or the download fails, an empty `sf` object with
the expected structure is returned.

## Details

The function transforms the `aoi_sf` polygon into a WKT string, which is
used in the
[`spocc::occ`](https://docs.ropensci.org/spocc/reference/occ.html)
geometry argument for **efficient WKT-based querying**. Final spatial
filtering is performed using
[`sf::st_intersection`](https://r-spatial.github.io/sf/reference/geos_binary_ops.html)
to ensure strict containment. A critical check is included to prevent
errors when the API returns no data (addressing the 'column not found'
error).

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming aoi_sf is a valid sf polygon
# h3_records <- h3sdm_get_records("Puma concolor", aoi_sf, providers = "gbif", limit = 1000)
# head(h3_records)
} # }
```
