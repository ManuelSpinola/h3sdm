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
# \donttest{
  library(sf)

  # Create a simple AOI polygon in Costa Rica
  aoi_sf <- sf::st_as_sf(
    data.frame(
      lon = c(-84.5, -83.5, -83.5, -84.5, -84.5),
      lat = c(9.5, 9.5, 10.5, 10.5, 9.5)
    ) |>
      {\(d) sf::st_sfc(sf::st_polygon(list(as.matrix(d))), crs = 4326)}(),
    data.frame(id = 1)
  )

  records <- h3sdm_get_records(
    species = "Puma concolor",
    aoi_sf = aoi_sf,
    providers = "gbif",
    limit = 100
  )

  print(records)
#> Simple feature collection with 100 features and 5 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -84.47708 ymin: 9.51482 xmax: -83.53498 ymax: 10.49709
#> Geodetic CRS:  WGS 84
#> # A tibble: 100 × 6
#>    name          prov  date       key      id             geometry
#>  * <chr>         <chr> <date>     <chr> <dbl>          <POINT [°]>
#>  1 Puma concolo… gbif  2025-02-19 5285…     1 (-83.71914 10.22468)
#>  2 Puma concolo… gbif  2025-03-04 5108…     1 (-84.01068 10.43068)
#>  3 Puma concolo… gbif  2025-03-18 5285…     1  (-83.68964 9.99557)
#>  4 Puma concolo… gbif  2025-04-07 5134…     1 (-84.04335 9.592563)
#>  5 Puma concolo… gbif  2025-04-09 5134…     1 (-83.75343 9.748917)
#>  6 Puma concolo… gbif  2025-07-09 5760…     1 (-83.76754 9.554898)
#>  7 Puma concolo… gbif  2024-01-18 5285…     1 (-83.58003 10.04675)
#>  8 Puma concolo… gbif  2024-02-23 4528…     1 (-84.01002 10.43693)
#>  9 Puma concolo… gbif  2024-03-20 4599…     1 (-84.47061 10.02827)
#> 10 Puma concolo… gbif  2024-05-28 5285…     1  (-84.3503 10.29371)
#> # ℹ 90 more rows
# }
```
