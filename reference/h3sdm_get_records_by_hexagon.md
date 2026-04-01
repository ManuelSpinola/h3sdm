# Download Species Records and Count Occurrences per H3 Hexagon

This function downloads occurrence records for one or more species and
counts the number of records falling inside each H3 hexagon covering the
specified Area of Interest (AOI).

## Usage

``` r
h3sdm_get_records_by_hexagon(
  species,
  aoi_sf,
  res = 6,
  providers = NULL,
  remove_duplicates = FALSE,
  date = NULL,
  expand_factor = 0.1,
  limit = 500
)
```

## Arguments

- species:

  Character vector of species names to query (e.g.,
  `c("Puma concolor", "Panthera onca")`).

- aoi_sf:

  An `sf` polygon defining the Area of Interest (AOI).

- res:

  Numeric. H3 resolution level (default 6), determining hexagon size.

- providers:

  Character vector of data providers (e.g., "gbif"). If `NULL`, all
  providers are used.

- remove_duplicates:

  Logical. If `TRUE`, duplicate coordinates are removed before counting.
  Default is `FALSE`.

- date:

  Character vector specifying a date range (e.g.,
  `c('2000-01-01','2020-12-31')`).

- expand_factor:

  Numeric. Factor to expand the AOI bounding box before generating the
  H3 grid. Default is 0.1.

- limit:

  Numeric. Maximum number of records to retrieve per species per
  provider. Default is 500.

## Value

An `sf` object containing the H3 hexagonal grid (`MULTIPOLYGON`) with
additional integer columns for each species (spaces replaced by
underscores) showing the count of occurrence records in each hexagon.
Hexagons with no records have 0.

## Details

Download Species Records and Count Occurrences per H3 Hexagon

For each species:

1.  An H3 grid is generated across the AOI using
    [`h3sdm_get_grid()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_get_grid.md).

2.  Occurrence records are downloaded using
    [`h3sdm_get_records()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_get_records.md).

3.  Points are joined to the hexagonal grid with
    [`sf::st_join()`](https://r-spatial.github.io/sf/reference/st_join.html).

4.  Counts of points per hexagon are calculated.

5.  Counts are merged into the main hex grid.

The function ensures column names derived from species names are safe in
R by replacing spaces with underscores and handles API failures
gracefully.

## See also

[`h3sdm_get_grid`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_get_grid.md),
[`h3sdm_get_records`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_get_records.md)

## Examples

``` r
# \donttest{
  library(sf)

  # Create a simple AOI polygon in Costa Rica
  aoi_sf <- sf::st_as_sf(
    data.frame(id = 1),
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(
        c(-84.5, 9.5,
          -83.5, 9.5,
          -83.5, 10.5,
          -84.5, 10.5,
          -84.5, 9.5),
        ncol = 2, byrow = TRUE
      ))),
      crs = 4326
    )
  )

  hex_counts <- h3sdm_get_records_by_hexagon(
    species = c("Agalychnis callidryas", "Smilisca baudinii"),
    aoi_sf  = aoi_sf,
    res     = 7,
    providers = "gbif",
    limit   = 100
  )

  print(hex_counts)
#> Simple feature collection with 2476 features and 3 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.5 ymin: 9.5 xmax: -83.5 ymax: 10.50039
#> Geodetic CRS:  WGS 84
#> First 10 features:
#>         h3_address Agalychnis_callidryas Smilisca_baudinii
#> 1  8766b459bffffff                     0                 0
#> 2  8766b4516ffffff                     0                 0
#> 3  87679a6f6ffffff                     0                 0
#> 4  87679a671ffffff                     0                 0
#> 5  87679a4e2ffffff                     0                 0
#> 6  876d69322ffffff                     0                 0
#> 7  876d6929dffffff                     0                 0
#> 8  87679a45dffffff                     0                 0
#> 9  876d69218ffffff                     0                 0
#> 10 876d6910effffff                     0                 1
#>                          geometry
#> 1  MULTIPOLYGON (((-84.15606 9...
#> 2  MULTIPOLYGON (((-84.07649 9...
#> 3  MULTIPOLYGON (((-83.71694 1...
#> 4  MULTIPOLYGON (((-83.53215 1...
#> 5  MULTIPOLYGON (((-83.89897 1...
#> 6  MULTIPOLYGON (((-84.23194 1...
#> 7  MULTIPOLYGON (((-84.40951 9...
#> 8  MULTIPOLYGON (((-83.76182 1...
#> 9  MULTIPOLYGON (((-84.28254 9...
#> 10 MULTIPOLYGON (((-84.46113 1...
# }
```
