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
if (FALSE) { # \dontrun{
# Assuming aoi_sf is a valid sf polygon
# species_list <- c("Agalychnis callidryas", "Smilisca baudinii")
# hex_counts <- h3sdm_get_records_by_hexagon(species_list, aoi_sf, res = 7, limit = 1000)
# head(hex_counts)
} # }
```
