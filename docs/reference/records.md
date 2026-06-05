# Presence/pseudo-absence records for Silverstoneia flotator

A dataset containing presence and pseudo-absence records for the species
*Silverstoneia flotator* in Costa Rica, generated using H3 hexagonal
grids at resolution 7.

## Usage

``` r
records
```

## Format

An `sf` object with columns:

- h3_address:

  H3 index of the hexagon

- presence:

  factor with levels "0" (pseudo-absence) and "1" (presence)

- geometry:

  MULTIPOLYGON of each hexagon

## Source

Generated using
[`h3sdm_pa()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_pa.md)
with occurrence data from GBIF (<https://www.gbif.org>).

## Examples

``` r
data(records)
head(records)
#> Simple feature collection with 6 features and 2 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.06344 ymin: 8.486587 xmax: -82.77295 ymax: 9.643344
#> Geodetic CRS:  WGS 84
#>          h3_address presence                       geometry
#> 43  8766b4415ffffff        1 MULTIPOLYGON (((-84.05549 9...
#> 165 87679b636ffffff        1 MULTIPOLYGON (((-82.79149 9...
#> 198 8766b54d3ffffff        1 MULTIPOLYGON (((-83.18895 8...
#> 427 87679b78effffff        1 MULTIPOLYGON (((-82.85228 9...
#> 796 8766b0135ffffff        1 MULTIPOLYGON (((-83.71366 8...
#> 893 8766b014cffffff        1 MULTIPOLYGON (((-83.53362 8...
table(records$presence)
#> 
#>   0   1 
#> 300 123 
```
