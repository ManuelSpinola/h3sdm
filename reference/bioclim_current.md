# Current bioclimatic raster

A GeoTIFF with current bioclimatic variables for Costa Rica.

## Format

GeoTIFF file, readable with
[`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html).

## Details

This file is stored in `inst/extdata/` and can be accessed with:
`terra::rast(system.file("extdata", "bioclim_current.tif", package = "h3sdm"))`

## Examples

``` r
library(terra)
#> terra 1.9.11
bio <- terra::rast(system.file("extdata", "bioclim_current.tif", package = "h3sdm"))
```
