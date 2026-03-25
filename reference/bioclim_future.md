# Future bioclimatic raster

A GeoTIFF with projected bioclimatic variables for Costa Rica.

## Format

GeoTIFF file, readable with
[`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html).

## Details

This dataset corresponds to the climate projection:

- Model: INM-CM4-8

- Scenario: SSP1-2.6

- Period: 2021–2040

The file is stored in `inst/extdata/` and can be accessed with:
`terra::rast(system.file("extdata", "bioclim_future.tif", package = "h3sdm"))`

## Examples

``` r
library(terra)
bio <- terra::rast(system.file("extdata", "bioclim_future.tif", package = "h3sdm"))
```
