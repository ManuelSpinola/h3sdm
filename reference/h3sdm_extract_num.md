# Extract Area-Weighted Mean from Numeric Raster Stack

Calculates the **area-weighted mean** value for each layer in a numeric
`SpatRaster` (or single layer) within each polygon feature of an `sf`
object. This function is designed to efficiently summarize continuous
environmental variables (such as bioclimatic data) for predefined
spatial units (e.g., H3 hexagons). It utilizes `exactextractr` to ensure
highly precise zonal statistics by accounting for sub-pixel coverage
fractions.

## Usage

``` r
h3sdm_extract_num(spat_raster_multi, sf_hex_grid)
```

## Arguments

- spat_raster_multi:

  A `SpatRaster` object from the `terra` package. Must contain numeric
  layers (can be a single layer or a stack/brick).

- sf_hex_grid:

  An `sf` object containing polygonal geometries (e.g., H3 hexagons).
  Must be a valid set of polygons for extraction.

## Value

An `sf` object identical to `sf_hex_grid`, but with new columns
appended. The new column names match the original `SpatRaster` layer
names. The values represent the area-weighted mean for that variable
within each polygon.

## Details

The function relies on
[`exactextractr::exact_extract`](https://isciences.gitlab.io/exactextractr/reference/exact_extract.html)
with `fun = "weighted_mean"` and `weights = "area"`. This methodology is
crucial for maintaining spatial accuracy when polygons are irregular or
small relative to the raster resolution. A critical check (`nrow` match)
is performed before binding columns to ensure data integrity and prevent
misalignment errors.

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming 'bio' is a SpatRaster stack and 'h7' is an sf hexagon grid
# bio_p <- h3sdm_extract_num(bio, h7)
# head(bio_p)
} # }
```
