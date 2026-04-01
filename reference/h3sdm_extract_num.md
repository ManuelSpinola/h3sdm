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
  library(sf)
  library(terra)

  # Create a SpatRaster stack with two numeric layers (e.g., bioclimatic variables)
  bio1 <- terra::rast(
    nrows = 10, ncols = 10,
    xmin = -84.5, xmax = -83.5,
    ymin = 9.5,  ymax = 10.5,
    crs = "EPSG:4326"
  )
  bio2 <- bio1
  terra::values(bio1) <- runif(terra::ncell(bio1), 15, 30)
  terra::values(bio2) <- runif(terra::ncell(bio2), 500, 3000)
  names(bio1) <- "bio1_temp"
  names(bio2) <- "bio12_precip"
  bio <- c(bio1, bio2)

  # Create a simple hexagon grid as sf polygons
  hex_grid <- sf::st_make_grid(
    sf::st_as_sfc(sf::st_bbox(c(
      xmin = -84.5, xmax = -83.5,
      ymin = 9.5,  ymax = 10.5
    ), crs = sf::st_crs(4326))),
    n = c(3, 3),
    square = FALSE
  )
  h7 <- sf::st_sf(h3_address = paste0("hex_", seq_along(hex_grid)),
                  geometry = hex_grid)

  # Extract numeric raster values by hexagon (mean per cell)
  bio_p <- h3sdm_extract_num(bio, h7)
#>   |                                                                     |                                                             |   0%  |                                                                     |===                                                          |   5%  |                                                                     |======                                                       |   9%  |                                                                     |========                                                     |  14%  |                                                                     |===========                                                  |  18%  |                                                                     |==============                                               |  23%  |                                                                     |=================                                            |  27%  |                                                                     |===================                                          |  32%  |                                                                     |======================                                       |  36%  |                                                                     |=========================                                    |  41%  |                                                                     |============================                                 |  45%  |                                                                     |==============================                               |  50%  |                                                                     |=================================                            |  55%  |                                                                     |====================================                         |  59%  |                                                                     |=======================================                      |  64%  |                                                                     |==========================================                   |  68%  |                                                                     |============================================                 |  73%  |                                                                     |===============================================              |  77%  |                                                                     |==================================================           |  82%  |                                                                     |=====================================================        |  86%  |                                                                     |=======================================================      |  91%  |                                                                     |==========================================================   |  95%  |                                                                     |=============================================================| 100%
  head(bio_p)
#> Simple feature collection with 6 features and 3 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.83333 ymin: 9.30755 xmax: -84.16667 ymax: 10.84715
#> Geodetic CRS:  WGS 84
#>   h3_address bio1_temp bio12_precip                       geometry
#> 1      hex_1       NaN          NaN POLYGON ((-84.5 9.69245, -8...
#> 2      hex_2       NaN          NaN POLYGON ((-84.5 10.2698, -8...
#> 3      hex_3  24.25950     1567.461 POLYGON ((-84.33333 9.40377...
#> 4      hex_4  24.07517     1941.622 POLYGON ((-84.33333 9.98112...
#> 5      hex_5  26.28069     1481.142 POLYGON ((-84.33333 10.5584...
#> 6      hex_6  22.12648     1566.341 POLYGON ((-84.16667 9.69245...
```
