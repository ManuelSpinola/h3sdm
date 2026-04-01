# Calculate Area Proportions for Categorical Raster Classes

Extracts and calculates the **area proportion** of each
land-use/land-cover (LULC) category found within each input polygon of
the `sf_hex_grid`. This function is tailored for categorical rasters and
ensures accurate, sub-pixel weighted statistics.

## Usage

``` r
h3sdm_extract_cat(spat_raster_cat, sf_hex_grid, proportion = TRUE)
```

## Arguments

- spat_raster_cat:

  A single-layer `SpatRaster` object containing categorical values
  (e.g., LULC classes).

- sf_hex_grid:

  An `sf` object containing polygonal geometries (e.g., H3 hexagons).
  Must contain a column named `h3_address` for joining and grouping.

- proportion:

  Logical. If `TRUE` (default), the output values are the proportion of
  the polygon area covered by each category (summing to 1 for covered
  area). If `FALSE`, the output is the raw sum of the coverage fraction
  (area).

## Value

An `sf` object identical to `sf_hex_grid`, but with new columns appended
for each categorical value found in the raster. Column names follow the
pattern `<layer_name>_prop_<category_value>`. Columns are **numerically
ordered** by the category value.

## Details

The function uses a custom function with
[`exactextractr::exact_extract`](https://isciences.gitlab.io/exactextractr/reference/exact_extract.html)
to perform three critical steps:

1.  **Filtering NA/NaN:** Raster cells with missing values (`NA`) are
    explicitly excluded from the calculation, preventing the creation of
    a `_prop_NaN` column.

2.  **Area Consolidation:** It sums the coverage fractions for all
    fragments belonging to the same category within the same hexagon,
    which is essential when polygons have been clipped or fragmented.

3.  **Numerical Ordering:** The final columns are explicitly sorted
    based on the numerical value of the category (e.g., `_prop_70`
    appears before `_prop_80`) to correct the default alphanumeric
    sorting behavior of
    [`tidyr::pivot_wider`](https://tidyr.tidyverse.org/reference/pivot_wider.html).

## Examples

``` r
  library(sf)
  library(terra)

  # Create a simple categorical SpatRaster
  lulc <- terra::rast(
    nrows = 20, ncols = 20,
    xmin = -85.0, xmax = -83.0,
    ymin = 9.0,  ymax = 11.0,
    crs = "EPSG:4326"
  )
  terra::values(lulc) <- sample(1:4, terra::ncell(lulc), replace = TRUE)
  names(lulc) <- "landuse"

  # Define categorical levels explicitly
  levels(lulc) <- data.frame(
    value = 1:4,
    class = c("forest", "grassland", "urban", "water")
  )

  # Create a simple hexagon grid as sf polygons (smaller than raster extent)
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

  # Extract categorical raster values by hexagon
  lulc_p <- h3sdm_extract_cat(lulc, h7, proportion = TRUE)
#>   |                                                                     |                                                             |   0%  |                                                                     |===                                                          |   5%  |                                                                     |======                                                       |   9%  |                                                                     |========                                                     |  14%  |                                                                     |===========                                                  |  18%  |                                                                     |==============                                               |  23%  |                                                                     |=================                                            |  27%  |                                                                     |===================                                          |  32%  |                                                                     |======================                                       |  36%  |                                                                     |=========================                                    |  41%  |                                                                     |============================                                 |  45%  |                                                                     |==============================                               |  50%  |                                                                     |=================================                            |  55%  |                                                                     |====================================                         |  59%  |                                                                     |=======================================                      |  64%  |                                                                     |==========================================                   |  68%  |                                                                     |============================================                 |  73%  |                                                                     |===============================================              |  77%  |                                                                     |==================================================           |  82%  |                                                                     |=====================================================        |  86%  |                                                                     |=======================================================      |  91%  |                                                                     |==========================================================   |  95%  |                                                                     |=============================================================| 100%
  head(lulc_p)
#> Simple feature collection with 6 features and 5 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.83333 ymin: 9.30755 xmax: -84.16667 ymax: 10.84715
#> Geodetic CRS:  WGS 84
#>   h3_address class_prop_1 class_prop_2 class_prop_3 class_prop_4
#> 1      hex_1   0.47825722    0.1948460   0.17402493    0.1528718
#> 2      hex_2   0.27594132    0.3420183   0.20056945    0.1814710
#> 3      hex_3   0.26392307    0.1460770   0.47523072    0.1147692
#> 4      hex_4   0.13238524    0.3493336   0.19353824    0.3247429
#> 5      hex_5   0.08144954    0.1131535   0.23430780    0.5710891
#> 6      hex_6   0.38912746    0.1039231   0.09289823    0.4140513
#>                         geometry
#> 1 MULTIPOLYGON (((-84.5 9.692...
#> 2 MULTIPOLYGON (((-84.5 10.26...
#> 3 MULTIPOLYGON (((-84.33333 9...
#> 4 MULTIPOLYGON (((-84.33333 9...
#> 5 MULTIPOLYGON (((-84.33333 1...
#> 6 MULTIPOLYGON (((-84.16667 9...
```
