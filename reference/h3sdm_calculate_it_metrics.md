# Calculate Information Theory Landscape Metrics for Hexagonal Grid

Calculates 5 Information Theory (IT)-based landscape metrics (`condent`,
`ent`, `joinent`, `mutinf`, `relmutinf`) for each hexagon in a given H3
hexagonal grid.

## Usage

``` r
h3sdm_calculate_it_metrics(landscape_raster, sf_grid)
```

## Arguments

- landscape_raster:

  A categorical SpatRaster containing land-cover data.

- sf_grid:

  An `sf` object containing the hexagonal grid with species or
  land-cover data.

## Value

An `sf` object containing the input hex grid with new columns for each
calculated metric.

## Details

This function computes landscape metrics using the
[`landscapemetrics::sample_lsm()`](https://r-spatialecology.github.io/landscapemetrics/reference/sample_lsm.html)
workflow. The results are pivoted to a wide format for easy use.

## References

Hesselbarth et al., 2019. landscapemetrics: an open-source R tool to
calculate landscape metrics. Ecography 42: 1648–1657.

Nowosad & Stepinski, 2019. Information theory as a consistent framework
for landscape patterns.
[doi:10.1007/s10980-019-00830-x](https://doi.org/10.1007/s10980-019-00830-x)

## Examples

``` r
# \donttest{
  library(sf)
  library(terra)

  # Create a categorical SpatRaster (land-cover map)
  landscape_raster <- terra::rast(
    nrows = 30, ncols = 30,
    xmin = -85.0, xmax = -83.0,
    ymin = 9.0,  ymax = 11.0,
    crs = "EPSG:4326"
  )
  terra::values(landscape_raster) <- sample(1:4, terra::ncell(landscape_raster),
                                            replace = TRUE)
  names(landscape_raster) <- "landcover"

  # Create a simple hexagon grid as sf polygons
  hex_grid <- sf::st_make_grid(
    sf::st_as_sfc(sf::st_bbox(c(
      xmin = -84.5, xmax = -83.5,
      ymin = 9.5,  ymax = 10.5
    ), crs = sf::st_crs(4326))),
    n = c(3, 3),
    square = FALSE
  )
  sf_grid <- sf::st_sf(h3_address = paste0("hex_", seq_along(hex_grid)),
                       geometry = hex_grid)

  # Calculate Information Theory (IT) landscape metrics per hexagon
  result_sf <- h3sdm_calculate_it_metrics(landscape_raster, sf_grid)
#> Warning: The 'perecentage_inside' is below 90% for at least one buffer.
#> Warning: Please use 'check_landscape()' to ensure the input data is valid.
  head(result_sf)
#> Simple feature collection with 6 features and 6 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.83333 ymin: 9.30755 xmax: -84.16667 ymax: 10.84715
#> Geodetic CRS:  WGS 84
#>   h3_address lsm_condent  lsm_ent lsm_joinent lsm_mutinf lsm_relmutinf
#> 1      hex_1    1.893977 1.990274    3.884252 0.09629689   0.048383731
#> 2      hex_2    1.930149 1.947507    3.877656 0.01735833   0.008913103
#> 3      hex_3    1.859770 1.975793    3.835564 0.11602324   0.058722350
#> 4      hex_4    1.759298 1.817505    3.576803 0.05820740   0.032025992
#> 5      hex_5    1.694564 1.971342    3.665906 0.27677840   0.140400992
#> 6      hex_6    1.877988 1.911213    3.789200 0.03322472   0.017384107
#>                         geometry
#> 1 POLYGON ((-84.66667 9.59622...
#> 2 POLYGON ((-84.66667 10.1735...
#> 3 POLYGON ((-84.5 9.30755, -8...
#> 4 POLYGON ((-84.5 9.8849, -84...
#> 5 POLYGON ((-84.5 10.46225, -...
#> 6 POLYGON ((-84.33333 9.59622...
# }
```
