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
# Assuming 'sf_grid' is an sf object with hexagons
# result_sf <- h3sdm_calculate_it_metrics(sf_grid)
# head(result_sf)
# }
```
