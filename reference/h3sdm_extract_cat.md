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
if (FALSE) { # \dontrun{
# Assuming 'lulc' is a categorical SpatRaster and 'h7' is an sf hexagon grid
# lulc_p <- h3sdm_extract_cat(lulc, h7, proportion = TRUE)
# head(lulc_p)
} # }
```
