# Create a spatial-aware cross-validation split for H3 data

Generates a spatially aware cross-validation split for species
distribution modeling using H3 hexagonal grids. This helps avoid
inflated model performance estimates caused by spatial autocorrelation,
producing more robust model evaluation.

## Usage

``` r
h3sdm_spatial_cv(data, method = "block", v = 5, ...)
```

## Arguments

- data:

  An `sf` object, typically the output of
  [`h3sdm_data()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_data.md).

- method:

  Character. The spatial resampling method to use:

  "block"

  :   Use
      [`spatialsample::spatial_block_cv()`](https://spatialsample.tidymodels.org/reference/spatial_block_cv.html)
      for block-based spatial CV.

  "cluster"

  :   Use
      [`spatialsample::spatial_clustering_cv()`](https://spatialsample.tidymodels.org/reference/spatial_clustering_cv.html)
      for cluster-based spatial CV.

- v:

  Integer. Number of folds (default = 5).

- ...:

  Additional arguments passed to the underlying `spatialsample`
  function.

## Value

An `rsplit` object (from `rsample`) representing the spatial CV folds.

## Details

Spatial cross-validation avoids overly optimistic performance estimates
by ensuring that training and testing data are spatially separated.

- `"block"`: Divides the spatial domain into contiguous blocks.

- `"cluster"`: Groups locations into spatial clusters before splitting.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example: Create spatial cross-validation splits for H3 data

# Block spatial CV with default folds
spatial_cv_block <- h3sdm_spatial_cv(combined_data, method = "block")

# Cluster spatial CV with 10 folds
spatial_cv_cluster <- h3sdm_spatial_cv(combined_data, method = "cluster", v = 10)
} # }
```
