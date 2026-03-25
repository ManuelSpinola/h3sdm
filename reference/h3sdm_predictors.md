# Combine Predictor Data from Multiple sf Objects

This function merges predictor variables from multiple `sf` objects into
a single `sf` object. It preserves the geometry from the first input and
joins columns from the other `sf` objects using a common key
(`h3_address` or `ID`).

## Usage

``` r
h3sdm_predictors(...)
```

## Arguments

- ...:

  Two or more `sf` objects containing predictor variables. The first
  object must contain the geometry to preserve. All objects must share a
  common key column (`h3_address` or `ID`).

## Value

An `sf` object containing the geometry of the first input and all
predictor columns from all provided `sf` objects.

## Details

The function uses a left join based on the `h3_address` column if
present, otherwise it falls back to `ID`. Geometries from the right-hand
side `sf` objects are dropped to avoid conflicts, and the final geometry
is cast to `MULTIPOLYGON`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Combine sf objects with different predictor types into one
combined <- h3sdm_predictors(num_sf, cat_sf, it_sf)
head(combined)
} # }
```
