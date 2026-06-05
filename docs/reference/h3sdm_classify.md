# Classify predictions based on an optimal threshold

Converts continuous probability predictions into binary presence/absence
based on a specified threshold.

## Usage

``` r
h3sdm_classify(predictions_sf, threshold)
```

## Arguments

- predictions_sf:

  An `sf` object containing a numeric column named `prediction`,
  typically produced by
  [`h3sdm_predict()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_predict.md).

- threshold:

  A numeric value representing the probability threshold (e.g., `0.45`)
  above which predictions are classified as presence (`1`).

## Value

An `sf` object with the same geometry and all original columns, plus a
new integer column `predicted_presence` with values `0` (absence) or `1`
(presence).

## Details

This function is useful for converting continuous probability outputs
into binary presence/absence data for mapping or model evaluation
purposes.

## Examples

``` r
if (FALSE) { # \dontrun{
library(sf)
library(dplyr)

# Crear un sf de ejemplo
df <- data.frame(
  id = 1:5,
  prediction = c(0.2, 0.6, 0.45, 0.8, 0.3),
  lon = c(-75, -74, -73, -72, -71),
  lat = c(10, 11, 12, 13, 14)
)

df_sf <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326)

# Clasificar usando un umbral
classified_sf <- h3sdm_classify(df_sf, threshold = 0.5)

# Revisar resultados
print(classified_sf)
} # }
```
