# Generar cuadrícula H3 para un área de interés

Crea una cuadrícula de hexágonos H3 que cubre un área de interés
(`sf_object`), asegurando que las celdas se ajusten a la extensión del
área y se recorten opcionalmente al contorno del AOI.

Esta función es equivalente a la usada en los módulos de paisaje de
`h3sdm`, pero con el nombre estandarizado para mantener consistencia en
el paquete.

## Usage

``` r
h3sdm_get_grid(sf_object, res = 6, expand_factor = 0.1, clip_to_aoi = TRUE)
```

## Arguments

- sf_object:

  Objeto `sf` que define el área de interés (AOI).

- res:

  Entero entre 1 y 16. Define la resolución del índice H3. Valores
  mayores producen hexágonos más pequeños.

- expand_factor:

  Valor numérico que amplía ligeramente el bounding box del AOI antes de
  generar los hexágonos. Por defecto `0.1`.

- clip_to_aoi:

  Lógico (`TRUE` o `FALSE`), indica si los hexágonos deben recortarse
  exactamente al contorno del AOI. Por defecto `TRUE`.

## Value

Un objeto `sf` con los hexágonos H3 correspondientes al área de interés,
con geometrías válidas (`MULTIPOLYGON`).

## Examples

``` r
if (FALSE) { # \dontrun{
library(sf)

# Crear un polígono de ejemplo
cr <- st_as_sf(data.frame(
  lon = c(-85, -85, -83, -83, -85),
  lat = c(9, 11, 11, 9, 9)
), coords = c("lon", "lat"), crs = 4326) |>
  summarise(geometry = st_combine(geometry)) |>
  st_cast("POLYGON")

# Generar cuadrícula H3
h5 <- h3sdm_get_grid(cr, res = 5)
plot(st_geometry(h5))
} # }
```
