## Script para regenerar cr_outline y cr_outline_c en h3sdm
## Fuente: GADM 4.1 via geodata
##
## cr_outline   -- Costa Rica completa (continente + todas las islas)
## cr_outline_c -- Solo continente (excluye Isla del Coco y otras islas menores)
##
## Para regenerar:
##   source("data-raw/cr_outline.R")

library(geodata)
library(sf)
library(dplyr)
library(usethis)

# -- Descargar GADM 4.1 -------------------------------------------------------
cr_raw <- geodata::gadm("CRI", level = 0, path = tempdir()) |>
  sf::st_as_sf()

# -- cr_outline: Costa Rica completa (continente + islas) ---------------------
cr_outline <- cr_raw |>
  dplyr::select(geometry)

# -- cr_outline_c: Solo continente (polígono de mayor área) -------------------
cr_outline_c <- cr_raw |>
  sf::st_cast("POLYGON") |>
  dplyr::mutate(area = sf::st_area(geometry)) |>
  dplyr::slice_max(.data$area, n = 1) |>
  dplyr::select(geometry)

# -- Guardar ------------------------------------------------------------------
usethis::use_data(cr_outline,   overwrite = TRUE)
usethis::use_data(cr_outline_c, overwrite = TRUE)
