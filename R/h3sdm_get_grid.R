#' @title Generar cuadrícula H3 para un área de interés
#' @name h3sdm_get_grid
#'
#' @description
#' Crea una cuadrícula de hexágonos H3 que cubre un área de interés (`sf_object`),
#' asegurando que las celdas se ajusten a la extensión del área y se recorten
#' opcionalmente al contorno del AOI.
#'
#' Esta función es equivalente a la usada en los módulos de paisaje de `h3sdm`,
#' pero con el nombre estandarizado para mantener consistencia en el paquete.
#'
#' @param sf_object Objeto `sf` que define el área de interés (AOI).
#' @param resolution Entero entre 1 y 16. Define la resolución del índice H3.
#' Valores mayores producen hexágonos más pequeños.
#' @param expand_factor Valor numérico que amplía ligeramente el bounding box
#' del AOI antes de generar los hexágonos. Por defecto `0.1`.
#' @param clip_to_aoi Lógico (`TRUE` o `FALSE`), indica si los hexágonos deben
#' recortarse exactamente al contorno del AOI. Por defecto `TRUE`.
#'
#' @return
#' Un objeto `sf` con los hexágonos H3 correspondientes al área de interés,
#' con geometrías válidas (`MULTIPOLYGON`).
#'
#' @examples
#' \dontrun{
#' library(sf)
#'
#' # Crear un polígono de ejemplo
#' cr <- st_as_sf(data.frame(
#'   lon = c(-85, -85, -83, -83, -85),
#'   lat = c(9, 11, 11, 9, 9)
#' ), coords = c("lon", "lat"), crs = 4326) |>
#'   summarise(geometry = st_combine(geometry)) |>
#'   st_cast("POLYGON")
#'
#' # Generar cuadrícula H3
#' h5 <- h3sdm_get_grid(cr, resolution = 5)
#' plot(st_geometry(h5))
#' }
#'
#' @export

h3sdm_get_grid <- function(sf_object, resolution = 6, expand_factor = 0.1, clip_to_aoi = TRUE) {
  # Validaciones iniciales
  if (!inherits(sf_object, "sf")) stop("Input must be an sf object")
  if (resolution < 1 || resolution > 16) stop("Resolution must be between 1 and 16")

  # Transformar a WGS84 y asegurar geometrías válidas
  sf_object <- sf::st_transform(sf_object, 4326)
  sf_object <- sf::st_make_valid(sf_object)

  # Expandir bbox ligeramente
  bbox <- sf::st_bbox(sf_object)
  bbox_expanded <- bbox + c(-expand_factor, -expand_factor, expand_factor, expand_factor)
  bbox_poly <- sf::st_as_sfc(bbox_expanded)

  # Crear hexágonos H3 sobre el bbox expandido
  h3_cells <- h3jsr::polygon_to_cells(bbox_poly, res = resolution)
  hexagons <- h3jsr::cell_to_polygon(h3_cells, simple = FALSE)

  # Recortar hexágonos al AOI (solo si clip_to_aoi = TRUE)
  if (clip_to_aoi) {
    hexagons <- sf::st_intersection(hexagons, sf_object)
  }

  # Unir fragmentos por celda para que siempre sea MULTIPOLYGON
  hexagons <- sf::st_union(hexagons, by_feature = TRUE)

  # Asegurar geometrías válidas
  hexagons <- sf::st_make_valid(hexagons)

  # Devolver
  return(hexagons)
}
