#' @name h3sdm_pa
#' @title Generate presence/pseudo-absence dataset for a species
#' @description
#' Generates a hexagonal grid over the AOI, assigns species presence records to hexagons,
#' and samples pseudo-absences from hexagons with no records.
#'
#' @param species `character` Species name (single string) for which records are requested.
#' @param aoi_sf `sf` AOI (area of interest) polygon.
#' @param res `integer` H3 resolution for the hexagonal grid.
#' @param n_pseudoabs `integer` Number of pseudo-absence hexagons to sample.
#' @param providers `character` Optional vector of data providers (e.g., "gbif", "inat").
#' @param remove_duplicates `logical` Remove duplicate records at the same coordinates.
#' @param date `character` Optional date filter for records.
#' @param limit `integer` Maximum number of records to download.
#' @param expand_factor `numeric` Factor to expand AOI before creating hex grid.
#'
#' @return `sf` object with columns:
#'   - `h3_address`: H3 index of the hexagon.
#'   - `presence`: factor with levels "0" (pseudo-absence) and "1" (presence).
#'   - `geometry`: MULTIPOLYGON of each hexagon.
#'
#' @examples
#' \donttest{
#' cr_outline <- st_read(system.file("shape/nc.shp", package="sf"))
#' dataset <- h3sdm_pa("Agalychnis callidryas", cr_outline, res = 7, n_pseudoabs = 100)
#' }
#'
#' @export

h3sdm_pa <- function(species,
                     aoi_sf,
                     res = 6,
                     n_pseudoabs = 500,
                     providers = NULL,
                     remove_duplicates = FALSE,
                     date = NULL,
                     limit = 500,
                     expand_factor = 0.1) {

  # Validar inputs (Estos siempre van primero y fuera del silenciamiento)
  if (!inherits(aoi_sf, "sf")) stop("aoi_sf must be an sf object")
  if (!is.character(species)) stop("species must be a character vector")

  # ❗ FIX: ENVOLVER TODO EL PROCESAMIENTO EN UN SOLO SUPPRESSWARNINGS
  # Esto asegura que todas las advertencias técnicas de sf, dplyr, h3jsr, etc., se silencien.
  results <- suppressWarnings({

    # 1️⃣ Generar hexagonal grid
    hex_grid <- h3sdm_get_grid(aoi_sf, res = res, expand_factor = expand_factor)
    hex_grid <- hex_grid[, c("h3_address", "geometry")]
    hex_grid <- sf::st_cast(hex_grid, "MULTIPOLYGON")

    # 2️⃣ Obtener registros de especie (Asumimos que esta es silenciosa para cero registros)
    sp_sf <- h3sdm_get_records(species, aoi_sf,
                               providers = providers,
                               remove_duplicates = remove_duplicates,
                               date = date,
                               limit = limit)

    # Si hay cero registros, devolvemos un objeto especial con hex_grid y el indicador
    if (nrow(sp_sf) == 0) {
      hex_grid$presence <- factor(0, levels = c("0", "1"))
      # Usamos 'attr' para pasar el estado del warning fuera del bloque
      attr(hex_grid, "no_records") <- TRUE
      return(hex_grid)
    }

    # 3️⃣ Limpieza de atributos
    sp_sf_clean <- sp_sf %>%
      dplyr::select(geometry)

    # 4️⃣ Asignar registros a hexágonos
    joined <- sf::st_join(sp_sf_clean, hex_grid, left = FALSE)

    rec_count <- joined %>%
      sf::st_drop_geometry() %>%
      dplyr::group_by(hex_id = h3_address) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop")

    # 5️⃣ Crear columna presence
    hex_grid$presence <- 0
    hex_grid$presence[hex_grid$h3_address %in% rec_count$hex_id] <- 1

    # 6️⃣ Sample pseudo-absences
    pos_hex <- hex_grid[hex_grid$presence == 1, ]
    neg_hex <- hex_grid[hex_grid$presence == 0, ]
    n_sample <- min(n_pseudoabs, nrow(neg_hex))
    if (n_sample > 0) {
      neg_hex <- neg_hex[sample(seq_len(nrow(neg_hex)), n_sample), ]
      neg_hex$presence <- 0
    }

    # 7️⃣ Combinar presencia y pseudo-ausencia
    dataset_sf <- rbind(pos_hex, neg_hex)
    dataset_sf$presence <- factor(dataset_sf$presence, levels = c("0", "1"))

    # Asegurarse de que el atributo de warning esté limpio
    attr(dataset_sf, "no_records") <- FALSE
    return(dataset_sf)
  })

  # ❗ GENERAR EL WARNING FUNCIONAL FUERA DEL BLOQUE SILENCIADO
  if (isTRUE(attr(results, "no_records"))) {
    warning("No records found for species: ", species)
  }

  return(results)
}
