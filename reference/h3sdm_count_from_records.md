# Generate species richness or abundance count dataset from records

Takes a user-provided dataset with presence records (from Excel,
fieldwork, acoustic detections, camera traps, or any other source) and
generates a hexagonal grid with counts (species richness, total
detections, or individuals) ready for analysis with h3sdm. The input can
be a `data.frame` with coordinate columns or an `sf` object. Coordinates
are assumed to be in WGS84 (EPSG:4326).

## Usage

``` r
h3sdm_count_from_records(
  records,
  aoi_sf,
  res = 7,
  expand_factor = 0.1,
  lon_col = "x",
  lat_col = "y",
  species_col = NULL,
  count_type = c("richness", "detections", "individuals"),
  presence_col = NULL,
  abundance_col = NULL,
  confidence_col = NULL,
  confidence_threshold = NULL,
  date_col = NULL,
  date_min = NULL,
  date_max = NULL
)
```

## Arguments

- records:

  `data.frame` or `sf` object containing records.

- aoi_sf:

  `sf` AOI (area of interest) polygon.

- res:

  `integer` H3 resolution for the hexagonal grid. Default `7`.

- expand_factor:

  `numeric` Factor to expand AOI before creating hex grid. Default
  `0.1`.

- lon_col:

  `character` Name of the longitude column. Ignored if `records` is
  already an `sf` object. Default `"x"`.

- lat_col:

  `character` Name of the latitude column. Ignored if `records` is
  already an `sf` object. Default `"y"`.

- species_col:

  `character` Name of the column containing species names. Required when
  `count_type = "richness"`.

- count_type:

  `character` One of `"richness"` (number of unique species per
  hexagon), `"detections"` (total number of records per hexagon), or
  `"individuals"` (sum of a numeric abundance column per hexagon).
  Default `"richness"`.

- presence_col:

  `character` Optional. Name of the column indicating presence (1) or
  absence (0). If provided, only records with value == 1 are used.

- abundance_col:

  `character` Required when `count_type = "individuals"`. Name of the
  column with individual counts to sum per hexagon.

- confidence_col:

  `character` Optional. Name of the column with detection confidence
  scores (numeric between 0 and 1). Useful for acoustic detection data
  (e.g. BirdNET output).

- confidence_threshold:

  `numeric` Optional. Minimum confidence score to retain a record.
  Ignored if `confidence_col` is `NULL`.

- date_col:

  `character` Optional. Name of the date column. The column must be of
  class `Date`. If your dates are stored as Excel numeric values,
  convert them first with `as.Date(datos$Fecha, origin = "1899-12-30")`.

- date_min:

  `character` or `Date` Optional. Minimum date to retain records
  (inclusive). Format `"YYYY-MM-DD"`.

- date_max:

  `character` or `Date` Optional. Maximum date to retain records
  (inclusive). Format `"YYYY-MM-DD"`.

## Value

`sf` object with columns:

- h3_address:

  H3 index of the hexagon.

- count:

  Numeric count per hexagon (richness, detections, or individuals).

- geometry:

  MULTIPOLYGON of each hexagon.

## Examples

``` r
# \donttest{
data(cr_outline_c, package = "h3sdm")

my_records <- data.frame(
  x         = c(-84.1, -84.2, -83.9, -84.0, -84.1),
  y         = c(9.9, 10.1, 9.8, 9.95, 10.0),
  Especie   = c("Ara macao", "Ara macao", "Pharomachrus mocinno",
                "Tapirus bairdii", "Ara macao"),
  Presencia = c(1, 1, 1, 1, 0)
)

richness_hex <- h3sdm_count_from_records(
  records      = my_records,
  aoi_sf       = cr_outline_c,
  res          = 7,
  lon_col      = "x",
  lat_col      = "y",
  species_col  = "Especie",
  count_type   = "richness",
  presence_col = "Presencia"
)
# }
```
