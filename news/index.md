# Changelog

## h3sdm 0.1.1

### New functions

- [`h3sdm_pa_from_records()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_pa_from_records.md):
  generates a presence/pseudo-absence dataset from user-provided
  records. Accepts a `data.frame` or `sf` object with coordinates in
  WGS84 (EPSG:4326). Supports optional filtering by `geospatialKosher`
  column, making it directly compatible with records downloaded via
  `bdcr_occurrences()` from the BiodataCR API.

## h3sdm 0.1.0

CRAN release: 2026-04-15

- Initial CRAN submission.
