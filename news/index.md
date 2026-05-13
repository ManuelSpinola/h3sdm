# Changelog

## h3sdm 0.1.1

CRAN release: 2026-05-13

### New functions

- [`h3sdm_pa_from_records()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_pa_from_records.md):
  generates a presence/pseudo-absence dataset from user-provided
  records. Accepts a `data.frame` or `sf` object with coordinates in
  WGS84 (EPSG:4326). Supports optional filtering by a `geospatialKosher`
  column to remove records with questionable spatial quality.

- [`h3sdm_count_from_records()`](https://manuelspinola.github.io/h3sdm/reference/h3sdm_count_from_records.md):
  generates a hexagonal grid with count-based response variables
  (species richness, total detections, or individual abundance) from
  user-provided records. Accepts a `data.frame` or `sf` object. Supports
  optional filtering by presence column, confidence threshold, and date
  range.

## h3sdm 0.1.0

CRAN release: 2026-04-15

- Initial CRAN submission.
