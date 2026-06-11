# Costa Rica Continental Outline

An `sf` polygon containing the continental outline of Costa Rica,
derived from GADM 4.1. The Isla del Coco and all other minor oceanic
islands have been removed, retaining only the largest polygon (the
continental landmass).

For the full outline including all islands, see
[`cr_outline`](https://manuelspinola.github.io/h3sdm/reference/cr_outline.md).

## Usage

``` r
cr_outline_c
```

## Format

An `sf` object with 1 feature and 1 column:

- geometry:

  POLYGON in WGS 84 (EPSG:4326) with 30,261 vertices, representing the
  continental outline of Costa Rica.

## Source

Global Administrative Areas (GADM) version 4.1. Downloaded via
`geodata::gadm("CRI", level = 0)`. <https://gadm.org>

## Details

### Island removal

Costa Rica includes the Isla del Coco (~550 km offshore in the Pacific),
which is excluded here. The continental polygon is obtained by casting
the GADM multipolygon to individual polygons and retaining the one with
the largest area. For analyses requiring all national territory use
[`cr_outline`](https://manuelspinola.github.io/h3sdm/reference/cr_outline.md).

### Reproducibility

Generated with `data-raw/cr_outline.R`. To regenerate:

    source("data-raw/cr_outline.R")

## See also

- [`cr_outline`](https://manuelspinola.github.io/h3sdm/reference/cr_outline.md)
  – full outline including all islands.

## Examples

``` r
data(cr_outline_c)
plot(sf::st_geometry(cr_outline_c), main = "Costa Rica (continental)")
```
