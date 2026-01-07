# Adds the coordinates of an sf LINESTRING object as columns and rows

Adds the coordinates of an sf LINESTRING object as columns and rows

## Usage

``` r
sf_lines_to_df(
  lines_sf,
  coord_colnames = c("shape_pt_lon", "shape_pt_lat"),
  remove_geometry = TRUE
)
```

## Arguments

- lines_sf:

  sf object

- coord_colnames:

  names of the new columns (existing columns are overwritten)

- remove_geometry:

  remove sf geometry column?
