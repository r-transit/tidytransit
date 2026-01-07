# Adds the coordinates of an sf POINT object as columns

Adds the coordinates of an sf POINT object as columns

## Usage

``` r
sf_points_to_df(
  pts_sf,
  coord_colnames = c("stop_lon", "stop_lat"),
  remove_geometry = TRUE
)
```

## Arguments

- pts_sf:

  sf object

- coord_colnames:

  names of the new columns (existing columns are overwritten)

- remove_geometry:

  remove sf geometry column?
