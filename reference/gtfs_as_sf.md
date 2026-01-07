# Convert stops and shapes to Simple Features

Stops are converted to POINT sf data frames. Shapes are converted to a
LINESTRING data frame. Note that this function replaces stops and shapes
tables in `gtfs_obj`.

## Usage

``` r
gtfs_as_sf(gtfs_obj, skip_shapes = FALSE, crs = NULL, quiet = TRUE)
```

## Arguments

- gtfs_obj:

  gtfs feed (tidygtfs object, created by
  [`read_gtfs()`](https://r-transit.github.io/tidytransit/reference/read_gtfs.md))

- skip_shapes:

  if TRUE, shapes are not converted. Default FALSE.

- crs:

  optional coordinate reference system (used by
  [`sf::st_transform()`](https://r-spatial.github.io/sf/reference/st_transform.html))
  to transform lon/lat coordinates of stops and shapes

- quiet:

  boolean whether to print status messages

## Value

tidygtfs object with stops and shapes as sf dataframes

## See also

[`sf_as_tbl`](https://r-transit.github.io/tidytransit/reference/sf_as_tbl.md),
[`stops_as_sf`](https://r-transit.github.io/tidytransit/reference/stops_as_sf.md),
[`shapes_as_sf`](https://r-transit.github.io/tidytransit/reference/shapes_as_sf.md)
