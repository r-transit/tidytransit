# Convert shapes into Simple Features Linestrings

Convert shapes into Simple Features Linestrings

## Usage

``` r
shapes_as_sf(gtfs_shapes, crs = NULL)
```

## Arguments

- gtfs_shapes:

  a gtfs\$shapes dataframe

- crs:

  optional coordinate reference system (used by
  [`sf::st_transform()`](https://r-spatial.github.io/sf/reference/st_transform.html))
  to transform lon/lat coordinates

## Value

an sf dataframe for gtfs shapes

## See also

[`gtfs_as_sf`](https://r-transit.github.io/tidytransit/reference/gtfs_as_sf.md)
