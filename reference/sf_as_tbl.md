# Convert stops and shapes from sf objects to tibbles

Coordinates are transformed to lon/lat columns (`stop_lon`/`stop_lat` or
`shape_pt_lon`/`shape_pt_lat`)

## Usage

``` r
sf_as_tbl(gtfs_obj)
```

## Arguments

- gtfs_obj:

  gtfs feed (tidygtfs object)

## Value

tidygtfs object with stops and shapes converted to tibbles

## See also

[`gtfs_as_sf`](https://r-transit.github.io/tidytransit/reference/gtfs_as_sf.md)
