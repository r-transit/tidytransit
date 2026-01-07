# Convert stops into Simple Features Points

Convert stops into Simple Features Points

## Usage

``` r
stops_as_sf(stops, crs = NULL)
```

## Arguments

- stops:

  a gtfs\$stops dataframe

- crs:

  optional coordinate reference system (used by
  [`sf::st_transform()`](https://r-spatial.github.io/sf/reference/st_transform.html))
  to transform lon/lat coordinates

## Value

an sf dataframe for gtfs routes with a point column

## See also

[`gtfs_as_sf`](https://r-transit.github.io/tidytransit/reference/gtfs_as_sf.md)

## Examples

``` r
data(gtfs_duke)
some_stops <- gtfs_duke$stops[sample(nrow(gtfs_duke$stops), 40),]
some_stops_sf <- stops_as_sf(some_stops)
plot(some_stops_sf[,"stop_name"])
```
