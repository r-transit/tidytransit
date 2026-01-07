# Calculate distances between a given set of stops

Calculate distances between a given set of stops

## Usage

``` r
stop_distances(gtfs_stops)
```

## Arguments

- gtfs_stops:

  gtfs stops table either as data frame (with at least `stop_id`,
  `stop_lon` and `stop_lat` columns) or as `sf` object.

## Value

Returns a data.frame with each row containing a pair of stop_ids
(columns `from_stop_id` and `to_stop_id`) and the `distance` between
them (in meters)

## Note

The resulting data.frame has `nrow(gtfs_stops)^2` rows, distances
calculations among all stops for large feeds should be avoided.

## Examples

``` r
if (FALSE) { # \dontrun{
library(dplyr)

nyc_path <- system.file("extdata", "nyc_subway.zip", package = "tidytransit")
nyc <- read_gtfs(nyc_path)

nyc$stops %>%
  filter(stop_name == "Borough Hall") %>%
  stop_distances() %>%
  arrange(desc(distance))

#> # A tibble: 36 × 3
#>    from_stop_id to_stop_id  distance
#>    <chr>        <chr>          <dbl>
#>  1 423          232             91.5
#>  2 423N         232             91.5
#>  3 423S         232             91.5
#>  4 423          232N            91.5
#>  5 423N         232N            91.5
#>  6 423S         232N            91.5
#>  7 423          232S            91.5
#>  8 423N         232S            91.5
#>  9 423S         232S            91.5
#> 10 232          423             91.5
#> # … with 26 more rows
} # }
```
