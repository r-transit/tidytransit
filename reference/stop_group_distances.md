# Calculates distances among stop within the same group column

By default calculates distances among stop_ids with the same stop_name.

## Usage

``` r
stop_group_distances(gtfs_stops, by = "stop_name", max_only = FALSE)
```

## Arguments

- gtfs_stops:

  gtfs stops table either as data frame (with at least `stop_id`,
  `stop_lon` and `stop_lat` columns) or as `sf` object.

- by:

  group column, default: "stop_name"

- max_only:

  only return max distance among stops? (default `FALSE`). `TRUE` allows
  a slightly faster calculation.

## Value

data.frame with one row per group containing a distance matrix
(distances), number of stop ids within that group (n_stop_ids) and
distance summary values (dist_mean, dist_median and dist_max).

## Examples

``` r
if (FALSE) { # \dontrun{
library(dplyr)

nyc_path <- system.file("extdata", "nyc_subway.zip", package = "tidytransit")
nyc <- read_gtfs(nyc_path)

stop_group_distances(nyc$stops)
#> # A tibble: 380 × 6
#>    stop_name   distances       n_stop_ids dist_mean dist_median dist_max
#>    <chr>       <list>               <dbl>     <dbl>       <dbl>    <dbl>
#>  1 86 St       <dbl [18 × 18]>         18     5395.       5395.   21811.
#>  2 79 St       <dbl [6 × 6]>            6    19053.      19053.   19053.
#>  3 Prospect Av <dbl [6 × 6]>            6    18804.      18804.   18804.
#>  4 77 St       <dbl [6 × 6]>            6    16947.      16947.   16947.
#>  5 59 St       <dbl [6 × 6]>            6    14130.      14130.   14130.
#>  6 50 St       <dbl [9 × 9]>            9     7097.       7097.   14068.
#>  7 36 St       <dbl [6 × 6]>            6    12496.      12496.   12496.
#>  8 8 Av        <dbl [6 × 6]>            6    11682.      11682.   11682.
#>  9 7 Av        <dbl [9 × 9]>            9     5479.       5479.   10753.
#> 10 111 St      <dbl [9 × 9]>            9     3877.       3877.    7753.
#> # … with 370 more rows
} # }
```
