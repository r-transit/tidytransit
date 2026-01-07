# Get a set of stops for a given set of service ids and route ids

Get a set of stops for a given set of service ids and route ids

## Usage

``` r
filter_stops(gtfs_obj, service_ids, route_ids)
```

## Arguments

- gtfs_obj:

  gtfs feed (tidygtfs object)

- service_ids:

  the service for which to get stops

- route_ids:

  the route_ids for which to get stops

## Value

stops table for a given service or route

## Examples

``` r
# \donttest{
library(dplyr)
local_gtfs_path <- system.file("extdata", "nyc_subway.zip", package = "tidytransit")
nyc <- read_gtfs(local_gtfs_path)
select_service_id <- filter(nyc$calendar, monday==1) %>% pull(service_id)
select_route_id <- sample_n(nyc$routes, 1) %>% pull(route_id)
filtered_stops_df <- filter_stops(nyc, select_service_id, select_route_id)
# }
```
