# Get Route Frequency

Calculate the number of departures and mean headways for routes within a
given timespan and for given service_ids.

## Usage

``` r
get_route_frequency(
  gtfs_obj,
  start_time = "06:00:00",
  end_time = "22:00:00",
  service_ids = NULL
)
```

## Arguments

- gtfs_obj:

  gtfs feed (tidygtfs object)

- start_time:

  analysis start time, can be given as "HH:MM:SS", hms object or numeric
  value in seconds.

- end_time:

  analysis period end time, can be given as "HH:MM:SS", hms object or
  numeric value in seconds.

- service_ids:

  A set of service_ids from the calendar dataframe identifying a
  particular service id. If not provided, the service_id with the most
  departures is used.

## Value

a dataframe of routes with variables or headway/frequency in seconds for
a route within a given time frame

## Note

Some GTFS feeds contain a frequency data frame already. Consider using
this instead, as it will be more accurate than what tidytransit
calculates.

## Examples

``` r
data(gtfs_duke)
routes_frequency <- get_route_frequency(gtfs_duke)
x <- order(routes_frequency$median_headways)
head(routes_frequency[x,])
#> # A tibble: 3 × 6
#>   route_id total_departures median_headways mean_headways st_dev_headways
#>   <chr>               <int>           <dbl>         <dbl>           <dbl>
#> 1 1679                  513            1440          2771           4940.
#> 2 1690                 1242            1477          1403            223.
#> 3 1683                  644            2590          4515           4390.
#> # ℹ 1 more variable: stop_count <int>
```
