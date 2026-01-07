# Get Stop Frequency

Calculate the number of departures and mean headways for all stops
within a given timespan and for given service_ids.

## Usage

``` r
get_stop_frequency(
  gtfs_obj,
  start_time = "06:00:00",
  end_time = "22:00:00",
  service_ids = NULL,
  by_route = TRUE
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

- by_route:

  Default TRUE, if FALSE then calculate headway for any line coming
  through the stop in the same direction on the same schedule.

## Value

dataframe of stops with the number of departures and the headway
(departures divided by timespan) in seconds as columns

## Note

Some GTFS feeds contain a frequency data frame already. Consider using
this instead, as it will be more accurate than what tidytransit
calculates.

## Examples

``` r
data(gtfs_duke)
stop_frequency <- get_stop_frequency(gtfs_duke)
x <- order(stop_frequency$mean_headway)
head(stop_frequency[x,])
#> # A tibble: 6 × 6
#>   stop_id route_id direction_id service_id         n_departures mean_headway
#>   <chr>   <chr>           <int> <chr>                     <int>        <dbl>
#> 1 778127  1683                0 c_876_b_21969_d_31          143          403
#> 2 778084  1683                0 c_876_b_21969_d_31          128          450
#> 3 778095  1690                0 c_876_b_21969_d_31           78          738
#> 4 778134  1679                0 c_876_b_21969_d_31           76          758
#> 5 778058  1690                0 c_876_b_21969_d_31           75          768
#> 6 778102  1690                0 c_876_b_21969_d_31           75          768
```
