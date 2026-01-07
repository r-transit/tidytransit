# Filter a `stop_times` table for a given date and timespan.

Filter a `stop_times` table for a given date and timespan.

## Usage

``` r
filter_stop_times(gtfs_obj, extract_date, min_departure_time, max_arrival_time)
```

## Arguments

- gtfs_obj:

  gtfs feed (tidygtfs object)

- extract_date:

  date to extract trips from this day (Date or "YYYY-MM-DD" string)

- min_departure_time:

  (optional) The earliest departure time. Can be given as "HH:MM:SS",
  hms object or numeric value in seconds.

- max_arrival_time:

  (optional) The latest arrival time. Can be given as "HH:MM:SS", hms
  object or numeric value in seconds.

## Value

Filtered `stop_times` data.table for
[`travel_times()`](https://r-transit.github.io/tidytransit/reference/travel_times.md)
and
[`raptor()`](https://r-transit.github.io/tidytransit/reference/raptor.md).

## Examples

``` r
feed_path <- system.file("extdata", "routing.zip", package = "tidytransit")
g <- read_gtfs(feed_path)

# filter the sample feed
stop_times <- filter_stop_times(g, "2018-10-01", "06:00:00", "08:00:00")
```
