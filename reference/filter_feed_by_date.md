# Filter a gtfs feed so that it only contains trips running on a given date

Only stop_times, stops, routes, services (in calendar and
calendar_dates), shapes, frequencies and transfers belonging to one of
those trips are kept.

## Usage

``` r
filter_feed_by_date(
  gtfs_obj,
  extract_date,
  min_departure_time,
  max_arrival_time
)
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

tidygtfs object with filtered tables

## See also

[`filter_feed_by_area()`](https://r-transit.github.io/tidytransit/reference/filter_feed_by_area.md),
[`filter_feed_by_stops()`](https://r-transit.github.io/tidytransit/reference/filter_feed_by_stops.md),
[`filter_feed_by_trips()`](https://r-transit.github.io/tidytransit/reference/filter_feed_by_trips.md)
