# Filter a gtfs feed so that it only contains a given set of trips

Only stop_times, stops, routes, services (in calendar and
calendar_dates), shapes, frequencies and transfers belonging to one of
those trips are kept.

## Usage

``` r
filter_feed_by_trips(gtfs_obj, trip_ids)
```

## Arguments

- gtfs_obj:

  gtfs feed (tidygtfs object)

- trip_ids:

  vector with trip_ids

## Value

tidygtfs object with filtered tables

## See also

[`filter_feed_by_date()`](https://r-transit.github.io/tidytransit/reference/filter_feed_by_date.md),
[`filter_feed_by_area()`](https://r-transit.github.io/tidytransit/reference/filter_feed_by_area.md),
[`filter_feed_by_stops()`](https://r-transit.github.io/tidytransit/reference/filter_feed_by_stops.md)
