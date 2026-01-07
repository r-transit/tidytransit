# Filter a gtfs feed so that it only contains trips that pass the given stops

Only stop_times, stops, routes, services (in calendar and
calendar_dates), shapes, frequencies and transfers belonging to one of
those trips are kept.

## Usage

``` r
filter_feed_by_stops(gtfs_obj, stop_ids = NULL, stop_names = NULL)
```

## Arguments

- gtfs_obj:

  gtfs feed (tidygtfs object)

- stop_ids:

  vector with stop_ids. You can either provide stop_ids or stop_names

- stop_names:

  vector with stop_names (will be converted to stop_ids)

## Value

tidygtfs object with filtered tables

## Note

The returned gtfs_obj likely contains more than just the stops given
(i.e. all stops that belong to a trip passing the initial stop).

## See also

[`filter_feed_by_date()`](https://r-transit.github.io/tidytransit/reference/filter_feed_by_date.md),
[`filter_feed_by_area()`](https://r-transit.github.io/tidytransit/reference/filter_feed_by_area.md),
[`filter_feed_by_trips()`](https://r-transit.github.io/tidytransit/reference/filter_feed_by_trips.md)
