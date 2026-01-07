# Filter a gtfs feed so that it only contains trips that pass a given area

Only stop_times, stops, routes, services (in calendar and
calendar_dates), shapes, frequencies and transfers belonging to one of
those trips are kept.

## Usage

``` r
filter_feed_by_area(gtfs_obj, area)
```

## Arguments

- gtfs_obj:

  gtfs feed (tidygtfs object)

- area:

  all trips passing through this area are kept. Either a bounding box
  (numeric vector with xmin, ymin, xmax, ymax) or a sf object.

## Value

tidygtfs object with filtered tables

## See also

[`filter_feed_by_date()`](https://r-transit.github.io/tidytransit/reference/filter_feed_by_date.md),
[`filter_feed_by_stops()`](https://r-transit.github.io/tidytransit/reference/filter_feed_by_stops.md),
[`filter_feed_by_trips()`](https://r-transit.github.io/tidytransit/reference/filter_feed_by_trips.md)
