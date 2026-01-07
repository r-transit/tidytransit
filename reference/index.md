# Package index

## Read and write GTFS feeds

- [`read_gtfs()`](https://r-transit.github.io/tidytransit/reference/read_gtfs.md)
  : Read and validate GTFS files
- [`write_gtfs()`](https://r-transit.github.io/tidytransit/reference/write_gtfs.md)
  : Write a tidygtfs object to a zip file

## Travel time calculations

- [`travel_times()`](https://r-transit.github.io/tidytransit/reference/travel_times.md)
  : Calculate shortest travel times from a stop to all reachable stops

- [`raptor()`](https://r-transit.github.io/tidytransit/reference/raptor.md)
  : Calculate travel times from one stop to all reachable stops

- [`filter_stop_times()`](https://r-transit.github.io/tidytransit/reference/filter_stop_times.md)
  :

  Filter a `stop_times` table for a given date and timespan.

## Filter GTFS objects

- [`filter_feed_by_date()`](https://r-transit.github.io/tidytransit/reference/filter_feed_by_date.md)
  : Filter a gtfs feed so that it only contains trips running on a given
  date
- [`filter_feed_by_area()`](https://r-transit.github.io/tidytransit/reference/filter_feed_by_area.md)
  : Filter a gtfs feed so that it only contains trips that pass a given
  area
- [`filter_feed_by_stops()`](https://r-transit.github.io/tidytransit/reference/filter_feed_by_stops.md)
  : Filter a gtfs feed so that it only contains trips that pass the
  given stops
- [`filter_feed_by_trips()`](https://r-transit.github.io/tidytransit/reference/filter_feed_by_trips.md)
  : Filter a gtfs feed so that it only contains a given set of trips
- [`filter_stops()`](https://r-transit.github.io/tidytransit/reference/filter_stops.md)
  : Get a set of stops for a given set of service ids and route ids

## Analyse GTFS objects

- [`validate_gtfs()`](https://r-transit.github.io/tidytransit/reference/validate_gtfs.md)
  : Validate GTFS feed
- [`set_servicepattern()`](https://r-transit.github.io/tidytransit/reference/set_servicepattern.md)
  : Calculate service pattern ids for a GTFS feed
- [`get_route_frequency()`](https://r-transit.github.io/tidytransit/reference/get_route_frequency.md)
  : Get Route Frequency
- [`get_stop_frequency()`](https://r-transit.github.io/tidytransit/reference/get_stop_frequency.md)
  : Get Stop Frequency
- [`summary(`*`<tidygtfs>`*`)`](https://r-transit.github.io/tidytransit/reference/summary.tidygtfs.md)
  : GTFS feed summary

## Find and fix inconsistencies in feeds

- [`interpolate_stop_times()`](https://r-transit.github.io/tidytransit/reference/interpolate_stop_times.md)
  : Interpolate missing stop_times linearly
- [`cluster_stops()`](https://r-transit.github.io/tidytransit/reference/cluster_stops.md)
  : Cluster nearby stops within a group
- [`stop_group_distances()`](https://r-transit.github.io/tidytransit/reference/stop_group_distances.md)
  : Calculates distances among stop within the same group column
- [`stop_distances()`](https://r-transit.github.io/tidytransit/reference/stop_distances.md)
  : Calculate distances between a given set of stops

## Geospatial functions (sf)

- [`gtfs_as_sf()`](https://r-transit.github.io/tidytransit/reference/gtfs_as_sf.md)
  : Convert stops and shapes to Simple Features
- [`gtfs_transform()`](https://r-transit.github.io/tidytransit/reference/gtfs_transform.md)
  : Transform coordinates of a gtfs feed
- [`shapes_as_sf()`](https://r-transit.github.io/tidytransit/reference/shapes_as_sf.md)
  : Convert shapes into Simple Features Linestrings
- [`stops_as_sf()`](https://r-transit.github.io/tidytransit/reference/stops_as_sf.md)
  : Convert stops into Simple Features Points
- [`get_trip_geometry()`](https://r-transit.github.io/tidytransit/reference/get_trip_geometry.md)
  : Get all trip shapes for given trip ids
- [`get_route_geometry()`](https://r-transit.github.io/tidytransit/reference/get_route_geometry.md)
  : Get all trip shapes for a given route and service
- [`sf_as_tbl()`](https://r-transit.github.io/tidytransit/reference/sf_as_tbl.md)
  : Convert stops and shapes from sf objects to tibbles

## Helpers

- [`as_tidygtfs()`](https://r-transit.github.io/tidytransit/reference/as_tidygtfs.md)
  : Convert another gtfs like object to a tidygtfs object
- [`na_to_empty_strings()`](https://r-transit.github.io/tidytransit/reference/na_to_empty_strings.md)
  : Convert NA values to empty strings ("")
- [`empty_strings_to_na()`](https://r-transit.github.io/tidytransit/reference/empty_strings_to_na.md)
  : Convert empty strings ("") to NA values in all gtfs tables
- [`plot(`*`<tidygtfs>`*`)`](https://r-transit.github.io/tidytransit/reference/plot.tidygtfs.md)
  : Plot GTFS stops and trips
- [`print(`*`<tidygtfs>`*`)`](https://r-transit.github.io/tidytransit/reference/print.tidygtfs.md)
  : Print a GTFS object

## Datasets

- [`gtfs_duke`](https://r-transit.github.io/tidytransit/reference/gtfs_duke.md)
  : Example GTFS data
- [`route_type_names`](https://r-transit.github.io/tidytransit/reference/route_type_names.md)
  : Dataframe of route type id's and the names of the types (e.g. "Bus")
