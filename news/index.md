# Changelog

## tidytransit 1.8.0

- Empty strings in txt files are always parsed as `NA` in
  [`read_gtfs()`](https://r-transit.github.io/tidytransit/reference/read_gtfs.md)
  [\#229](https://github.com/r-transit/tidytransit/pull/229)
- [`raptor()`](https://r-transit.github.io/tidytransit/reference/raptor.md)
  and
  [`travel_times()`](https://r-transit.github.io/tidytransit/reference/travel_times.md)
  now support in-seat transfers
  [\#225](https://github.com/r-transit/tidytransit/pull/225)
- Updated documentation on authenticated downloads
  [\#224](https://github.com/r-transit/tidytransit/pull/224)
- The parameter `max_departure_time` in
  [`travel_times()`](https://r-transit.github.io/tidytransit/reference/travel_times.md)
  is no longer supported
- Improved error handling

## tidytransit 1.7.1

CRAN release: 2025-08-21

- update gtfs reference sysdata
  [\#221](https://github.com/r-transit/tidytransit/pull/221)
- use fallback MobilityData dataset (extdata) in vignette if download
  fails
  [9e352eb](https://github.com/r-transit/tidytransit/commit/9e352ebc63adeb5f9bc540e2d56c432ddcb75f57)
- bugfix: `by` parameter in
  [`stop_group_distances()`](https://r-transit.github.io/tidytransit/reference/stop_group_distances.md)
  is no longer ignored
  [\#222](https://github.com/r-transit/tidytransit/pull/222)
- bugfix: catch empty/NA parameter values in
  [`filter_stop_times()`](https://r-transit.github.io/tidytransit/reference/filter_stop_times.md)

## tidytransit 1.7.0

CRAN release: 2024-10-18

- [`read_gtfs()`](https://r-transit.github.io/tidytransit/reference/read_gtfs.md)
  can now read `locations.geojson` files according to the updated
  specifications
  [\#214](https://github.com/r-transit/tidytransit/pull/214)
- tidytransit 1.7.0 needs `gtfsio` 1.2.0 as it uses json reading
  capabilities from
  [`gtfsio::import_gtfs()`](https://r-transit.github.io/gtfsio/reference/import_gtfs.html)
- Internally, the automatically parsed specifications from
  <https://gtfs.org/documentation/schedule/reference/> are now used
- The example feed `google_transit_nyc_subway.zip` has been modified and
  renamed to `nyc_subway.zip`
  [\#218](https://github.com/r-transit/tidytransit/pull/218)

## tidytransit 1.6.1

CRAN release: 2023-12-07

- fix: Add `fare_media_id` to `fare_products` in spec.R
  [\#207](https://github.com/r-transit/tidytransit/pull/207)
- compare values, ignore attributes for sf object tests
  [\#211](https://github.com/r-transit/tidytransit/pull/211)
- Fix tests for CRAN and update contributors
  [\#212](https://github.com/r-transit/tidytransit/pull/212)

## tidytransit 1.6.0

CRAN release: 2023-06-23

- update raptor() and travel_times(), add interpolate_stop_times()
  [\#204](https://github.com/r-transit/tidytransit/pull/204)
- depend on dplyr \>= 1.1.1
  [\#205](https://github.com/r-transit/tidytransit/pull/205)

## tidytransit 1.5.1

CRAN release: 2023-05-01

- Improve duplicated primary key check
  [\#203](https://github.com/r-transit/tidytransit/pull/203)

## tidytransit 1.5.0

CRAN release: 2023-03-10

- introduce `as_tidygtfs` and primary key check
  [\#198](https://github.com/r-transit/tidytransit/pull/198)
- `tidygtfs`inherits list class
  [\#202](https://github.com/r-transit/tidytransit/pull/202)
- Use `NA` for empty arrival/departure time strings
  [\#196](https://github.com/r-transit/tidytransit/pull/196)
- pass on `...` argument in `read_gtfs`to
  [`gtfsio::import_gtfs`](https://r-transit.github.io/gtfsio/reference/import_gtfs.html)
  [\#199](https://github.com/r-transit/tidytransit/pull/199)
- Fix for dplyr 1.1.1
  [\#197](https://github.com/r-transit/tidytransit/pull/197)
- depend on gtfsio \>= 1.1.0
  [\#201](https://github.com/r-transit/tidytransit/pull/201)
- update raptor doc
  [\#200](https://github.com/r-transit/tidytransit/pull/200)

## tidytransit 1.4.1

CRAN release: 2023-02-01

- optional transfers in filter_stop_times
  [\#188](https://github.com/r-transit/tidytransit/pull/188)
- Suppress multiple match warning (dplyr 1.1)
  [\#191](https://github.com/r-transit/tidytransit/pull/191)
- Use expect_no_warning, remove httr dependency
  [\#194](https://github.com/r-transit/tidytransit/pull/194)

## tidytransit 1.4.0

- remove transitfeeds API and dataset, add mobilitydata.org to vignette
  [\#184](https://github.com/r-transit/tidytransit/pull/184)
- update URLs [\#185](https://github.com/r-transit/tidytransit/pull/185)
- increase test coverage and update doc
  [\#186](https://github.com/r-transit/tidytransit/pull/186)

## tidytransit 1.3.1

CRAN release: 2022-05-20

- optimize filter_stop_times
  [\#183](https://github.com/r-transit/tidytransit/pull/183)

## tidytransit 1.3.0

CRAN release: 2022-01-31

- fix set_dates_services for feeds only using calendar_dates
  [\#178](https://github.com/r-transit/tidytransit/pull/178)
- only keep transfers where both stops match filter
  [\#180](https://github.com/r-transit/tidytransit/pull/180)
- stop distance analysis and stop_name clustering
  [\#181](https://github.com/r-transit/tidytransit/pull/181)
- functions to convert empty strings to NA and vice versa
  [\#182](https://github.com/r-transit/tidytransit/pull/182)

## tidytransit 1.2.0

CRAN release: 2021-11-23

- Trim dependencies
  [\#171](https://github.com/r-transit/tidytransit/pull/171)
- restore get_route_frequency and vignette
  [\#173](https://github.com/r-transit/tidytransit/pull/173)
- fix github actions
  [\#174](https://github.com/r-transit/tidytransit/pull/174)
- fix ggplot warnings in vignette
- use “tidygtfs” in summary
- files parameter is now passed on to gtfsio::import_gtfs

## tidytransit 1.1.0

CRAN release: 2021-07-26

- added filter_feed_by functions
  [\#168](https://github.com/r-transit/tidytransit/pull/168)
- write_gtfs now correctly converts sf objects to gtfs csv tables (via
  sf_as_tbl) [\#168](https://github.com/r-transit/tidytransit/pull/168)
- added gtfs_transform which runs sf::st_transform on shapes and stops
  [\#168](https://github.com/r-transit/tidytransit/pull/168)
- added trolleybus and monorail to route_types
- update documentation and deployment

## tidytransit 1.0.0

CRAN release: 2021-05-09

- use gtfsio backend
  [\#164](https://github.com/r-transit/tidytransit/pull/164)
