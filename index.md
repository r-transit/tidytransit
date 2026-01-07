# tidytransit

Use tidytransit to map transit stops and routes, calculate travel times
and transit frequencies, and validate transit feeds. Tidytransit reads
the [General Transit Feed Specification](https://gtfs.org/) into
[tidyverse](https://tibble.tidyverse.org/) and [simple
features](https://en.wikipedia.org/wiki/Simple_Features) data frames.
Tidytransit can be used to:

- [read GTFS feeds into
  R](https://r-transit.github.io/tidytransit/reference/read_gtfs.html)
- [calculate travel times between transit
  stops](https://r-transit.github.io/tidytransit/reference/travel_times.html)
- [convert stops and routes to sf data
  frames](https://r-transit.github.io/tidytransit/reference/gtfs_as_sf.html)
- [validate transit feeds and
  more](https://r-transit.github.io/tidytransit/reference/index.html)

Have a look at the following vignettes to see how tidytransit can be
used to analyse a feed:

- [introduction](https://r-transit.github.io/tidytransit/articles/introduction.html)
- [calendar and service
  patterns](https://r-transit.github.io/tidytransit/articles/servicepatterns.html)
- [create time tables for
  stops](https://r-transit.github.io/tidytransit/articles/timetable.html)
- [frequency and headway
  calculations](https://r-transit.github.io/tidytransit/articles/frequency.html)

## Installation

This package requires a working installation of
[sf](https://github.com/r-spatial/sf#installing).

Install tidytransit from CRAN:

``` r
install.packages('tidytransit')
```

For the development version from Github:

``` r
# install.packages("remotes")
remotes::install_github("r-transit/tidytransit")
```

## GTFS-related packages

- [`gtfsio`](https://github.com/r-transit/gtfsio) R package to read and
  write gtfs feeds, tidytransit uses gtfsio for reading/writing feeds
- [`gtfstools`](https://github.com/ipeaGIT/gtfstools) Tools for editing
  and analysing transit feeds
- [`gtfsrouter`](https://github.com/UrbanAnalyst/gtfsrouter) Package for
  public transport routing
- [`gtfs2gps`](https://github.com/ipeaGIT/gtfs2gps) Converting public
  transport data from GTFS format to GPS-like records
- [`GTFSwizard`](https://github.com/nelsonquesado/GTFSwizard) Set of
  tools for exploring and manipulating, builds on tidytransit

## Contributing

Please feel free to issue a pull request or [open an
issue](https://github.com/r-transit/tidytransit/issues/new).
