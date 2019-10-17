[![Travis-CI Build
Status](https://travis-ci.com/r-transit/tidytransit.svg?branch=master)](https://travis-ci.com/r-transit/tidytransit)
[![CRAN
status](http://www.r-pkg.org/badges/version-ago/tidytransit?)](https://cran.r-project.org/package=tidytransit)
[![Project Status: Active](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active) 
[![](https://cranlogs.r-pkg.org/badges/tidytransit)](https://cran.r-project.org/package=tidytransit)
[![](https://cranlogs.r-pkg.org/badges/grand-total/tidytransit?color=lightgrey)](https://cran.r-project.org/package=tidytransit)

# tidytransit

Use tidytransit to map transit stops and routes, calculate transit
frequencies, and validate transit feeds. tidytransit reads the 
[General Transit Feed Specification](http://gtfs.org/) into 
[tidyverse](https://tibble.tidyverse.org/) and 
[simple features](https://en.wikipedia.org/wiki/Simple_Features) data frames. 
Tidytransit can be used to:
- [read GTFS feeds into R](http://tidytransit.r-transit.org/reference/read_gtfs.html)
- [estimate transit frequencies](http://tidytransit.r-transit.org/reference/get_route_frequency.html)
- [calculate travel times between transit stops](http://tidytransit.r-transit.org/reference/travel_times.html)
- [convert stops and routes to sf data frames](http://tidytransit.r-transit.org/reference/gtfs_as_sf.html)
- map existing stops and routes
- [validate transit feeds and more](http://tidytransit.r-transit.org/reference/index.html)

Have a look at the following vignettes to see how tidytransit can be used to analyse a feed:

- [the tutorial](http://tidytransit.r-transit.org/articles/introduction.html) 
- [introduction to service patterns](http://tidytransit.r-transit.org/articles/servicepatterns.html)
- [introduction to time tables](http://tidytransit.r-transit.org/articles/timetable.html)
- [introduction to frequency calculation](http://tidytransit.r-transit.org/articles/frequency.html)  

## Installation

This package requires a working installation of
[sf](https://github.com/r-spatial/sf#installing).

A CRAN version is available:

``` r
install.packages('tidytransit')
```

For the development version from Github:

```
# install.packages("devtools")
devtools::install_github("r-transit/tidytransit")
```

For some users, `sf` is impractical to install due to system level
dependencies. For these users,
[`trread`](https://github.com/r-transit/trread) may work better. It has
more limited functionality, but it can read GTFS tables into R.

# Background

tidytransit is a
[fork](https://en.wikipedia.org/wiki/Fork_\(software_development\)) of
[gtfsr](https://github.com/ropensci/gtfsr), published to
[CRAN](https://cran.r-project.org/), with frequency/headway calculation
functions. 

# Contributing

If you would like to contribute please feel free to issue a pull request or [open an issue](https://github.com/r-transit/tidytransit/issues/new).

# Contributors

Among the many
[contributors](https://github.com/r-transit/tidytransit/graphs/contributors),
[Danton Noriega](https://github.com/dantonnoriega) wrote much of this
package.
