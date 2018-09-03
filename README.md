
[![Travis-CI Build
Status](https://travis-ci.com/r-transit/tidytransit.svg?branch=master)](https://travis-ci.com/r-transit/tidytransit)
[![CRAN
status](http://www.r-pkg.org/badges/version-ago/tidytransit?)](https://cran.r-project.org/package=tidytransit)
[![Project Status: Active](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active) 
[![](https://cranlogs.r-pkg.org/badges/tidytransit)](https://cran.r-project.org/package=tidytransit)

# tidytransit

Use tidytransit to map transit stops and routes, calculate transit
frequencies, and validate transit feeds. tidytransit reads the [General Transit Feed Specification](http://gtfs.org/) into [tidyverse](https://tibble.tidyverse.org/) and [simple features](https://en.wikipedia.org/wiki/Simple_Features) dataframes.

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

# Usage

Please see:

- [the tutorial](http://tidytransit.r-transit.org/articles/introduction.html)   
- [the reference](http://tidytransit.r-transit.org/reference/index.html).   


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
