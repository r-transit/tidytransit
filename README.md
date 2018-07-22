[![Travis-CI Build Status](https://travis-ci.com/r-gtfs/trread.svg?branch=master)](https://travis-ci.com/r-gtfs/trread)
[![cran version](https://www.r-pkg.org/badges/version/trread)](https://cran.r-project.org/package=trread)

## Description

`trread` is a package for reading the GTFS data standard into R. It can
read directly from URL’s or flat files, and does some validation of the
data structure against the specification.

## Goal

This is a highly simplified fork/piece of the
[gtfsr](https://github.com/ropensci/gtfsr/) package.

The goal is to break that package into parts to simplify maintenance.

## Contributors

Among the many
[contributors](https://github.com/ropensci/gtfsr/graphs/contributors),
[Danton Noriega](https://github.com/dantonnoriega) wrote much of this
package.

## Installation

For now, you can install this package from GitHub using the devtools
package:

    if (!require(devtools)) {
        install.packages('devtools')
    }
    devtools::install_github('r-gtfs/trread')

## Usage

Fetch data for a bus system in Accra, Ghana from GitHub.

``` r
library(trread)
library(dplyr)

accra_gtfs <- import_gtfs("https://github.com/AFDLab4Dev/AccraMobility/raw/master/GTFS/GTFS_Accra.zip")
#> [1] "agency.txt"      "calendar.txt"    "feed_info.txt"   "frequencies.txt"
#> [5] "routes.txt"      "shapes.txt"      "stop_times.txt"  "stops.txt"      
#> [9] "trips.txt"
```

Count and list the number of stops per route.

``` r
attach(accra_gtfs)

routes_df %>% inner_join(trips_df, by="route_id") %>%
  inner_join(stop_times_df) %>% 
    inner_join(stops_df, by="stop_id") %>% 
      group_by(route_long_name) %>%
        summarise(stop_count=n_distinct(stop_id)) %>%
  arrange(desc(stop_count))
#> # A tibble: 271 x 2
#>    route_long_name          stop_count
#>    <chr>                         <int>
#>  1 Kasoa ↔ Accra New Town          116
#>  2 Omanjor ↔ Accra CMB             109
#>  3 Manhean ↔ Accra CMB             105
#>  4 Adeyman ↔ Abeka Lapaz           104
#>  5 Ashongman ↔ Abeka Lapaz         101
#>  6 Nungua ↔ Circle Odorna           91
#>  7 Odorna ↔ Nungua                  91
#>  8 Teshie-Nungua ↔ Achimota         91
#>  9 Accra CMB ↔ Ablekuma             86
#> 10 Kasoa ↔ Korle Bu                 84
#> # ... with 261 more rows
```
