# Calculate service pattern ids for a GTFS feed

Each trip has a defined number of dates it runs on. This set of dates is
called a service pattern in tidytransit. Trips with the same
`servicepattern` id run on the same dates. In general, `service_id` can
work this way but it is not enforced by the GTFS standard.

## Usage

``` r
set_servicepattern(
  gtfs_obj,
  id_prefix = "s_",
  hash_algo = "md5",
  hash_length = 7
)
```

## Arguments

- gtfs_obj:

  gtfs feed (tidygtfs object)

- id_prefix:

  all servicepattern ids will start with this string

- hash_algo:

  hashing algorithm used by
  [`digest::digest()`](https://eddelbuettel.github.io/digest/man/digest.html)

- hash_length:

  length the hash should be cut to with
  [`substr()`](https://rdrr.io/r/base/substr.html). Use `-1` if the full
  hash should be used

## Value

modified gtfs_obj with added servicepattern list and a table linking
trips and pattern (trip_servicepatterns), added to `gtfs_obj$.` sublist.

## Examples

``` r
gtfs = set_servicepattern(gtfs_duke)
gtfs$.$dates_servicepatterns
#> # A tibble: 463 × 2
#>    date       servicepattern_id
#>    <date>     <chr>            
#>  1 2019-07-05 s_7e264b5        
#>  2 2019-07-08 s_7e264b5        
#>  3 2019-07-09 s_7e264b5        
#>  4 2019-07-10 s_7e264b5        
#>  5 2019-07-11 s_7e264b5        
#>  6 2019-07-12 s_7e264b5        
#>  7 2019-07-15 s_7e264b5        
#>  8 2019-07-16 s_7e264b5        
#>  9 2019-07-17 s_7e264b5        
#> 10 2019-07-18 s_7e264b5        
#> # ℹ 453 more rows
```
