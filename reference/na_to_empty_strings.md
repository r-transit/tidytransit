# Convert NA values to empty strings ("")

[`write_gtfs()`](https://r-transit.github.io/tidytransit/reference/write_gtfs.md)
converts `NA` to empty strings

## Usage

``` r
na_to_empty_strings(gtfs_obj)
```

## Arguments

- gtfs_obj:

  gtfs feed (tidygtfs object)

## Value

a gtfs_obj where all NA strings in tables have been replaced with ""

## See also

[`empty_strings_to_na()`](https://r-transit.github.io/tidytransit/reference/empty_strings_to_na.md)
