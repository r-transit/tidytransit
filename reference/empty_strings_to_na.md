# Convert empty strings ("") to NA values in all gtfs tables

[`read_gtfs()`](https://r-transit.github.io/tidytransit/reference/read_gtfs.md)
converts all empty strings to `NA` values

## Usage

``` r
empty_strings_to_na(gtfs_obj)
```

## Arguments

- gtfs_obj:

  gtfs feed (tidygtfs object)

## Value

a gtfs_obj where all empty strings in tables have been replaced with NA

## See also

[`na_to_empty_strings()`](https://r-transit.github.io/tidytransit/reference/na_to_empty_strings.md)
