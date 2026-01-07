# Returns TRUE if the given gtfs_obj contains the table in tidytransit's "calculated tables sublist" (`gtfs_obj$.`)

Returns TRUE if the given gtfs_obj contains the table in tidytransit's
"calculated tables sublist" (`gtfs_obj$.`)

## Usage

``` r
feed_contains.(gtfs_obj, table_name)
```

## Arguments

- gtfs_obj:

  gtfs feed (tidygtfs object)

- table_name:

  name of the table to look for, as string
