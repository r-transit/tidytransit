# Convert columns between gtfsio types to tidytransit types according to GTFS reference

Convert columns between gtfsio types to tidytransit types according to
GTFS reference

## Usage

``` r
convert_types(gtfs_list, conversion_table, conversion_function)
```

## Arguments

- gtfs_list:

  gtfs object

- conversion_table:

  data.frame containing a column `file` and `Field_Name`, generally from
  internal `gtfs_reference_types` dataset

- conversion_function:

  function to convert columns

## Value

gtfs_list with converted (overwritten) columns in tables
