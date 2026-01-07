# Convert an object created by gtfsio::import_gtfs to a tidygtfs object

Some basic validation is done to ensure the feed works in tidytransit

## Usage

``` r
gtfs_to_tidygtfs(gtfs_list, files = NULL)
```

## Arguments

- gtfs_list:

  list of tables

- files:

  subset of files to validate
