# Write a tidygtfs object to a zip file

Write a tidygtfs object to a zip file

## Usage

``` r
write_gtfs(gtfs_obj, zipfile, compression_level = 9, as_dir = FALSE)
```

## Arguments

- gtfs_obj:

  gtfs feed (tidygtfs object)

- zipfile:

  path to the zip file the feed should be written to. The file is
  overwritten if it already exists.

- compression_level:

  a number between 1 and 9, defaults to 9 (best compression).

- as_dir:

  if `TRUE`, the feed is not zipped and zipfile is used as a directory
  path. The directory will be overwritten if it already exists.

## Value

Invisibly returns `gtfs_obj`

## Note

Auxiliary tidytransit tables (e.g. `dates_services`) are not exported.
Calls
[`gtfsio::export_gtfs()`](https://r-transit.github.io/gtfsio/reference/export_gtfs.html)
after preparing the data.

## See also

[`read_gtfs()`](https://r-transit.github.io/tidytransit/reference/read_gtfs.md)

## Examples

``` r
if (FALSE) { # \dontrun{
local_gtfs_path <- system.file("extdata", "nyc_subway.zip", package = "tidytransit")
gtfs <- read_gtfs(local_gtfs_path)

gtfs <- filter_feed_by_date(gtfs, "2018-06-30")

write_gtfs(gtfs, "feed_filtered.zip")
} # }
```
