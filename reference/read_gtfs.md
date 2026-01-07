# Read and validate GTFS files

Reads a GTFS feed from either a local `.zip` file or an URL and
validates them against GTFS specifications.

## Usage

``` r
read_gtfs(path, files = NULL, quiet = TRUE, ...)
```

## Arguments

- path:

  The path to a GTFS `.zip` file.

- files:

  A character vector containing the text files to be validated against
  the GTFS specification without the file extension (`txt` or
  `geojson`). If `NULL` (the default), all existing files are read.

- quiet:

  Whether to hide log messages and progress bars (defaults to TRUE).

- ...:

  Can be used to pass on arguments to
  [`gtfsio::import_gtfs()`](https://r-transit.github.io/gtfsio/reference/import_gtfs.html).
  The parameters `files` and `quiet` are passed on by default.

## Value

A tidygtfs object: a list of tibbles in which each entry represents a
GTFS text file. Additional tables are stored in the `.` sublist.

## Note

**Limitations:** `read_gtfs()` does not support downloading GTFS files
from URLs that require authentication. If you need to read a feed behind
authentication, first download the feed to a local file and then pass
the local path to `read_gtfs()`.

## See also

[`validate_gtfs()`](https://r-transit.github.io/tidytransit/reference/validate_gtfs.md),
[`write_gtfs()`](https://r-transit.github.io/tidytransit/reference/write_gtfs.md)

## Examples

``` r
if (FALSE) { # \dontrun{
local_gtfs_path <- system.file("extdata", "nyc_subway.zip", package = "tidytransit")
gtfs <- read_gtfs(local_gtfs_path)
summary(gtfs)

gtfs <- read_gtfs(local_gtfs_path, files = c("trips", "stop_times"))
names(gtfs)
} # }
```
