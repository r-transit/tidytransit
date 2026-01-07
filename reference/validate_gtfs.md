# Validate GTFS feed

Validates the GTFS object against GTFS specifications and raises
warnings if required files/fields are not found. This function is called
in
[`read_gtfs()`](https://r-transit.github.io/tidytransit/reference/read_gtfs.md).

## Usage

``` r
validate_gtfs(gtfs_obj, files = NULL, warnings = TRUE)
```

## Arguments

- gtfs_obj:

  gtfs object (i.e. a list of tables, not necessary a tidygtfs object)

- files:

  A character vector containing the text files to be validated against
  the GTFS specification without the file extension (`txt` or
  `geojson`). If `NULL` (the default), the provided GTFS feed is
  validated against all possible GTFS text files.

- warnings:

  Whether to display warning messages (defaults to `TRUE`).

## Value

A `validation_result` tibble containing the validation summary of all
possible fields from the specified files.

## Details

Note that this function just checks if required files or fields are
missing. There's no validation for internal consistency (e.g. no
departure times before arrival times or calendar covering a reasonable
period).

## Details

GTFS object's files and fields are validated against the GTFS
specifications as documented in [GTFS Schedule
Reference](https://gtfs.org/documentation/schedule/reference/):

- GTFS feeds are considered valid if they include all required files and
  fields. If a required file/field is missing the function (optionally)
  raises a warning.

- Optional files/fields are listed in the reference above but are not
  required, thus no warning is raised if they are missing.

- Extra files/fields are those who are not listed in the reference above
  (either because they refer to a specific GTFS extension or due to any
  other reason).

Note that some files (`calendar.txt`, `calendar_dates.txt` and
`feed_info.txt`) are conditionally required. This means that:

- `calendar.txt` is initially set as a required file. If it's not
  present, however, it becomes optional and `calendar_dates.txt`
  (originally set as optional) becomes required.

- `feed_info.txt` is initially set as an optional file. If
  `translations.txt` is present, however, it becomes required.

## Examples

``` r
validate_gtfs(gtfs_duke)
#> # A tibble: 254 × 8
#>    file   file_spec file_provided_status field  field_spec field_provided_status
#>    <chr>  <chr>     <lgl>                <chr>  <chr>      <lgl>                
#>  1 agency Required  TRUE                 agenc… Condition… TRUE                 
#>  2 agency Required  TRUE                 agenc… Required   TRUE                 
#>  3 agency Required  TRUE                 agenc… Required   TRUE                 
#>  4 agency Required  TRUE                 agenc… Required   TRUE                 
#>  5 agency Required  TRUE                 agenc… Optional   TRUE                 
#>  6 agency Required  TRUE                 agenc… Optional   TRUE                 
#>  7 agency Required  TRUE                 agenc… Optional   TRUE                 
#>  8 agency Required  TRUE                 agenc… Optional   FALSE                
#>  9 areas  Optional  FALSE                area_… Required   FALSE                
#> 10 areas  Optional  FALSE                area_… Optional   FALSE                
#> # ℹ 244 more rows
#> # ℹ 2 more variables: validation_status <chr>, validation_details <chr>

if (FALSE) { # \dontrun{
local_gtfs_path <- system.file("extdata", "nyc_subway.zip", package = "tidytransit")
gtfs <- read_gtfs(local_gtfs_path)
attr(gtfs, "validation_result")

gtfs$shapes <- NULL
validation_result <- validate_gtfs(gtfs)

# should raise a warning
gtfs$stop_times <- NULL
validation_result <- validate_gtfs(gtfs)
} # } 
```
