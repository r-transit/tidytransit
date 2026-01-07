# Print a GTFS object

Prints a GTFS object suppressing the `class` attribute and hiding the
validation_result attribute, created with
[`validate_gtfs()`](https://r-transit.github.io/tidytransit/reference/validate_gtfs.md).

## Usage

``` r
# S3 method for class 'tidygtfs'
print(x, ...)
```

## Arguments

- x:

  a tidygtfs object as read by
  [`read_gtfs()`](https://r-transit.github.io/tidytransit/reference/read_gtfs.md)

- ...:

  Optional arguments ultimately passed to `format`.

## Value

The GTFS object that was printed, invisibly

## Examples

``` r
 if (FALSE) { # \dontrun{
path = system.file("extdata", 
           "nyc_subway.zip", 
           package = "tidytransit")

g = read_gtfs(path)
print(g)
} # }
```
