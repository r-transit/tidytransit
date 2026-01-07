# Plot GTFS stops and trips

Plot GTFS stops and trips

## Usage

``` r
# S3 method for class 'tidygtfs'
plot(x, ...)
```

## Arguments

- x:

  a tidygtfs object as read by
  [`read_gtfs()`](https://r-transit.github.io/tidytransit/reference/read_gtfs.md)

- ...:

  ignored for tidygtfs

## Value

plot

## Examples

``` r
# \donttest{
local_gtfs_path <- system.file("extdata",
                              "nyc_subway.zip",
                              package = "tidytransit")
nyc <- read_gtfs(local_gtfs_path)
plot(nyc)

# }
```
