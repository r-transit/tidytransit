# Interpolate missing stop_times linearly

Interpolate missing stop_times linearly

## Usage

``` r
interpolate_stop_times(x, use_shape_dist = TRUE)
```

## Arguments

- x:

  tidygtfs object or stop_times table

- use_shape_dist:

  If TRUE, use `shape_dist_traveled` column from the shapes table for
  time interpolation (if that column is available). If FALSE or
  `shape_dist_traveled` is missing, times are interpolated equally
  between stops.

## Value

tidygtfs or stop_times with interpolated arrival and departure times

## Examples

``` r
if (FALSE) { # \dontrun{
data(gtfs_duke)
print(gtfs_duke$stop_times[1:5, 1:5])

gtfs_duke_2 = interpolate_stop_times(gtfs_duke)
print(gtfs_duke_2$stop_times[1:5, 1:5])

gtfs_duke_3 = interpolate_stop_times(gtfs_duke, FALSE)
print(gtfs_duke_3$stop_times[1:5, 1:5])
} # }
```
