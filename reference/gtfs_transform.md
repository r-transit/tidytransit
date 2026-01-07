# Transform coordinates of a gtfs feed

Transform coordinates of a gtfs feed

## Usage

``` r
gtfs_transform(gtfs_obj, crs)
```

## Arguments

- gtfs_obj:

  gtfs feed (tidygtfs object)

- crs:

  target coordinate reference system, used by
  [`sf::st_transform()`](https://r-spatial.github.io/sf/reference/st_transform.html)

## Value

tidygtfs object with transformed stops and shapes sf dataframes

gtfs object with transformed sf tables
