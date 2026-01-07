# Convert a json (read with jsonlite) to sf object

The json object is written to a temporary file and re-read with
[`sf::read_sf()`](https://r-spatial.github.io/sf/reference/st_read.html).

## Usage

``` r
json_to_sf(json_list)
```

## Arguments

- json_list:

  list as read by
  [`jsonlite::read_json()`](https://jeroen.r-universe.dev/jsonlite/reference/read_json.html)
  (in gtfsio)

## Value

sf object
