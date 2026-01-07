# Convert an sf object to a json list

The sf object is written to a temporary file and re-read with
jsonlite::read_json().

## Usage

``` r
sf_to_json(sf_obj, layer_name)
```

## Arguments

- sf_obj:

  sf table

## Value

json list
