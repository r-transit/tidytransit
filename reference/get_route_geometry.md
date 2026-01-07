# Get all trip shapes for a given route and service

Get all trip shapes for a given route and service

## Usage

``` r
get_route_geometry(gtfs_sf_obj, route_ids = NULL, service_ids = NULL)
```

## Arguments

- gtfs_sf_obj:

  tidytransit gtfs object with sf data frames

- route_ids:

  routes to extract

- service_ids:

  service_ids to extract

## Value

an sf dataframe for gtfs routes with a row/linestring for each trip

## Examples

``` r
data(gtfs_duke)
gtfs_duke_sf <- gtfs_as_sf(gtfs_duke)
routes_sf <- get_route_geometry(gtfs_duke_sf)
plot(routes_sf[c(1,1350),])
```
