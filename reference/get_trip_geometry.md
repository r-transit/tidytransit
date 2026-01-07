# Get all trip shapes for given trip ids

Get all trip shapes for given trip ids

## Usage

``` r
get_trip_geometry(gtfs_sf_obj, trip_ids)
```

## Arguments

- gtfs_sf_obj:

  tidytransit gtfs object with sf data frames

- trip_ids:

  trip_ids to extract shapes

## Value

an sf dataframe for gtfs routes with a row/linestring for each trip

## Examples

``` r
data(gtfs_duke)
gtfs_duke <- gtfs_as_sf(gtfs_duke)
trips_sf <- get_trip_geometry(gtfs_duke, c("t_726295_b_19493_tn_41", "t_726295_b_19493_tn_40"))
plot(trips_sf[1,"shape_id"])
```
