Example use:

```
library(gtfsr)
library(magrittr)
library(dplyr)

# set the API key
set_api_key() # uncomment to set api key

# get the feedlist dataframe and filter out NYC subway
feedlist_df <- get_feedlist() 

sf <- filter(feedlist_df,grepl('Muni GTFS', t, ignore.case= TRUE))

# import NYC gtfs feed by sending the url to `import_gtfs`
SF <- import_gtfs(sf$url_d)
#> [1] "agency.txt"         "calendar.txt"       "calendar_dates.txt"
#> [4] "routes.txt"         "shapes.txt"         "stop_times.txt"    
#> [7] "stops.txt"          "transfers.txt"      "trips.txt"

routes_shapes_df <- join_shape_route_service_ids(SF)

sfl1 <- get_sf_lines(SF)

ml1 <- gtfs_route_to_sf_multiline(sfl1,routes_shapes_df,service_id="73053",route_id="E")
```