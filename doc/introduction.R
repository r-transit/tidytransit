## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(tidytransit)
library(dplyr)

## ----eval=FALSE---------------------------------------------------------------
#  # Once sf is installed, you can install from CRAN with:
#  install.packages('tidytransit')
#  
#  # For the development version from Github:
#  # install.packages("devtools")
#  devtools::install_github("r-transit/tidytransit")

## -----------------------------------------------------------------------------
# nyc <- read_gtfs("http://web.mta.info/developers/data/nyct/subway/google_transit.zip")

local_gtfs_path <- system.file("extdata", 
                               "google_transit_nyc_subway.zip", 
                               package = "tidytransit")
nyc <- read_gtfs(local_gtfs_path)

## -----------------------------------------------------------------------------
summary(nyc)

## -----------------------------------------------------------------------------
head(nyc$stops)

## -----------------------------------------------------------------------------
names(nyc)

## -----------------------------------------------------------------------------
validation_result <- attr(nyc, "validation_result")
head(validation_result)

## -----------------------------------------------------------------------------
MobilityData.csv = read.csv("https://storage.googleapis.com/storage/v1/b/mdb-csv/o/sources.csv?alt=media")

MobilityData_feedlist = MobilityData.csv %>% 
  as_tibble() %>% 
  filter(data_type == "gtfs")

str(MobilityData_feedlist)

## ----eval=FALSE---------------------------------------------------------------
#  gtfs_path_goldengate <- MobilityData_feedlist %>%
#    filter(provider == "Golden Gate Transit") %>%
#    pull(urls.direct_download)
#  
#  gtfs_goldengate = read_gtfs(gtfs_path_goldengate)

## -----------------------------------------------------------------------------
# create a bounding box polygon from min/max corner coordinates
suppressPackageStartupMessages(library(sf))
bbox_polygon = function(lon_min, lon_max, lat_min, lat_max) {
  corner_coords = matrix(
    c(lon_min, lat_min,
      lon_min, lat_max,
      lon_max, lat_max,
      lon_max, lat_min,
      lon_min, lat_min),
    ncol = 2, byrow = T
  )
  polyg = st_polygon(list(corner_coords))
  return(st_sfc(polyg, crs = 4326))
}

# create bounding box polygon (only for reasonable values)
MobilityData_sf = MobilityData_feedlist %>% 
  filter(!is.na(location.bounding_box.minimum_longitude)) %>% 
  filter(location.bounding_box.minimum_latitude > -89) %>% 
  group_by(mdb_source_id) %>% 
  mutate(geometry = bbox_polygon(location.bounding_box.minimum_longitude,
                                 location.bounding_box.maximum_longitude,
                                 location.bounding_box.minimum_latitude,
                                 location.bounding_box.maximum_latitude)) %>% 
  ungroup() %>% st_as_sf()

## ----fig.width=7--------------------------------------------------------------
library(leaflet)
leaflet() %>% 
  addProviderTiles(provider = providers$CartoDB.Positron) %>% 
  addPolygons(data = MobilityData_sf, weight = 2, 
              fillOpacity = 0.1, label = substr(MobilityData_sf$provider, 0, 60))


