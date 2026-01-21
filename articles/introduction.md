# Introduction to tidytransit

## Introduction

Use tidytransit to:

- [Read a GTFS Feed into R Data
  Types](https://r-transit.github.io/tidytransit/articles/introduction.html#read-a-gtfs-feed)
- [Validate transit
  feeds](https://r-transit.github.io/tidytransit/articles/introduction.html#feed-validation-results)
- [Convert stops and routes to ‘simple
  features’](https://r-transit.github.io/tidytransit/reference/gtfs_as_sf.html)
  and [plot
  them](https://r-transit.github.io/tidytransit/reference/plot.tidygtfs.html)
- [Calculate travel times between transit
  stops](https://r-transit.github.io/tidytransit/reference/travel_times.html)
- [Analyse route frequency and
  headways](https://r-transit.github.io/tidytransit/articles/servicepatterns.html)
- [Analyse calendar
  data](https://r-transit.github.io/tidytransit/articles/servicepatterns.html)
- [Create time tables for
  stops](https://r-transit.github.io/tidytransit/articles/timetable.html)

## Installation & Dependencies

This package requires a working installation of
[sf](https://github.com/r-spatial/sf#installing).

``` r
# Once sf is installed, you can install from CRAN with: 
install.packages("tidytransit")

# For the development version from Github:
# install.packages("devtools")
devtools::install_github("r-transit/tidytransit")
```

## The General Transit Feed Specification

The [summary page for the GTFS
standard](https://gtfs.org/documentation/overview/) is a good resource
for more information on the standard.

GTFS feeds contain many linked tables about published transit schedules,
service schedules, trips, stops, and routes. Below is a diagram of these
relationships and tables:

![gtfs-relationship-diagram](figures/GTFS_class_diagram.svg.png) Source:
Wikimedia, user -stk.

## Read a GTFS Feed

GTFS data come packaged as a zip file of comma separated tables in text
form. The first thing tidytransit does is consolidate the reading of all
those tables into a single R object, which contains a list of the tables
in each feed.

Below we use the tidytransit `read_gtfs` function in order to read a
feed from the NYC MTA into R.

We use an example feed included in the package in the example below. But
note that you can read directly from the New York City Metropolitan
Transit Authority, as shown in the commented code below.

You can also read from any other URL. This is useful because there are
many sources for GTFS data, and often the best source is transit service
providers themselves. See the next section on “Finding More GTFS Feeds”
for more sources of feeds.

``` r
# nyc <- read_gtfs("http://web.mta.info/developers/data/nyct/subway/google_transit.zip")

local_gtfs_path <- system.file("extdata", 
                               "nyc_subway.zip", 
                               package = "tidytransit")
nyc <- read_gtfs(local_gtfs_path)
```

You can use `summary` to get an overview of the feed.

``` r
summary(nyc)
```

    ## tidygtfs object
    ## files        agency, routes, stop_times, trips, shapes, transfers, calendar, calendar_dates, stops
    ## agency       MTA New York City Transit
    ## service      from 2018-06-24 to 2018-11-03
    ## uses         stop_times (no frequencies)
    ## # routes        22
    ## # trips      15911
    ## # stop_ids    1223
    ## # stop_names   306
    ## # shapes       215

Each of the source tables for the GTFS feed is now available in the nyc
`gtfs` object. For example, stops:

``` r
head(nyc$stops)
```

    ## # A tibble: 6 × 6
    ##   stop_id stop_name               stop_lat stop_lon location_type parent_station
    ##   <chr>   <chr>                      <dbl>    <dbl>         <int> <chr>         
    ## 1 101     Van Cortlandt Park - 2…     40.9    -73.9             1 ""            
    ## 2 101N    Van Cortlandt Park - 2…     40.9    -73.9             0 "101"         
    ## 3 101S    Van Cortlandt Park - 2…     40.9    -73.9             0 "101"         
    ## 4 103     238 St                      40.9    -73.9             1 ""            
    ## 5 103N    238 St                      40.9    -73.9             0 "103"         
    ## 6 103S    238 St                      40.9    -73.9             0 "103"

The tables available on each feed may vary. Below we can simply print
the names of all the tables that were read in for this feed. Each of
these is a table.

``` r
names(nyc)
```

    ##  [1] "trips"          "stop_times"     "agency"         "calendar"      
    ##  [5] "calendar_dates" "stops"          "routes"         "shapes"        
    ##  [9] "transfers"      "."

### Feed Validation Results

When reading a feed, it is checked against the GTFS specification, and
an attribute is added to the resultant object called
`validation_result`, which is a tibble about the files and fields in the
GTFS feed and how they compare to the specification.

You can get this tibble from the metadata about the feed.

``` r
validation_result <- attr(nyc, "validation_result")
head(validation_result)
```

    ## # A tibble: 6 × 8
    ##   file   file_spec file_provided_status field   field_spec field_provided_status
    ##   <chr>  <chr>     <lgl>                <chr>   <chr>      <lgl>                
    ## 1 agency Required  TRUE                 agency… Condition… TRUE                 
    ## 2 agency Required  TRUE                 agency… Required   TRUE                 
    ## 3 agency Required  TRUE                 agency… Required   TRUE                 
    ## 4 agency Required  TRUE                 agency… Required   TRUE                 
    ## 5 agency Required  TRUE                 agency… Optional   TRUE                 
    ## 6 agency Required  TRUE                 agency… Optional   TRUE                 
    ## # ℹ 2 more variables: validation_status <chr>, validation_details <chr>

### Finding More GTFS Feeds

#### Feed registries

You can find more feeds on the following sites

- <https://database.mobilitydata.org/>
- <https://www.transit.land/>

There might be other feed registries available, depending on your area
of interest. Transit agencies might also provide their feeds on their on
website.

#### Using MobilityData

MobilityData.org provide a csv file with all their feed sources on their
[GitHub
page](https://github.com/MobilityData/mobility-database-catalogs).

``` r
mbd_url = "https://storage.googleapis.com/storage/v1/b/mdb-csv/o/sources.csv?alt=media"
MobilityData.csv = read.csv(mbd_url)
```

You can load it in R and browse it as a data frame. Tidytransit cannot
handle GTFS realtime data, so we can remove those entries.

``` r
MobilityData_feedlist = MobilityData.csv %>% 
  as_tibble() %>% 
  filter(data_type == "gtfs")

str(MobilityData_feedlist)
```

    ## tibble [2,184 × 27] (S3: tbl_df/tbl/data.frame)
    ##  $ mdb_source_id                          : int [1:2184] 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ data_type                              : chr [1:2184] "gtfs" "gtfs" "gtfs" "gtfs" ...
    ##  $ entity_type                            : chr [1:2184] "" "" "" "" ...
    ##  $ location.country_code                  : chr [1:2184] "US" "CA" "CA" "US" ...
    ##  $ location.subdivision_name              : chr [1:2184] "Maine" "Ontario" "Ontario" "Ohio" ...
    ##  $ location.municipality                  : chr [1:2184] "Casco Bay" "London" "Barrie" "Athens" ...
    ##  $ provider                               : chr [1:2184] "Casco Bay Lines" "London Transit Commission" "Barrie Transit" "Athens Public Transit" ...
    ##  $ is_official                            : chr [1:2184] "" "" "True" "" ...
    ##  $ name                                   : chr [1:2184] "" "" "" "" ...
    ##  $ note                                   : chr [1:2184] "" "" "" "" ...
    ##  $ feed_contact_email                     : chr [1:2184] "" "croy@londontransit.ca" "" "" ...
    ##  $ static_reference                       : chr [1:2184] "" "" "" "" ...
    ##  $ urls.direct_download                   : chr [1:2184] "http://smttracker.com/downloads/gtfs/cascobaylines-portland-me-usa.zip" "http://www.londontransit.ca/gtfsfeed/google_transit.zip" "http://www.myridebarrie.ca/gtfs/Google_transit.zip" "https://transitfeeds.com/p/athens-public-transit/298/latest/download" ...
    ##  $ urls.authentication_type               : int [1:2184] NA 0 NA NA NA NA NA NA 0 0 ...
    ##  $ urls.authentication_info               : chr [1:2184] "" "" "" "" ...
    ##  $ urls.api_key_parameter_name            : chr [1:2184] "" "" "" "" ...
    ##  $ urls.latest                            : chr [1:2184] "https://storage.googleapis.com/storage/v1/b/mdb-latest/o/us-maine-casco-bay-lines-gtfs-1.zip?alt=media" "https://storage.googleapis.com/storage/v1/b/mdb-latest/o/ca-ontario-london-transit-commission-gtfs-2.zip?alt=media" "https://storage.googleapis.com/storage/v1/b/mdb-latest/o/ca-ontario-barrie-transit-gtfs-3.zip?alt=media" "https://storage.googleapis.com/storage/v1/b/mdb-latest/o/us-ohio-athens-public-transit-gtfs-4.zip?alt=media" ...
    ##  $ urls.license                           : chr [1:2184] "" "https://www.londontransit.ca/open-data/ltcs-open-data-terms-of-use/" "https://www.barrie.ca/services-payments/transportation-parking/barrie-transit/barrie-gtfs" "" ...
    ##  $ location.bounding_box.minimum_latitude : num [1:2184] 43.7 42.9 44.3 39.3 44.1 ...
    ##  $ location.bounding_box.maximum_latitude : num [1:2184] 43.7 43.1 44.4 39.4 45 ...
    ##  $ location.bounding_box.minimum_longitude: num [1:2184] -70.2 -81.4 -79.7 -82.1 -75.8 ...
    ##  $ location.bounding_box.maximum_longitude: num [1:2184] -70.1 -81.1 -79.6 -82 -74.6 ...
    ##  $ location.bounding_box.extracted_on     : chr [1:2184] "2022-02-22T19:51:22+00:00" "2022-02-22T19:51:34+00:00" "2022-03-01T22:43:25+00:00" "2022-03-23T14:43:11+00:00" ...
    ##  $ status                                 : chr [1:2184] "inactive" "" "" "deprecated" ...
    ##  $ features                               : chr [1:2184] "" "" "" "" ...
    ##  $ redirect.id                            : chr [1:2184] "" "" "" "1973" ...
    ##  $ redirect.comment                       : chr [1:2184] "" "" "" "" ...

There is a URL (column `urls.direct_download`) for each feed, which can
be used to read the feed for a given area directly into R. For example:

``` r
gtfs_path_goldengate <- MobilityData_feedlist %>% 
  filter(provider == "Golden Gate Transit") %>%
  pull(urls.direct_download)

gtfs_goldengate = read_gtfs(gtfs_path_goldengate)
```

The MobilityData feedlist contains bounding box coordinates for a feed
which can be used to give a basic impression of the location of feeds.
Note that some bounding boxes might be wrong.

First, let’s create bounding box polygons.

``` r
# create a bounding box polygon from min/max corner coordinates
suppressPackageStartupMessages(library(sf))
bbox_polygon = function(lon_min, lon_max, lat_min, lat_max) {
  corner_coords = matrix(
    c(lon_min, lat_min,
      lon_min, lat_max,
      lon_max, lat_max,
      lon_max, lat_min,
      lon_min, lat_min),
    ncol = 2, byrow = TRUE
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
  ungroup() %>% 
  st_as_sf()
```

These boxes could now be shown on an interactive map.

``` r
library(leaflet)
leaflet() %>% 
  addProviderTiles(provider = providers$CartoDB.Positron) %>% 
  addPolygons(data = MobilityData_sf, weight = 2, 
              fillOpacity = 0.1, label = substr(MobilityData_sf$provider, 0, 60))
```
