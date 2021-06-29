# https://opentransportdata.swiss/de/dataset/timetable-2021-gtfs2020
library(tidytransit)

gtfs_full = read_gtfs("~/Downloads/gtfs_fp2021_2021-06-16_09-10.zip")

# bbox = osmdata::getbb("Zug, Switzerland", featuretype = "city")
bbox = c(8.4738273, 47.0810323, 8.5775926, 47.1898484)
g <- filter_feed_by_area(gtfs_full, bbox)

write_gtfs(g, "inst/extdata/gtfs_ch_zug.zip")

# 
# g$stops %>%
#   filter(stop_in_area) %>%
#   mapview::mapview()

