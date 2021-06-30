# https://opentransportdata.swiss/de/dataset/timetable-2021-gtfs2020
library(tidytransit)

gtfs_full = read_gtfs("~/Downloads/gtfs_fp2021_2021-06-16_09-10.zip")

bbox = c(8.3906, 47.0988, 8.5668, 47.2246)
g <- filter_feed_by_area(gtfs_full, bbox)

write_gtfs(g, "inst/extdata/gtfs_ch_zg.zip")
