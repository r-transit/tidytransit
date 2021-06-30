# https://opentransportdata.swiss/de/dataset/timetable-2021-gtfs2020
library(tidytransit)

gtfs_full = read_gtfs("~/Downloads/gtfs_fp2021_2021-06-16_09-10.zip")

bbox = c(8.44546, 47.16418, 8.55613, 47.20850)
g <- filter_feed_by_area(gtfs_full, bbox)

write_gtfs(g, "inst/extdata/gtfs_ch_zg.zip")
