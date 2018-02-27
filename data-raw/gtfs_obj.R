url <- "http://data.trilliumtransit.com/gtfs/duke-nc-us/duke-nc-us.zip"
gtfs_obj <- url %>% import_gtfs(quiet=TRUE)
usethis::use_data(gtfs_obj)
