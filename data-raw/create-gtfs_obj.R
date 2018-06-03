devtools::load_all()
url <- "http://data.trilliumtransit.com/gtfs/duke-nc-us/duke-nc-us.zip"
gtfs_obj <- url %>% import_gtfs(quiet=TRUE)
saveRDS(gtfs_obj, here::here('data-raw/gtfs_obj'))

