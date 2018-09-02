devtools::load_all()
url <- "http://data.trilliumtransit.com/gtfs/duke-nc-us/duke-nc-us.zip"
gtfs_obj <- read_gtfs(url, quiet=TRUE)
saveRDS(gtfs_obj, here::here('data-raw/gtfs_obj'))
devtools::use_data(gtfs_obj)
