devtools::load_all()
url <- "http://data.trilliumtransit.com/gtfs/duke-nc-us/duke-nc-us.zip"
gtfs_duke <- read_gtfs(url, quiet=TRUE)
saveRDS(gtfs_duke, here::here('data-raw/gtfs_duke'))
usethis::use_data(gtfs_duke)
