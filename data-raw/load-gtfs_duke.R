devtools::load_all()
url <- "http://data.trilliumtransit.com/gtfs/duke-nc-us/duke-nc-us.zip"
gtfs_duke <- read_gtfs(url)
usethis::use_data(gtfs_duke)

for(tblname in setdiff(names(gtfs_duke), c(".", "calendar_dates", "frequencies"))) {
  if(nrow(gtfs_duke[[tblname]]) == 0) {
    gtfs_duke[[tblname]] <- NULL
  }
}

usethis::use_data(gtfs_duke, overwrite = T)
