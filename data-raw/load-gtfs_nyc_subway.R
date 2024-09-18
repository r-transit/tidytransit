library(downloader)
nyc_subway_url <- "http://web.mta.info/developers/data/nyct/subway/google_transit.zip"

download("http://web.mta.info/developers/data/nyct/subway/google_transit.zip",
         here::here("inst/extdata/google_transit_nyc_subway.zip"), mode = "wb")
