library(gtfsr)
context('Converting GTFS routes and shapes into simple feature (sf) dataframes')

not_working <- function() {
	url <- "http://data.trilliumtransit.com/gtfs/duke-nc-us/duke-nc-us.zip"
	connecting <- function(url) {
		r <- base::try(httr::GET(url, httr::timeout(5)))
		if(!assertthat::is.error(r)) r$status_code == 200 else FALSE
	}
	connecting(url)
}

check_url <- function() {
  if (not_working()) {
    skip("Test URL not available.")
  }
}

url <- "http://data.trilliumtransit.com/gtfs/duke-nc-us/duke-nc-us.zip"
gtfs_obj <- url %>% import_gtfs(quiet=TRUE)

# gtfs_routes_as_sf()
test_that('Can convert get a gtfsr routes dataframe to a dataframe with a simple features column', {
	expect_is(gtfs_routes_as_sf(gtfs_obj), 'sf')
})
