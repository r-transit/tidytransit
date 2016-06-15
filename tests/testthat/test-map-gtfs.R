library(gtfsr)
context('Mapping GTFS objects')

not_working <- function() {
	url <- "https://developers.google.com/transit/gtfs/examples/sample-feed.zip"
	connecting <- function(url) {
		r <- base::try(httr::GET(url, httr::timeout(3)))
		if(!assertthat::is.error(r)) r$status_code == 200 else FALSE
	}
	connecting(url)
}

check_url <- function() {
  if (not_working()) {
    skip("Test URL not available.")
  }
}

# gtfs_map_*()
test_that('Mapping single stops and routes', {

	url <- "http://data.trilliumtransit.com/gtfs/duke-nc-us/duke-nc-us.zip"
	gtfs_obj <- url %>% import_gtfs(quiet=TRUE)

	yes_stop_id <- "778070"
	not_stop_id1 <- 12345
	not_stop_id2 <- "SUPBRO"

	yes_route_id <- "1693"
	not_route_id1 <- 12345
	not_route_id2 <- "SUPBRO"

	# map_gtfs_stop()
	expect_is(map_gtfs_stop(gtfs_obj, yes_stop_id), 'leaflet') # leaflet check
	expect_error(map_gtfs_stop(gtfs_obj, stop_id = not_stop_id1))
	expect_error(map_gtfs_stop(gtfs_obj, stop_id = not_stop_id2))
	expect_error(map_gtfs_stop(gtfs_obj = 1234, stop_id = not_stop_id1))
	expect_error(map_gtfs_stop(gtfs_obj = 1234, stop_id = not_stop_id2))

	# map_gtfs_route_stops()
	expect_is(map_gtfs_route_stops(gtfs_obj, yes_route_id), 'leaflet') # leaflet check
	expect_error(map_gtfs_route_stops(gtfs_obj, route_id = not_route_id1))
	expect_error(map_gtfs_route_stops(gtfs_obj, route_id = not_route_id2))
	expect_error(map_gtfs_route_stops(gtfs_obj = 1234, route_id = not_route_id1))
	expect_error(map_gtfs_route_stops(gtfs_obj = 1234, route_id = not_route_id2))

	# map_gtfs_route_shape()
	expect_is(map_gtfs_route_shape(gtfs_obj, yes_route_id), 'leaflet') # leaflet check
	expect_error(map_gtfs_route_shape(gtfs_obj, route_id = not_route_id1))
	expect_error(map_gtfs_route_shape(gtfs_obj, route_id = not_route_id2))
	expect_error(map_gtfs_route_shape(gtfs_obj = 1234, route_id = not_route_id1))
	expect_error(map_gtfs_route_shape(gtfs_obj = 1234, route_id = not_route_id2))

})
