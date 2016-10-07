library(gtfsr)
context('Mapping GTFS objects')

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
agency_name <- gtfs_obj$agency_df$agency_name
not_gtfs_obj <- 123

# gtfs_map_*()
test_that('Mapping single stops and routes', {


	yes_stop_id <- "778070"
	not_stop_id1 <- 12345
	not_stop_id2 <- "SUPBRO"

	yes_route_id <- "1693"
	not_route_id1 <- 12345
	not_route_id2 <- "SUPBRO"

	# map_gtfs_stop()
	expect_is(map_gtfs_stop(gtfs_obj, yes_stop_id), 'leaflet')
	expect_error(map_gtfs_stop(gtfs_obj, stop_id = not_stop_id1))
	expect_error(map_gtfs_stop(gtfs_obj, stop_id = not_stop_id2))
	expect_error(map_gtfs_stop(not_gtfs_obj, stop_id = not_stop_id1))
	expect_error(map_gtfs_stop(not_gtfs_obj, stop_id = not_stop_id2))

	# map_gtfs()
	expect_is(map_gtfs(gtfs_obj, yes_route_id), 'leaflet')
	expect_error(map_gtfs(gtfs_obj, not_route_id1))
	expect_error(map_gtfs(gtfs_obj, not_route_id2))
	expect_error(map_gtfs(not_gtfs_obj, not_route_id1))
	expect_error(map_gtfs(not_gtfs_obj, not_route_id2))

})

# gtfs_map_*()
test_that('Mapping one or more routes or full network', {

	yes_route_id <- "1693"
	yes_route_ids <- c("1693", "1689")
	ok_route_ids <- c(11111, "1689")
	not_route_id1 <- 12345
	not_route_id2 <- "SUPBRO"

	yes_service_ids <- c("1504A4978", "1505A4978")
	no_service_ids <- c("SUPBRO", 11111111)
	ok_service_ids <- c("SUPBRO", "1505A4978")


	# map_gtfs()
	expect_is(map_gtfs(gtfs_obj), 'leaflet')
	expect_is(map_gtfs(gtfs_obj, yes_route_id), 'leaflet')
	expect_is(map_gtfs(gtfs_obj, yes_route_ids), 'leaflet')
	expect_is(map_gtfs(gtfs_obj, yes_route_ids, yes_service_ids), 'leaflet')
	expect_warning(map_gtfs(gtfs_obj, yes_route_ids, ok_service_ids))
	expect_error(map_gtfs(gtfs_obj, yes_route_ids, no_service_ids))
	expect_error(map_gtfs(gtfs_obj, not_route_id1))
	expect_error(map_gtfs(gtfs_obj, not_route_id2))
	expect_error(map_gtfs(not_gtfs_obj, not_route_id1))
	expect_error(map_gtfs(not_gtfs_obj, not_route_id2))

	# map_gtfs()
	expect_is(map_gtfs(gtfs_obj, agency_name = agency_name), 'leaflet')
	expect_error(map_gtfs(gtfs_obj, agency_name = not_route_id1))
	expect_error(map_gtfs(gtfs_obj, agency_name = not_route_id2))
	expect_error(map_gtfs(not_gtfs_obj, agency_name = not_route_id1))
	expect_error(map_gtfs(not_gtfs_obj, agency_name = not_route_id2))

	# add stops should still return leaflet object
	expect_is(map_gtfs(gtfs_obj, agency_name = agency_name, include_stops = TRUE), 'leaflet')
	expect_error(map_gtfs(gtfs_obj, agency_name = agency_name, route_ids = "2"))

})
