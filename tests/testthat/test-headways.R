context("Frequencies are calculated correctly")

# TODO rewrite with synthesized sample data
test_that("Stop frequencies (headways) for included data are as expected", {
  stops_frequency <- get_stop_frequency(gtfs_duke, by_route = FALSE)
  ex_address <- 
    stops_frequency$headway[stops_frequency$stop_id==778037]
  expect_equal(as.integer(240), as.integer(ex_address))
})

test_that("Route frequencies (headways) for included data are as expected", {
  routes_frequency <- get_route_frequency(gtfs_duke)
  expect_equal(routes_frequency[routes_frequency$route_id==13048, ]$median_headways, 104)
})

test_that("Route frequencies (headways) can be calculated for included data for a particular service id", {
  # TODO rewrite with synthesized sample data
  routes_frequency <- get_route_frequency(gtfs_duke, service_id = "c_853_b_22586_d_24")
  expect_equal(routes_frequency[routes_frequency$route_id == 1693, ]$median_headways, 31)
})
