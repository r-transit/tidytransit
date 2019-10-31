context("Frequencies are calculated correctly")

test_that("Stop frequencies (headways) for included data are as expected", {
  # TODO rewrite with synthesized sample data
  stops_frequency <- get_stop_frequency(gtfs_duke, by_route = FALSE)
  ex_address <- 
    stops_frequency[
      stops_frequency$stop_id==778123, ]$headway
  expect_equal(as.integer(14), as.integer(ex_address))
})

test_that("Route frequencies (headways) for included data are as expected", {
  # TODO rewrite with synthesized sample data
  routes_frequency <- get_route_frequency(gtfs_duke)
  expect_equal(routes_frequency[routes_frequency$route_id==1679, ]$median_headways, 24)
})

test_that("Route frequencies (headways) 
          can be calculated for included data 
          for a particular service id", {
            # TODO rewrite with synthesized sample data
            routes_frequency <- get_route_frequency(gtfs_duke, 
                                  service_id = "c_883_b_21967_d_31")
  expect_equal(routes_frequency[routes_frequency$route_id == 1680, ]$median_headways, 53)
})