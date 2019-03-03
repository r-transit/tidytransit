context("Frequencies are calculated correctly")

test_that("Stop frequencies (headways) for included data are as expected", {
  gtfs_obj <- get_stop_frequency(gtfs_obj, by_route=FALSE)
  stop_frequency_summary <- gtfs_obj$stops_frequency
  fifteenth_st_at_hillsborough_rd <- 
    stop_frequency_summary[
      stop_frequency_summary$stop_id==778123, ]$headway
  expect_equal(as.integer(7.8688), as.integer(fifteenth_st_at_hillsborough_rd))
})

test_that("Route frequencies (headways) for included data are as expected", {
  gtfs_obj <- get_route_frequency(gtfs_obj)
  rf <- gtfs_obj$routes_frequency
  expect_equal(rf[rf$route_id==1679,]$median_headways, 24)
})

test_that("Route frequencies (headways) 
          can be calculated for included data 
          for a particular service id", {
  gtfs_obj <- get_route_frequency(gtfs_obj, 
                                  service_id = "c_883_b_6873_d_31")
  rf <- gtfs_obj$routes_frequency
  expect_equal(rf[rf$route_id==1680,]$median_headways, 25)
})