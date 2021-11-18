context("Frequencies are calculated correctly")

# TODO rewrite with synthesized sample data
test_that("Stop frequencies (headways) for included data are as expected", {
  expect_equal(nrow(get_stop_frequency(gtfs_duke, by_route = FALSE)), 47)
  expect_equal(nrow(get_stop_frequency(gtfs_duke, start_hour = 10, end_hour = 11, by_route = FALSE)), 41)

  stops_frequency <- get_stop_frequency(gtfs_duke, service_ids = "c_853_b_19828_d_64")
  ex_address <- stops_frequency$mean_headway[stops_frequency$stop_id==778058]
  expect_equal(as.integer(711), as.integer(ex_address))
  
  stops_frequency_by_route <- get_stop_frequency(gtfs_duke, 
                                                 service_ids = "c_853_b_19828_d_64",
                                                 by_route = TRUE)
  expect_equal(
    colnames(stops_frequency_by_route), 
    c("stop_id", "route_id", "direction_id", "service_id", "n_departures", "mean_headway"))
})
