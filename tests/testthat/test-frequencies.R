# TODO rewrite with synthesized sample data
test_that("Stop frequencies (headways) for included data are as expected", {
  expect_equal(nrow(get_stop_frequency(gtfs_duke, by_route = FALSE)), 47)
  expect_equal(nrow(get_stop_frequency(gtfs_duke, start_time = 10*3600, end_time = 11*3600, by_route = FALSE)), 41)

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

test_that("Route frequencies (headways)", {
  # TODO rewrite with synthesized sample data
  routes_frequency <- get_route_frequency(gtfs_duke)
  expect_equal(routes_frequency[routes_frequency$route_id == 1679, ]$median_headways, 24*60)
})

test_that("Route frequencies (headways) w/ service id", {
  # TODO rewrite with synthesized sample data
  routes_frequency <- get_route_frequency(gtfs_duke, service_id = "c_883_b_21967_d_31")
  expect_equal(routes_frequency[routes_frequency$route_id == 1680, ]$median_headways, (53+1/3)*60)

  expect_error(get_route_frequency(gtfs_duke, service_id = "unknown"), 
                 "Failed to calculate frequency, no departures found")
  
  gtfs_duke2 = gtfs_duke
  gtfs_duke2$frequencies[1,] <- list("t_674449_b_19828_tn_21", "00:00:00", "23:00:00", 60, 0)
  expect_message(get_route_frequency(gtfs_duke2, service_id = "c_883_b_21967_d_31"), 
                 "A pre-calculated frequencies dataframe exists for this feed already, consider using that.")
  
})
