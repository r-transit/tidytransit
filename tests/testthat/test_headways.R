context("Frequencies are calculates correctly")

test_that("Stop frequencies (headways) for included data are as expected", {
  stop_frequency_summary <- get_stop_frequency(gtfs_obj, by_route=FALSE)
  fifteenth_st_at_hillsborough_rd <- stop_frequency_summary[stop_frequency_summary$stop_id==778123,]$headway
  expect_equal(as.integer(7.8688), as.integer(fifteenth_st_at_hillsborough_rd))
})

test_that("Route frequencies (headways) for included data are as expected", {
  rf <- get_route_frequency(gtfs_obj)
  expect_equal(rf[rf$route_id==1679,]$median_headways, 26)
})