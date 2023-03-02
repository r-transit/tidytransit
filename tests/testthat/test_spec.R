test_that("specs", {
  m = get_gtfs_meta()
  
  ids = unlist(lapply(m, function(x) x[["required_unique_id"]]))
  expect_equal(length(setdiff(c(NA, "stop_id", "route_id", "trip_id", "service_id", "fare_id", "pathway_id", "level_id"), ids)), 0)
})
  