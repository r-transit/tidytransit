test_that("specs", {
  m = get_gtfs_meta()
  
  ids = unlist(lapply(m, function(x) x[["required_unique_id"]]))
  expect_equal(length(setdiff(c(NA, "stop_id", "route_id", "trip_id", "service_id", "fare_id", "pathway_id", "level_id"), ids)), 0)
})
  
test_that("warning for duplicated primary key", {
  g1 = read_gtfs(system.file("extdata", "routing.zip", package = "tidytransit"))
  g1$stops$stop_id[1] <- "stop1a"
  g1$routes$route_id[1] <- "lineB"
  
  g2 = expect_warning(as_tidygtfs(g1), "Duplicated ids found in: routes, stops")
  expect_is(g2, "list")
  
  g3 = g2
  g3$stops$stop_id[1] <- "stop1"
  g3$routes$route_id[1] <- "lineA"

  g4 = as_tidygtfs(g3)  
  expect_is(g4, "tidygtfs")  
})
  