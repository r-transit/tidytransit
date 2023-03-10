test_that("specs", {
  m = get_gtfs_meta()
  
  ids = unlist(lapply(m, function(x) x[["primary_key"]]))
  expect_equal(length(setdiff(c(NA, "stop_id", "route_id", "trip_id", "service_id", "fare_id", "pathway_id", "level_id"), ids)), 0)
})

test_that("warning for duplicated primary key", {
  g1 = read_gtfs(system.file("extdata", "routing.zip", package = "tidytransit"))
  g1$stops$stop_id[1] <- "stop1a"
  g1$routes$route_id[1] <- "lineB"
  
  # function
  g1a = g1[3:6]
  g1a$fare_rules <- data.frame(fare_id = c("1", "2", "2", "2"), route_id = c("B", "B", "A", "A"))
  g1b = convert_list_tables_to_data.tables(g1a)
  d1 = duplicated_primary_keys(g1a)
  d2 = duplicated_primary_keys(g1a)
  expect_equal(unname(d1), c(F, T, F, T, T))
  expect_equal(d1, d2)
  
  # within as_tidygtfs
  g2 = expect_warning(as_tidygtfs(g1), "Duplicated ids found in: routes, stops")
  expect_is(g2, "list")
  
  g3 = g2
  g3$stops$stop_id[1] <- "stop1"
  g3$routes$route_id[1] <- "lineA"

  g4 = as_tidygtfs(g3)  
  expect_is(g4, "tidygtfs")  
})
  