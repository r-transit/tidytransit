test_that("warning for duplicated primary key", {
  g1 = read_gtfs(system.file("extdata", "routing.zip", package = "tidytransit"))
  g1$stops$stop_id[1] <- "stop1a"
  g1$routes$route_id[1] <- "lineB"
  
  # function
  g1a = g1[3:6]
  g1a$fare_rules <- data.frame(fare_id = c("1", "2", "2", "2"), route_id = c("B", "B", "A", "A"))
  g1b = convert_list_tables_to_data.tables(g1a)
  d1a = duplicated_primary_keys(g1a)
  d1b = duplicated_primary_keys(g1b)
  expect_identical(unname(d1a), c(FALSE, TRUE, FALSE, TRUE, TRUE))
  expect_identical(d1a, d1b)
  
  # within as_tidygtfs
  g2 = expect_warning(as_tidygtfs(g1), "Duplicated ids found in: routes, stops")
  expect_type(g2, "list")
  
  g3 = g2
  g3$stops$stop_id[1] <- "stop1"
  g3$routes$route_id[1] <- "lineA"
  
  g4 = as_tidygtfs(g3)  
  expect_s3_class(g4, "tidygtfs")  
})

test_that("validate non gtfs object", {
  expect_error(validate_gtfs(data.frame(stop_id = "1")), "gtfs_obj must be a gtfs or list object")
})

test_that("validation", {
  g_invalid_path = system.file("extdata","sample-feed-invalid.zip", package = "tidytransit")
  expect_warning(read_gtfs(g_invalid_path), "Invalid feed. Missing required file(s): stop_times.txt", fixed = TRUE)
  expect_warning(read_gtfs(g_invalid_path), "Invalid feed. Missing required field(s) in stops: stop_id", fixed = TRUE)
  
  # extra table
  g = read_gtfs(system.file("extdata", "sample-feed-fixed.zip", package = "tidytransit"))
  g$extra <- "not_a_dataframe"
  vd = validate_gtfs(g)
  expect_true(is.na(vd[vd$file == "extra","field"]))
  
  expect_error(validate_gtfs(g, files = c("unknown", "other")), "File names not found in gtfs_obj: unknown, other")
})
