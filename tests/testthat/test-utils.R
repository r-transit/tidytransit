context("Utils")

test_that("summary.tidygtfs", {
  gpath <- system.file("extdata", "routing.zip", package = "tidytransit")
  g1 = read_gtfs(gpath)
  x1 = capture.output(summary(g1))
  g2 <- set_servicepattern(g1)
  x2 = capture.output(summary(g2))
  expect_true(all(x1 == x2))
})

test_that("filter_stops", {
  g = read_gtfs(system.file("extdata", "routing.zip", package = "tidytransit"))
  fs1 = filter_stops(g, service_ids = "WEEK", route_ids = c("lineA", "lineD"))
  expect_equal(sort(unique(fs1$stop_id)), c("stop1a", "stop1b", "stop2", "stop3a", "stop3b", "stop4", "stop8b"))
  fs2 = filter_stops(g, service_ids = "WEND", route_ids = c("lineA", "lineD"))
  expect_equal(nrow(fs2), 0)
})

test_that("filter_feed", {
  g = read_gtfs(system.file("extdata", "routing.zip", package = "tidytransit"))
  g_sf = gtfs_as_sf(g, T)

  # sf
  bbox = c(7.4077, 46.9534, 7.40924, 46.95466)
  bbox_sf = sf::st_sf(geometry = sf::st_sfc(
    sf::st_polygon(list(matrix(c(7.4077, 7.40783, 7.40924, 7.40906, 7.4077, 46.95457, 46.9534, 
                 46.9535, 46.95466, 46.95457), nrow = 5)))), crs = 4326)
  
  expect_message(filter_trips_through_area(g_sf, bbox_sf), "although coordinates are longitude/latitude, st_intersection assumes that they are planar")
  f1 = suppressMessages(filter_trips_through_area(g_sf, bbox_sf))
  f2 = filter_trips_through_area(g, bbox)
  expect_equal(f1$stop_times, f2$stop_times)
  expect_equal(f1$trips, f2$trips)
  expect_error(filter_trips_through_area(g, 1:3), "bbox_area must be a numeric vector of length four, with xmin, ymin, xmax and ymax values")
})
