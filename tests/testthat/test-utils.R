context("Utils")

test_that("summary.tidygtfs", {
  gpath <- system.file("extdata", "routing.zip", package = "tidytransit")
  g1 = read_gtfs(gpath)
  x1 = capture.output(summary(g1))
  g2 <- set_servicepattern(g1)
  x2 = capture.output(summary(g2))
  expect_true(all(x1 == x2))
  
  gag = list(agency = data.frame(agency_id = 1:10, agency_name = LETTERS[1:10]))
  expect_equal(agency_info(gag), "A, B, C ... 7 more")
  
  g2 = gtfs_duke
  g2$frequencies <- data.frame(trip_id = "x", start_time = "x", end_time = "x", headway_secs = 0, exact_times = 0)
  expect_equal(capture.output(summary(g2))[5], "uses         stop_times and frequencies")
  g2$stop_times <- NULL
  expect_equal(capture.output(summary(g2))[5], "uses         frequencies (no stop_times)")
})

test_that("print.tidygtfs", {
  gpath <- system.file("extdata", "routing.zip", package = "tidytransit")
  g1 = read_gtfs(gpath)
  expect_true(any(capture.output(print(g1)) == "$.$dates_services"))
})

test_that("filter_stops", {
  g = read_gtfs(system.file("extdata", "routing.zip", package = "tidytransit"))
  fs1 = filter_stops(g, service_ids = "WEEK", route_ids = c("lineA", "lineD"))
  expect_equal(sort(unique(fs1$stop_id)), c("stop1a", "stop1b", "stop2", "stop3a", "stop3b", "stop4", "stop8b"))
  fs2 = filter_stops(g, service_ids = "WEND", route_ids = c("lineA", "lineD"))
  expect_equal(nrow(fs2), 0)
})

test_that("filter_trips", {
  g = read_gtfs(system.file("extdata", "routing.zip", package = "tidytransit"))
  g_sf = gtfs_as_sf(g)
  
  bbox = c(7.4077, 46.9534, 7.40924, 46.95466)
  bbox_sf = sf::st_sf(geometry = sf::st_sfc(
    sf::st_polygon(list(matrix(c(7.4077, 7.40783, 7.40924, 7.40906, 7.4077, 46.95457, 46.9534, 
                 46.9535, 46.95466, 46.95457), nrow = 5)))), crs = 4326)
  
  f1 = suppressMessages(filter_trips_through_area(g_sf, bbox_sf))
  f2 = filter_trips_through_area(g, bbox)
  f3 = filter_trips(g, stop_names = "Six") 
  expect_equal(f1$stop_times, f2$stop_times)
  expect_equal(f1$trips, f2$trips)
  expect_equal(f1$trips, f3$trips)
  
  expect_error(filter_trips_through_area(g, 1:3), "bbox_area must be a numeric vector of length four, with xmin, ymin, xmax and ymax values")
  expect_error(filter_trips(g, "xyz"), "stop_ids found in stops table: xyz")
  expect_error(filter_trips(g, "xyz", "XYZ"), "Please provide either stop_ids or stop_names")
  expect_error(filter_trips(g), "Please provide either stop_ids or stop_names")
})

test_that("filter_trips with shapes", {
  duke_wgs84 = gtfs_as_sf(gtfs_duke)
  duke_epsg = gtfs_as_sf(gtfs_duke, crs = 32119)
  
  area_epsg = duke_epsg$stops %>% 
    filter(stop_id == "2382815") %>%
    sf::st_buffer(30)
  area_wgs84 = sf::st_transform(area_epsg, 4326)
  
  expect_error(filter_trips(gtfs_duke, area_wgs84), "Please use filter_trips_through_area with sf objects")
  expect_error(filter_trips_through_area(duke_wgs84, area_epsg), "feed and area are not in the same coordinate reference system")
  duke_0_0 = filter_trips(gtfs_duke, "2382815")
  
  duke_0_1 = filter_trips_through_area(gtfs_duke, area_wgs84)
  duke_0_2 = filter_trips_through_area(gtfs_duke, area_epsg)
  duke_1_1 = suppressMessages(filter_trips_through_area(duke_wgs84, area_wgs84))
  duke_2_2 = filter_trips_through_area(duke_epsg, area_epsg)
  
  expect_equal(duke_0_1$trips, duke_0_0$trips)
  expect_equal(duke_0_2$trips, duke_0_0$trips)
  expect_equal(duke_1_1$trips, duke_0_0$trips)
  expect_equal(duke_2_2$trips, duke_0_0$trips)
})

test_that("gtfs_meta", { # empty test
  expect_equal(gtfs_meta, get_gtfs_meta())
})

