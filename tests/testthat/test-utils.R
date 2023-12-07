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

test_that("filter_feed_by_stops", {
  g = read_gtfs(system.file("extdata", "routing.zip", package = "tidytransit"))
  g_sf = gtfs_as_sf(g)
  
  bbox = c(7.4077, 46.9534, 7.40924, 46.95466)
  bbox_sf = sf::st_sf(geometry = sf::st_sfc(
    sf::st_polygon(list(matrix(c(7.4077, 7.40783, 7.40924, 7.40906, 7.4077, 46.95457, 46.9534, 
                 46.9535, 46.95466, 46.95457), nrow = 5)))), crs = 4326)
  
  f1 = suppressMessages(filter_feed_by_area(g_sf, bbox_sf))
  f2 = filter_feed_by_area(g, bbox)
  f3 = filter_feed_by_stops(g, stop_names = "Six") 
  expect_equal(f1$stop_times, f2$stop_times)
  expect_equal(f1$trips, f2$trips)
  expect_equal(f1$trips, f3$trips)
  
  expect_error(filter_feed_by_area(g, 1:3), "bbox_area must be a numeric vector of length four, with xmin, ymin, xmax and ymax values")
  expect_error(filter_feed_by_stops(g, "xyz"), "stop_ids found in stops table: xyz")
  expect_error(filter_feed_by_stops(g, "xyz", "XYZ"), "Please provide either stop_ids or stop_names")
  expect_error(filter_feed_by_stops(g), "Please provide either stop_ids or stop_names")
})

test_that("filter_feed with shapes", {
  duke_wgs84 = gtfs_as_sf(gtfs_duke)
  duke_epsg = gtfs_as_sf(gtfs_duke, crs = 32119)
  
  area_epsg = duke_epsg$stops %>% 
    filter(stop_id == "2382815") %>%
    sf::st_buffer(30)
  area_wgs84 = sf::st_transform(area_epsg, 4326)
  
  expect_error(filter_feed_by_stops(gtfs_duke, area_wgs84), 
               "Please use filter_feed_by_area with sf objects")
  expect_error(filter_feed_by_area(duke_wgs84, area_epsg), 
               "feed and area are not in the same coordinate reference system")
  duke_0_0 = filter_feed_by_stops(gtfs_duke, "2382815")
  
  duke_0_1 = filter_feed_by_area(gtfs_duke, area_wgs84)
  duke_0_2 = filter_feed_by_area(gtfs_duke, area_epsg)
  duke_1_1 = suppressMessages(filter_feed_by_area(duke_wgs84, area_wgs84))
  duke_2_2 = filter_feed_by_area(duke_epsg, area_epsg)
  
  expect_equal(duke_0_1$trips, duke_0_0$trips)
  expect_equal(duke_0_2$trips, duke_0_0$trips)
  expect_equal(duke_1_1$trips, duke_0_0$trips)
  expect_equal(duke_2_2$trips, duke_0_0$trips)
})

test_that("filter_feed_by_date", {
  skip_on_cran()
  g0 = read_gtfs(system.file("extdata", "google_transit_nyc_subway.zip",
                             package = "tidytransit"))
  g1 = filter_feed_by_date(g0, "2018-06-28")
  g2 = filter_feed_by_date(g0, "2018-10-30")
  expect_true(all(g2$.$dates_services$date == as.Date("2018-10-30")))

  expect_lt(nrow(g1$stops), nrow(g0$stops))
  expect_lt(nrow(g2$stops), nrow(g0$stops))
  
  expect_is(g1$stop_times, "tbl_df")
  expect_is(g2$stop_times, "tbl_df")
})


test_that("gtfs_meta", { # empty test
  expect_equal(gtfs_meta, get_gtfs_meta())
})

test_that("empty_strings_to_na", {
  gpath = system.file("extdata", "sample-feed-fixed.zip", package = "tidytransit")
  g1 = read_gtfs(gpath)
  g_na = empty_strings_to_na(g1)
  g2 = na_to_empty_strings(g_na)

  for(tbl in names(g1)) {
    expect_equal(g1[[tbl]], g2[[tbl]])
  }
  
  tmppath = tempfile(fileext = ".zip")
  write_gtfs(g_na, tmppath)
  g3 = read_gtfs(tmppath)
   
  for(tbl in names(g1)) {
    expect_equal(g1[[tbl]], g3[[tbl]])
  } 
})
