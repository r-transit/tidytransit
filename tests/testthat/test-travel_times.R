context("travel_times routing")

local_gtfs_path <- system.file("extdata", "routing.zip", package = "tidytransit")
gtfs_routing <- read_gtfs(local_gtfs_path)
test_from_stop_ids <- c("stop1a", "stop1b")

stop_times = gtfs_routing$stop_times
stop_times_0710 = dplyr::filter(gtfs_routing$stop_times, departure_time >= 7*3600+10*60)
stop_times_0711 = dplyr::filter(gtfs_routing$stop_times, departure_time >= 7*3600+11*60)
stop_times_0715 = dplyr::filter(gtfs_routing$stop_times, departure_time >= 7*3600+15*60)
transfers = gtfs_routing$transfers

test_that("travel times wrapper function", {
  fst = filter_stop_times(gtfs_routing, "2018-10-01", 7*3600, 24*3600)
  tt = travel_times(
    filtered_stop_times = fst,
    stop_name = "One",
    time_range = 3600)
  expect_equal(nrow(tt), length(unique(gtfs_routing$stops$stop_name)))
  expect_equal(tt %>% dplyr::filter(to_stop_name == "One") %>% dplyr::pull(travel_time), 0)
  expect_equal(tt$travel_time[which(tt$to_stop_name == "One")], 0)
  expect_equal(tt$travel_time[which(tt$to_stop_name == "Two")], 4*60)
  expect_equal(tt$travel_time[which(tt$to_stop_name == "Three")], (18-12)*60)
  expect_equal(tt$travel_time[which(tt$to_stop_name == "Four")], (37-17)*60)
  expect_equal(tt$travel_time[which(tt$to_stop_name == "Five")], (15-10)*60)
  expect_equal(tt$travel_time[which(tt$to_stop_name == "Six")], (20-10)*60)
  expect_equal(tt$travel_time[which(tt$to_stop_name == "Seven")], (25-10)*60)
  expect_equal(tt$travel_time[which(tt$to_stop_name == "Eight")], (24-12)*60)

  tt2 = travel_times(filtered_stop_times = fst, stop_name = "One",
                     time_range = 3600, return_coords = TRUE)
  expect_equal(
    setdiff(colnames(tt2), colnames(tt)),
    c("from_stop_lon", "from_stop_lat", "to_stop_lon", "to_stop_lat"))
})

test_that("stop_dist warning", {
  fst = filter_stop_times(gtfs_routing, "2018-10-01", 7*3600, 24*3600)
  expect_error(travel_times(filtered_stop_times = fst, stop_name = "One",
                            time_range = 3600, stop_dist_check = 30))
})

test_that("stop times are filtered correctly", {
  expect_error(filter_stop_times(gtfs_routing, "2018-09-28", "07:00:00", "08:00:00"))
  expect_error(filter_stop_times(gtfs_routing, "2018-10-01", "07:00:00", "06:00:00"))
  expect_error(filter_stop_times(gtfs_routing, "2018-10-01", "08:00:00", "09:00:00"))

  fst = filter_stop_times(gtfs_routing, "2018-10-01", "07:00:00", "08:00:00")
  expect_true(all(c("transfers", "stops") %in% names(attributes(fst))))
  expect_error(travel_times(gtfs_routing$stop_times, "One"))
})

test_that("travel_time works with different params", {
  fst = filter_stop_times(gtfs_routing, "2018-10-01", 7*3600, 24*3600)
  expect_warning(travel_times(fst, "One", max_departure_time = 7*3600+5*60), "max_departure_time is deprecated, use time_range")
  expect_warning(travel_times(fst, "One", max_departure_time = "07:05:00"), "max_departure_time is deprecated, use time_range")
  expect_warning(
    expect_error(travel_times(fst, "One", time_range = 1800, max_departure_time = "07:45:00")),
    "max_departure_time is deprecated, use time_range")
  expect_error(travel_times(fst, "unknown stop"))
  expect_warning(
    expect_error(travel_times(fst, "One", max_departure_time = "06:45:00")),
    "max_departure_time is deprecated, use time_range")
})

test_that("transfers for travel_times", {
  fst = filter_stop_times(gtfs_routing, "2018-10-01", 7*3600, 24*3600)
  tt = travel_times(
    filtered_stop_times = fst,
    stop_name = "One",
    time_range = 3600) %>%
    arrange(to_stop_id)
  expect_equal(tt$transfers,
               c(0, 0, 0, 1, 0, 0, 1, 0))
})

test_that("travel_times return type", {
  fst = filter_stop_times(gtfs_routing, "2018-10-01", 7*3600, 24*3600)
  expect_s3_class(travel_times(fst, "One", return_DT = TRUE), "data.frame")
  expect_s3_class(travel_times(fst, "One", return_DT = FALSE), "data.frame")
  expect_s3_class(travel_times(fst, "One"), "tbl_df")
  expect_s3_class(travel_times(fst, "One", return_DT = FALSE), "tbl_df")
  expect_s3_class(travel_times(fst, "One", return_DT = TRUE), "data.table")
})

test_that("travel_times from stop with departures from transfer stops", {
  fst = filter_stop_times(gtfs_routing, "2018-10-01", 7*3600, 9*3600)

  g2 = gtfs_routing
  g2$stops[nrow(g2$stops)+1,] <- list("stop0", "Zero", 46.9596, 7.39071, "", 0)
  g2$transfers[nrow(g2$transfers)+1,] <- list("stop0", "stop1a", 2, 1)

  fst2 = filter_stop_times(g2, "2018-10-01", 7*3600, 24*3600)
  expect_equal(nrow(travel_times(fst2, "Zero")), 9)
})

test_that("travel_times with arrival=TRUE stop_name", {
  fst = filter_stop_times(gtfs_routing, "2018-10-01", 7*3600, hhmmss_to_seconds("08:00:00"))
  tt_to = travel_times(fst, stop_name = "Four", arrival = TRUE)
  tt_to <- tt_to[order(tt_to$from_stop_id),]
  expect_equal(tt_to$journey_arrival_time, hms::hms(c(37,37,37,0,37,37,41,41)*60+7*3600))
})

test_that("catch invalid params", {
  expect_error(travel_times(gtfs_routing, stop_name = "One"), "Travel times cannot be calculated with an unfiltered tidygtfs object. Use filter_feed_by_date().")
  fst = filter_stop_times(gtfs_routing, "2018-10-01", 7*3600, 24*3600)
  expect_error(raptor(fst, attributes(fst)$transfers, stop_id = "stop1a", max_transfers = -1), "max_transfers is less than 0")
  expect_error(travel_times(fst, stop_name = "One", max_transfers = -1), "max_transfers is less than 0")
})

test_that("travel_times with filtered feed", {
  x1 = filter_stop_times(gtfs_routing, "2018-10-01", 7*3600, 24*3600)
  g2 = filter_feed_by_date(gtfs_routing, "2018-10-01", 7*3600, 24*3600)

  tt1 = travel_times(filtered_stop_times = x1,
                     stop_name = "One", time_range = 3600)
  tt2 = travel_times(filtered_stop_times = g2,
                     stop_name = "One", time_range = 3600)
  expect_equal(tt1, tt2)
})

test_that("time_range param", {
  st = filter_feed_by_date(gtfs_routing, "2018-10-01")

  tt1 = travel_times(st, stop_name = "One", time_range = c("07:09:00", "07:59:00"))
  expect_equal((as.numeric(unique(tt1$journey_departure_time))-7*3600)/60,
               c(9,10,12,17))
  tt2 = travel_times(st, stop_name = "Three", time_range = c("07:20:00", "07:20:00"), arrival = T)
  expect_equal(as.numeric(tt2$journey_departure_time), 7*3600+20*60)
  tt3 = travel_times(st, stop_name = "Three", time_range = c("07:20:00", "07:23:00"), arrival = T)
  expect_equal(tt3$journey_departure_time[2], hhmmss_to_hms("07:17:00"))
})

test_that("example pipe", {
  pipe1 = gtfs_routing |>
    filter_feed_by_date("2018-10-01") |>
    travel_times("One", time_range = c("07:09:00", "07:19:00"))

  pipe2 = gtfs_routing |>
    filter_stop_times("2018-10-01", "07:09:00") |>
    travel_times("One", time_range = 600)

  expect_equal(pipe1, pipe2)
})

test_that("feed without transfers", {
  g_no_transfers = gtfs_routing
  g_no_transfers$transfers <- NULL

  expect_warning(fst <- filter_stop_times(g_no_transfers, "2018-10-01", 7*3600, 24*3600),
                 "No transfers found in feed")

  tts = travel_times(filtered_stop_times = fst,
                     stop_name = "Two", time_range = 3600)
  expect_equal(tts$to_stop_name, c("Two", "Three", "Four"))
  expect_equal(tts$to_stop_id, c("stop2", "stop3a", "stop4"))
})

test_that("nyc feed", {
  nyc_path <- system.file("extdata", "google_transit_nyc_subway.zip", package = "tidytransit")
  nyc <- read_gtfs(nyc_path)

  .child_index = which(nyc$stops$location_type == 0)
  .parent_index = which(nyc$stops$location_type == 1)
  nyc$stops$stop_name[.child_index] <- paste0(nyc$stops$stop_name[.child_index], " (", nyc$stops$parent_station[.child_index], ")")
  nyc$stops$stop_name[.parent_index] <- paste0(nyc$stops$stop_name[.parent_index], " (", nyc$stops$stop_id[.parent_index], ")")

  length(unique(nyc$stops$stop_name))
  x2 = cluster_stops(nyc$stops)
  length(unique(x2$stop_name_cluster))

  nyc_st <- filter_stop_times(nyc, "2018-06-26", 7*3600, 9*3600)

  tts <- travel_times(nyc_st, "34 St - Herald Sq (D17)", return_coords = TRUE, stop_dist_check = FALSE)

  expect_is(tts, "data.frame")
})
