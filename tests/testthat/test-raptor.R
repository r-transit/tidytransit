context("raptor travel time routing")

local_gtfs_path <- system.file("extdata", "routing.zip", package = "tidytransit")
gtfs_routing <- read_gtfs(local_gtfs_path)
test_from_stop_ids <- c("stop1a", "stop1b")

stop_times = gtfs_routing$stop_times
stop_times_0710 = dplyr::filter(gtfs_routing$stop_times, departure_time >= 7*3600+10*60)
stop_times_0711 = dplyr::filter(gtfs_routing$stop_times, departure_time >= 7*3600+11*60)
stop_times_0715 = dplyr::filter(gtfs_routing$stop_times, departure_time >= 7*3600+15*60)
transfers = gtfs_routing$transfers

test_that("travel times wrapper function", {
  fst = filter_stop_times(gtfs_routing, "2018-10-01", 0, 24*3600)
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
  fst = filter_stop_times(gtfs_routing, "2018-10-01", 0, 24*3600)
  expect_error(travel_times(filtered_stop_times = fst, stop_name = "One",
                            time_range = 3600, stop_dist_check = 30))
})

test_that("travel_time works with different params", {
  fst = filter_stop_times(gtfs_routing, "2018-10-01", 0, 24*3600)
  travel_times(fst, "One", max_departure_time = 7*3600+5*60)
  travel_times(fst, "One", max_departure_time = "07:05:00")
  expect_error(travel_times(fst, "One", time_range = 1800,  max_departure_time = "07:45:00"))
  expect_error(travel_times(fst, "unknown stop"))
  expect_error(travel_times(fst, "One", max_departure_time = "06:45:00"))
})

test_that("stop times are filtered correctly", {
  expect_error(filter_stop_times(gtfs_routing, "2018-09-28", "07:00:00", "08:00:00"))
  expect_error(filter_stop_times(gtfs_routing, "2018-10-01", "07:00:00", "06:00:00"))
  expect_error(filter_stop_times(gtfs_routing, "2018-10-01", "08:00:00", "09:00:00"))

  fst = filter_stop_times(gtfs_routing, "2018-10-01", "07:00:00", "08:00:00")
  expect_true(all(c("transfers", "stops") %in% names(attributes(fst))))
  expect_error(travel_times(gtfs_routing$stop_times, "One"))
})

test_that("raptor travel times", {
  actual_tbl = raptor(stop_times, transfers,
                      test_from_stop_ids, time_range = 3600,
                      keep = "shortest")

  expected_tbl = dplyr::tribble(~travel_time_expected, ~to_stop_id, ~from_stop_id,
                                0,          "stop1a", "stop1a", # 00:00:00
                                0,          "stop1b", "stop1b", # 00:00:00
                                04*60 + 00, "stop2",  "stop1a", # 00:04:00
                                06*60 + 10, "stop3a", "stop1b", # 00:06:10  :18 - :12 + transfer
                                06*60 + 00, "stop3b", "stop1b", # 00:06:00  :18 - :12
                                20*60 + 00, "stop4",  "stop1b", # 00:20:00  :37 - :17
                                05*60 + 00, "stop5",  "stop1a", # 00:05:00  :15 - :10
                                10*60 + 00, "stop6",  "stop1a", # 00:10:00  :20 - :10
                                15*60 + 00, "stop7",  "stop1a", # 00:15:00  :25 - :10
                                12*60 + 10, "stop8a", "stop1b", # 00:12:10  :24 - :12 + transfer
                                12*60 + 00, "stop8b", "stop1b", # 00:12:00  :24 - :12
  )

  check = dplyr::inner_join(actual_tbl, expected_tbl, c("from_stop_id", "to_stop_id"))
  check |> filter(travel_time != travel_time_expected)
  expect_equal(check$travel_time, check$travel_time_expected)
})

test_that("only param stop_ids are returned as from_stop_ids", {
  r = raptor(stop_times, transfers, "stop3a")
  expect_equal(unique(r$from_stop_id), "stop3a")
})

test_that("ea and tt return the same result for one departure", {
  shortest = raptor(stop_times, transfers, test_from_stop_ids,
                    time_range = 60,
                    keep = "shortest")[order(to_stop_id)]
  shortest_tt <- shortest$travel_time

  earliest_arrival = raptor(stop_times, transfers, test_from_stop_ids,
                            time_range = 60,
                            keep = "earliest")[order(to_stop_id)]
  earliest_arrival_tt <- earliest_arrival$journey_arrival_time - 7*3600

  check = inner_join(shortest[,1:3], earliest_arrival[,1:3], c("from_stop_id", "to_stop_id")) |>
    filter(!to_stop_id %in% test_from_stop_ids)
  check |> filter(travel_time.x != travel_time.y)

  expect_equal(check$travel_time.x, check$travel_time.y)
})

test_that("raptor with one stop and reduced time_range", {
  expected_tbl = dplyr::tribble(~expected_travel_time, ~to_stop_id,
                                00*60 + 00, "stop1a",  # 00:00:00
                                00*60 + 10, "stop1b",  # 00:00:10
                                18*60 + 00, "stop3a",  # 00:18:00
                                18*60 + 10, "stop3b",  # 00:18:10
                                27*60 + 00, "stop4",   # 00:27:00
                                05*60 + 00, "stop5",   # 00:05:00
                                10*60 + 00, "stop6",   # 00:10:00
                                15*60 + 00, "stop7",   # 00:15:00
                                22*60 + 00, "stop8a",  # 00:22:00
                                22*60 + 10, "stop8b",  # 00:22:10
  )

  actual_tbl = raptor(stop_times_0710, transfers, "stop1a",
                      time_range = 30,
                      keep = "shortest")[order(to_stop_id)]

  check = dplyr::full_join(actual_tbl, expected_tbl, "to_stop_id")
  check |> filter(travel_time != expected_travel_time)

  expect_equal(check$travel_time, check$expected_travel_time)
})

test_that("parameters are checked", {
  st = stop_times
  tr = transfers
  # keeps
  raptor(st, tr, c("stop1a", "stop1b"), keep = "all")
  raptor(st, tr, c("stop1a", "stop1b"), keep = "shortest")
  raptor(st, tr, c("stop1a", "stop1b"), keep = "earliest")
  expect_error(raptor(st, tr, c("stop1a", "stop1b"), keep = NULL))
  expect_error(raptor(st, tr, c("stop1a", "stop1b"), keep = "NULL"))

  # non-existent stop_id
  expect_warning(raptor(st, tr, "stop99"))
  expect_error(raptor(st, tr, 42))

  # time range type
  expect_error(raptor(st, tr, "stop5", time_range = "char"))
  expect_error(raptor(st, tr, "stop5", time_range = NULL))
  expect_error(raptor(st, tr, "stop5", time_range = 0))
  expect_error(raptor(st, tr, "stop5", time_range = -99))
  expect_error(raptor(st, tr, "stop5", time_range = hms::hms(900)))

  # empty results
  expect_equal(nrow(raptor(st, tr, "stop5", time_range = 60)), 1)
})

test_that("earliest arrival times", {
  r = raptor(stop_times, transfers, "stop2", keep = "earliest")
  actual = r[order(to_stop_id), journey_arrival_time]
  expected = c(
    7*3600 + 00*60 + 00, # stop2  07:05:00 departure time
    7*3600 + 11*60 + 00, # stop3a 07:11:00
    7*3600 + 11*60 + 10, # stop3b 07:11:10
    7*3600 + 37*60 + 00, # stop4  07:37:00
    7*3600 + 24*60 + 10, # stop8a 07:24:10
    7*3600 + 24*60 + 00  # stop8b 07:24:00
  )
  expect_equal(actual, expected)
})

test_that("earliest arrival time without transfers", {
  r = raptor(stop_times, NULL, test_from_stop_ids, keep = "earliest")
  actual = r[order(to_stop_id), journey_arrival_time]
  expected = c(
    7*3600 + 00*60, # stop1a 07:00
    7*3600 + 00*60, # stop1b 07:12
    7*3600 + 04*60, # stop2  07:04
    7*3600 + 11*60, # stop3a 07:11
    7*3600 + 18*60, # stop3b 07:18
    7*3600 + 37*60, # stop4  07:37
    7*3600 + 15*60, # stop5  07:15
    7*3600 + 20*60, # stop6  07:20
    7*3600 + 25*60, # stop7  07:25
    7*3600 + 32*60, # stop8a 07:32
    7*3600 + 24*60  # stop8b 07:24
  )
  expect_equal(actual, expected)
})

test_that("transfers are returned", {
  r = raptor(stop_times, transfers, "stop2", keep = "all")
  setorder(r, travel_time)
  expect_equal(r[to_stop_id == "stop3a"]$transfers, c(0,0))
  expect_equal(r[to_stop_id == "stop4"]$transfers, c(1,1))
  expect_equal(r[to_stop_id == "stop8a"]$transfers, c(1,1))
  expect_equal(r[to_stop_id == "stop8b"]$transfers, c(1,1))
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

test_that("only max_transfers are used", {
  expect_equal(max(raptor(stop_times, transfers, test_from_stop_ids, max_transfers = 0)$transfers), 0)
  expect_equal(max(raptor(stop_times, transfers, test_from_stop_ids, max_transfers = 1)$transfers), 1)
  expect_equal(max(raptor(stop_times, transfers, test_from_stop_ids, max_transfers = NULL)$transfers), 1)
})

test_that("travel_times return type", {
  fst = filter_stop_times(gtfs_routing, "2018-10-01", 0, 24*3600)
  expect_s3_class(travel_times(fst, "One", return_DT = TRUE), "data.frame")
  expect_s3_class(travel_times(fst, "One", return_DT = FALSE), "data.frame")
  expect_s3_class(travel_times(fst, "One"), "tbl_df")
  expect_s3_class(travel_times(fst, "One", return_DT = FALSE), "tbl_df")
  expect_s3_class(travel_times(fst, "One", return_DT = TRUE), "data.table")
})

test_that("travel_times from stop with departures from transfer stops", {
  fst = filter_stop_times(gtfs_routing, "2018-10-01", 0, 9*3600)

  g2 = gtfs_routing
  g2$stops[nrow(g2$stops)+1,] <- list("stop0", "Zero", 46.9596, 7.39071, "", 0)
  g2$transfers[nrow(g2$transfers)+1,] <- list("stop0", "stop1a", 2, 1)

  fst2 = filter_stop_times(g2, "2018-10-01", 0, 24*3600)
  expect_equal(nrow(travel_times(fst2, "Zero")), 9)
})

test_that("raptor from stop without departures", {
  expect_warning(raptor(stop_times_0711, transfers, "stop2"))

  expect_equal(nrow(raptor(stop_times_0711, transfers, "stop4")), 1)
})

test_that("empty return data.table has the same columns as correct", {
  r1 = suppressWarnings(raptor(stop_times_0711, transfers, "stop2"))
  r2 = raptor(stop_times_0711, transfers, "stop3a")
  expect_equal(colnames(r1), colnames(r2))
})

test_that("raptor errors without any stop_ids", {
  expect_error(raptor(stop_times, transfers))
})

test_that("raptor travel times with arrival=TRUE", {
  rptr = raptor(stop_times, transfers, stop_ids = "stop4", arrival = TRUE, keep = "shortest")
  setorder(rptr, from_stop_id)
  arr_expected = c(
    37*60, # stop1a
    37*60, # stop1b
    37*60, # stop2
    37*60, # stop3a
    37*60, # stop3b
    00*60, # stop4
    37*60, # stop5
    37*60, # stop6
    41*60, # stop7
    41*60, # stop8a
    41*60  # stop8b
  )+7*3600
  dep_expected = c(
    17*60 - 10, # stop1a
    17*60 - 00, # stop1b
    10*60 - 00, # stop2
    29*60 - 00, # stop3a
    29*60 - 10, # stop3b
    00*60 - 00, # stop4
    15*60 - 00, # stop5
    21*60 - 00, # stop6
    26*60 - 00, # stop7
    32*60 - 00, # stop8a
    32*60 - 10  # stop8b
  )+7*3600
  tt_expected = arr_expected - dep_expected

  expect_equal(rptr$journey_arrival_time, arr_expected)
  expect_equal(rptr$journey_departure_time, dep_expected)
  expect_equal(rptr$travel_time, tt_expected)
  expect_equal(unique(rptr$to_stop_id), "stop4")
})

test_that("raptor with arrival=TRUE and reduced time_range", {
  rptr_2 = raptor(stop_times, transfers, stop_ids = "stop4",
                  arrival = TRUE, time_range = 6*60,
                  keep = "shortest")
  setorder(rptr_2, from_stop_id)
  arr_expected_2 = c(
    41*60, # stop1a
    41*60, # stop1b
    41*60, # stop2
    41*60, # stop3a
    41*60, # stop3b
    00*60, # stop4
    41*60, # stop5
    41*60, # stop6
    41*60, # stop7
    41*60, # stop8a
    41*60  # stop8b
  )+7*3600
  dep_expected_2 = c(
    17*60 - 10, # stop1a
    17*60 - 00, # stop1b
    10*60 - 00, # stop2
    23*60 - 10, # stop3a
    23*60 - 00, # stop3b
    00*60 - 00, # stop4
    15*60 - 00, # stop5
    22*60 - 00, # stop6
    26*60 - 00, # stop7
    32*60 - 00, # stop8a
    32*60 - 10  # stop8b
  )+7*3600
  tt_expected_2 = arr_expected_2 - dep_expected_2

  rptr_2$arr_expected_2_time <- arr_expected_2
  rptr_2$dep_expected_2_time <- dep_expected_2
  rptr_2 |> filter(arr_expected_2_time != journey_arrival_time | dep_expected_2_time != journey_departure_time)

  expect_equal(rptr_2$journey_arrival_time, arr_expected_2)
  expect_equal(rptr_2$journey_departure_time, dep_expected_2)
  expect_equal(rptr_2$travel_time, tt_expected_2)
})

test_that("latest arrivals are correct", {
  r0 = raptor(stop_times, transfers, stop_ids = "stop1b", arrival = FALSE, keep = "all")
  r1 = raptor(stop_times, transfers, stop_ids = "stop1b", arrival = FALSE, keep = "latest")
  expect_equal(r1[which(r1$to_stop_id == "stop4")]$journey_arrival_time, 37*60+7*3600)
  expect_equal(r1[which(r1$to_stop_id == "stop3a")]$journey_arrival_time, 28*60+7*3600)

  r2 = raptor(stop_times, transfers, stop_ids = "stop4", arrival = TRUE, keep = "latest")
  expect_equal(r2[which(r2$from_stop_id == "stop1a")]$journey_arrival_time, 45*60+7*3600)
  expect_equal(r2[which(r2$from_stop_id == "stop4")]$journey_arrival_time, 8*3600)
})

test_that("travel_times with arrival=TRUE stop_name", {
  fst = filter_stop_times(gtfs_routing, "2018-10-01", 0, 24*3600)
  tt_to = travel_times(fst, stop_name = "Four", arrival = TRUE)
  tt_to <- tt_to[order(tt_to$from_stop_id),]
  expect_equal(tt_to$journey_arrival_time, hms::hms(c(37,37,37,0,37,37,41,41)*60+7*3600))
})

test_that("set_num_times w/o hms or num", {
  local_gtfs_path = system.file("extdata", "routing.zip", package = "tidytransit")
  g2 = read_gtfs(local_gtfs_path)
  g_st_dt = as.data.table(g2$stop_times)
  set_num_times(g_st_dt)
  expect_true(all(c("arrival_time_num", "departure_time_num") %in% colnames(g_st_dt)))
})

test_that("catch invalid params", {
  expect_error(travel_times(gtfs_routing, stop_name = "One"), "Travel times cannot be calculated on an unfiltered tidygtfs object. Use filter_feed_by_date().")
  fst = filter_stop_times(gtfs_routing, "2018-10-01", 0, 24*3600)
  expect_error(raptor(fst, attributes(fst)$transfers, stop_id = "stop1a", max_transfers = -1), "max_transfers is less than 0")
  expect_error(travel_times(fst, stop_name = "One", max_transfers = -1), "max_transfers is less than 0")
})

test_that("raptor with filtered feed", {
  x1 = filter_stop_times(gtfs_routing, "2018-10-01", 0, 24*3600)
  g2 = filter_feed_by_date(gtfs_routing, "2018-10-01", 0, 24*3600)

  tt1 = travel_times(filtered_stop_times = x1,
                     stop_name = "One", time_range = 3600)
  tt2 = travel_times(filtered_stop_times = g2,
                     stop_name = "One", time_range = 3600)
  expect_equal(tt1, tt2)
})

test_that("filter feed without min/max time", {
  st.1 = filter_stop_times(gtfs_routing, "2018-10-01")
  st.2 = filter_stop_times(gtfs_routing, "2018-10-01", "00:00:00", 999*3600)
  expect_true(all(st.1 == st.2))
})

test_that("feed without transfers", {
  g_no_transfers = gtfs_routing
  g_no_transfers$transfers <- NULL

  expect_warning(fst <- filter_stop_times(g_no_transfers, "2018-10-01", 0, 24*3600),
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

test_that("routing with missing NA", {
  gtfs_routing2 = read_gtfs(system.file("extdata", "routing-NA-times.zip", package = "tidytransit"))
  fst1 = filter_stop_times(gtfs_routing, "2018-10-01", 0, 24*3600)
  fst2 = filter_stop_times(gtfs_routing2, "2018-10-01", 0, 24*3600)

  tts1a = raptor(gtfs_routing$stop_times, gtfs_routing$transfers, "stop1b")
  tts1b = raptor(fst1, attributes(fst1)$transfers, "stop1b")
  tts2 = raptor(fst2, attributes(fst2)$transfers, "stop1b")

  expect_equal(tts1a, tts2)
  expect_equal(tts1b, tts2)
})

test_that("raptor considers each stop_id as a separate starting journey", {
  possible_routes = read.csv("possible_routes.csv", sep = ";")
  all_stop_ids = sort(unique(stop_times$stop_id))

  rptr_all = raptor(stop_times, transfers, all_stop_ids, keep = "all") |>
    arrange(from_stop_id, to_stop_id) |> dplyr::as_tibble()

  rptr_stop_pairs = unique(rptr_all[,c("from_stop_id", "to_stop_id")])
  rptr_stop_pairs$raptor_route <- TRUE

  stop_pairs = dplyr::full_join(rptr_stop_pairs, possible_routes, c("from_stop_id", "to_stop_id")) |>
    arrange(from_stop_id, to_stop_id)

  missing_routes = stop_pairs |> filter(is.na(raptor_route) & possible == TRUE)
  expect_equal(nrow(missing_routes), 0)
})
