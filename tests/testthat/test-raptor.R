context("raptor travel time routing")

local_gtfs_path <- system.file("extdata", "routing.zip", package = "tidytransit")
g <- read_gtfs(local_gtfs_path)
g <- set_hms_times(g)
test_from_stop_ids <- c("stop1a", "stop1b")

stop_times = g$stop_times
stop_times_0709 = dplyr::filter(g$stop_times, departure_time_hms >= 7*3600+10*60)
stop_times_0711 = dplyr::filter(g$stop_times, departure_time_hms >= 7*3600+11*60)
stop_times_0715 = dplyr::filter(g$stop_times, departure_time_hms >= 7*3600+15*60)
transfers = g$transfers

test_that("travel times wrapper function", {
  fst = filter_stop_times(g, "2018-10-01", 0, 24*3600)
  tt = travel_times(
    filtered_stop_times = fst, 
    stop_name = "One", 
    time_range = 3600)
  expect_equal(nrow(tt), length(unique(g$stops$stop_name)))
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

test_that("travel_time works with different params", {
  fst = filter_stop_times(g, "2018-10-01", 0, 24*3600)
  travel_times(fst, "One", max_departure_time = 7*3600+5*60)
  travel_times(fst, "One", max_departure_time = "07:05:00")
  expect_error(travel_times(fst, "One", time_range = 1800,  max_departure_time = "07:45:00"))
  expect_error(travel_times(fst, "unknown stop"))
  expect_error(travel_times(fst, "One", max_departure_time = "06:45:00"))
})

test_that("stop times are filtered correctly", {
  expect_error(filter_stop_times(g, "2018-09-28", "07:00:00", "08:00:00"))
  expect_error(filter_stop_times(g, "2018-10-01", "07:00:00", "06:00:00"))
  expect_error(filter_stop_times(g, "2018-10-01", "08:00:00", "09:00:00"))
  
  fst = filter_stop_times(g, "2018-10-01", "07:00:00", "08:00:00")
  expect_true(all(c("transfers", "stops") %in% names(attributes(fst))))
  expect_error(travel_times(g$stop_times, "One"))
})

test_that("raptor travel times", {
  r = raptor(stop_times, transfers,
             test_from_stop_ids, time_range = 3600,
             keep = "shortest")
  actual = r[order(to_stop_id), travel_time]
  
  expected = c(
    0,          # stop1a 00:00:00
    0,          # stop1b 00:00:00
    04*60 + 00, # stop2  00:04:00 
    06*60 + 10, # stop3a 00:06:10  :18 - :12 + transfer
    06*60 + 00, # stop3b 00:06:00  :18 - :12
    20*60 + 00, # stop4  00:20:00  :37 - :17
    05*60 + 00, # stop5  00:05:00  :15 - :10
    10*60 + 00, # stop6  00:10:00  :20 - :10
    15*60 + 00, # stop7  00:15:00  :25 - :10
    12*60 + 10, # stop8a 00:12:10  :24 - :12 + transfer
    12*60 + 00  # stop8b 00:12:00  :24 - :12
  )
  expect_equal(actual, expected)
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

  expect_equal(shortest_tt, earliest_arrival_tt)
})

test_that("travel_time with one stop and reduced time_range", {
  r = raptor(stop_times_0709, transfers, "stop1a",
             time_range = 30,
             keep = "shortest")[order(to_stop_id)]
  actual <- r$travel_time
  
  expected = c(
    00*60 + 00, # stop1a 00:00:00
    00*60 + 10, # stop1b 00:00:10
    18*60 + 00, # stop3a 00:18:00
    18*60 + 10, # stop3b 00:18:10
    27*60 + 00, # stop4  00:27:00
    05*60 + 00, # stop5  00:05:00
    10*60 + 00, # stop6  00:10:00
    15*60 + 00, # stop7  00:15:00
    22*60 + 00, # stop8a 00:22:00
    22*60 + 10  # stop8b 00:22:10
  )
  expect_equal(actual, expected)
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
  expect_equal(r[to_stop_id == "stop4"]$transfers, c(1,0))
  expect_equal(r[to_stop_id == "stop8a"]$transfers, 2)
  expect_equal(r[to_stop_id == "stop8b"]$transfers, 1)
})

test_that("transfers for travel_times", {
  fst = filter_stop_times(g, "2018-10-01", 0, 24*3600)
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
  expect_equal(max(raptor(stop_times, transfers, test_from_stop_ids, max_transfers = 2)$transfers), 2)
  expect_equal(max(raptor(stop_times, transfers, test_from_stop_ids, max_transfers = NULL)$transfers), 2)
})

test_that("travel_times return type", {
  fst = filter_stop_times(g, "2018-10-01", 0, 24*3600)
  expect_s3_class(travel_times(fst, "One", return_DT = TRUE), "data.frame")
  expect_s3_class(travel_times(fst, "One", return_DT = FALSE), "data.frame")
  expect_s3_class(travel_times(fst, "One"), "tbl_df")
  expect_s3_class(travel_times(fst, "One", return_DT = FALSE), "tbl_df")
  expect_s3_class(travel_times(fst, "One", return_DT = TRUE), "data.table")
})

test_that("travel_times from stop with departures from transfer stops", {
  g2 = g
  g2$stops[nrow(g2$stops)+1,] <- c("stop0", "Zero", 46.9596, 7.39071, NA, 0)
  g2$transfers[nrow(g2$transfers)+1,] <- c("stop0", "stop1a", "2", 1)
  g2$transfers$min_transfer_time <- as.numeric(g2$transfers$min_transfer_time)
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
    45*60, # stop4
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
    45*60 - 00, # stop4
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
                arrival = TRUE, time_range = 5*60,
                keep = "shortest")
  setorder(rptr_2, from_stop_id)
  arr_expected_2 = c(
    40*60, # stop1a 
    40*60, # stop1b 
    40*60, # stop2   
    40*60, # stop3a
    40*60, # stop3b
    45*60, # stop4
    40*60, # stop5
    40*60, # stop6
    41*60, # stop7
    41*60, # stop8a
    41*60  # stop8b
  )+7*3600
  dep_expected_2 = c(
    17*60 - 10, # stop1a
    17*60 - 00, # stop1b 
    10*60 - 00, # stop2   
    29*60 - 00, # stop3a
    29*60 - 10, # stop3b
    45*60 - 00, # stop4
    15*60 - 00, # stop5
    21*60 - 00, # stop6
    26*60 - 00, # stop7
    32*60 - 00, # stop8a
    32*60 - 10  # stop8b
  )+7*3600
  tt_expected_2 = arr_expected_2 - dep_expected_2

  expect_equal(rptr_2$journey_arrival_time, arr_expected_2)
  expect_equal(rptr_2$journey_departure_time, dep_expected_2)
  expect_equal(rptr_2$travel_time, tt_expected_2)
})

test_that("latest arrivals are correct", {
  r1 = raptor(stop_times, transfers, stop_ids = "stop1a", arrival = FALSE, keep = "latest")
  expect_equal(r1[which(r1$to_stop_id == "stop4")]$journey_arrival_time, 41*60+7*3600)
  expect_equal(r1[which(r1$to_stop_id == "stop3b")]$journey_arrival_time, 28*60+7*3600+10)
  
  r2 = raptor(stop_times, transfers, stop_ids = "stop4", arrival = TRUE, keep = "latest")
  expect_equal(r2[which(r2$from_stop_id == "stop1a")]$journey_arrival_time, 45*60+7*3600)
  expect_equal(r2[which(r2$from_stop_id == "stop4")]$journey_arrival_time, 45*60+7*3600)
}

test_that("travel_times with arrival=TRUE stop_name", {
  fst = filter_stop_times(g, "2018-10-01", 0, 24*3600)
  tt_to = travel_times(fst, stop_name = "Four", arrival = TRUE)
  tt_to <- tt_to[order(tt_to$from_stop_id),]
  expect_equal(tt_to$journey_arrival_time, hms::hms(c(37,37,37,45,37,37,41,41)*60+7*3600))
})


