context("raptor travel time routing")

local_gtfs_path <- system.file("extdata", "routing.zip", package = "tidytransit")
g <- read_gtfs(local_gtfs_path, local=TRUE)
g <- set_hms_times(g)
from_stop_ids <- c("stop1a", "stop1b")

stop_times = g$stop_times
stop_times_0709 = dplyr::filter(g$stop_times, departure_time_hms >= 7*3600+09*60)
stop_times_0711 = dplyr::filter(g$stop_times, departure_time_hms >= 7*3600+11*60)
stop_times_0715 = dplyr::filter(g$stop_times, departure_time_hms >= 7*3600+15*60)
transfers = g$transfers

test_that("travel times wrapper function", {
  fst = filter_stop_times(g, "2018-10-01", 0, 24*3600)
  tt = travel_times(
    filtered_stop_times = fst, 
    from_stop_name = "One", 
    departure_time_range = 3600)
  expect_equal(nrow(tt), length(unique(g$stops$stop_name)))
  expect_equal(tt %>% dplyr::filter(stop_name == "One") %>% dplyr::pull(travel_time), 0)
  expect_equal(tt$travel_time[which(tt$stop_name == "One")], 0)
  expect_equal(tt$travel_time[which(tt$stop_name == "Two")], 4*60)
  expect_equal(tt$travel_time[which(tt$stop_name == "Three")], (18-12)*60)
  expect_equal(tt$travel_time[which(tt$stop_name == "Four")], (37-17)*60)
  expect_equal(tt$travel_time[which(tt$stop_name == "Five")], (15-10)*60)
  expect_equal(tt$travel_time[which(tt$stop_name == "Six")], (20-10)*60)
  expect_equal(tt$travel_time[which(tt$stop_name == "Seven")], (25-10)*60)
  expect_equal(tt$travel_time[which(tt$stop_name == "Eight")], (24-12)*60)
})

test_that("travel_time works with different params", {
  fst = filter_stop_times(g, "2018-10-01", 0, 24*3600)
  travel_times(fst, "One", max_departure_time = 7*3600+5*60)
  travel_times(fst, "One", max_departure_time = "07:05:00")
  expect_warning(travel_times(fst, "One", departure_time_range = 1800,  max_departure_time = "07:45:00"))
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
             from_stop_ids, departure_time_range = 3600,
             keep = "shortest")
  actual = r[order(stop_id), travel_time]
  
  expected = as.numeric(hms::parse_hms(c(
    "stop1a" = "00:00:00",
    "stop1b" = "00:00:00",
    "stop2"  = "00:04:00",
    "stop3a" = "00:06:10", # :18 - :12 + transfer
    "stop3b" = "00:06:00", # :18 - :12
    "stop4"  = "00:20:00", # :37 - :17
    "stop5"  = "00:05:00", # :15 - :10
    "stop6"  = "00:10:00", # :20 - :10
    "stop7"  = "00:15:00", # :25 - :10
    "stop8a" = "00:12:10", # :24 - :12 + transfer
    "stop8b" = "00:12:00"  # :24 - :12
  )))
  expect_equal(actual, expected)
})

test_that("ea and tt return the same result for one departure", {
  travel_time = raptor(stop_times, transfers, from_stop_ids,
                       departure_time_range = 60,
                       keep = "shortest")
  travel_time <- travel_time[order(stop_id), travel_time]
  
  earliest_arrival = raptor(stop_times, transfers, from_stop_ids,
                            departure_time_range = 60,
                            keep = "earliest")
  earliest_arrival_tt <- earliest_arrival[order(stop_id), min_arrival_time]
  earliest_arrival_tt <- earliest_arrival_tt - 7*3600
  
  expect_equal(travel_time, earliest_arrival_tt)
})

test_that("travel_time with one stop and reduced departure_time_range", {
  r = raptor(stop_times_0709, transfers, "stop1a",
             departure_time_range = 120,
             keep = "shortest")[order(stop_id)]
  actual <- r[order(stop_id), travel_time]
  
  expected = as.numeric(hms::parse_hms(c(
    "stop1a" = "00:00:00",
    "stop3a" = "00:18:00",
    "stop3b" = "00:18:10",
    "stop4"  = "00:27:00",
    "stop5"  = "00:05:00",
    "stop6"  = "00:10:00",
    "stop7"  = "00:15:00",
    "stop8a" = "00:22:00",
    "stop8b" = "00:22:10"
  )))
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
  expect_warning(raptor(st, tr, "stop1"))
  expect_warning(raptor(st, tr, 42))
  
  # time range type
  expect_error(raptor(st, tr, "stop5", departure_time_range = "char"))
  expect_error(raptor(st, tr, "stop5", departure_time_range = NULL))
  expect_error(raptor(st, tr, "stop5", departure_time_range = 0))
  expect_error(raptor(st, tr, "stop5", departure_time_range = -99))
  expect_error(raptor(st, tr, "stop5", departure_time_range = hms::hms(900)))
  
  # empty results
  expect_equal(nrow(raptor(st, tr, "stop5", departure_time_range = 60)), 0)
})

test_that("earliest arrival times", {
  r = raptor(stop_times, transfers, "stop2", keep = "earliest")
  actual = r[order(stop_id), min_arrival_time]
  expected = as.numeric(hms::parse_hms(c(
    "stop2"  = "07:05:00", # departure time
    "stop3a" = "07:11:00",
    "stop3b" = "07:11:10",
    "stop4"  = "07:37:00",
    "stop8a" = "07:24:10",
    "stop8b" = "07:24:00"
  )))
  expect_equal(actual, expected)
})

test_that("earliest arrival time without transfers", {
  r = raptor(stop_times, NULL, from_stop_ids, keep = "earliest")
  actual = r[order(stop_id), min_arrival_time]
  expected = as.numeric(hms::parse_hm(c(
    "stop1a" = "07:00",
    "stop1b" = "07:12",
    "stop2"  = "07:04",
    "stop3a" = "07:11",
    "stop3b" = "07:18",
    "stop4"  = "07:37",
    "stop5"  = "07:15",
    "stop6"  = "07:20",
    "stop7"  = "07:25",
    "stop8a" = "07:32",
    "stop8b" = "07:24"
  )))
  expect_equal(actual, expected)
})

test_that("transfers are returned", {
  r = raptor(stop_times, transfers, "stop2", keep = "all")
  setorder(r, travel_time)
  expect_equal(r[stop_id == "stop3a"]$transfers, c(0,0))
  expect_equal(r[stop_id == "stop4"]$transfers, c(1,0))
  expect_equal(r[stop_id == "stop8a"]$transfers, 2)
  expect_equal(r[stop_id == "stop8b"]$transfers, 1)
})

test_that("transfers for travel_times", {
  fst = filter_stop_times(g, "2018-10-01", 0, 24*3600)
  tt = travel_times(
    filtered_stop_times = fst, 
    from_stop_name = "One", 
    departure_time_range = 3600) %>% 
    arrange(stop_id)
  expect_equal(tt$transfers, 
               c(0, 0, 0, 1, 0, 0, 1, 0))
})

test_that("travel_times return type", {
  fst = filter_stop_times(g, "2018-10-01", 0, 24*3600)
  expect_s3_class(travel_times(fst, "One", return_DT = TRUE), "data.frame")
  expect_s3_class(travel_times(fst, "One", return_DT = FALSE), "data.frame")
  expect_s3_class(travel_times(fst, "One"), "tbl_df")
  expect_s3_class(travel_times(fst, "One", return_DT = FALSE), "tbl_df")
  expect_s3_class(travel_times(fst, "One", return_DT = TRUE), "data.table")
})
