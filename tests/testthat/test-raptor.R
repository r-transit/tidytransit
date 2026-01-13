local_gtfs_path <- system.file("extdata", "routing.zip", package = "tidytransit")
gtfs_routing <- read_gtfs(local_gtfs_path)
test_from_stop_ids <- c("stop1a", "stop1b")

stop_times = gtfs_routing$stop_times
stop_times_0710 = dplyr::filter(gtfs_routing$stop_times, departure_time >= 7*3600+10*60)
stop_times_0711 = dplyr::filter(gtfs_routing$stop_times, departure_time >= 7*3600+11*60)
stop_times_0715 = dplyr::filter(gtfs_routing$stop_times, departure_time >= 7*3600+15*60)
transfers = gtfs_routing$transfers

# raptor with time range check
raptor. = function(stop_times, transfers, stop_ids, arrival = FALSE, time_range = 3600,
                   max_transfers = NULL, keep = "all") {
  rptr = raptor(stop_times, transfers, stop_ids, arrival, time_range,
                max_transfers = max_transfers, keep = keep)
  
  tw = setup_time_window(time_range, arrival, stop_times)
  if(arrival) {
    expect_true(all(rptr$journey_arrival_time >= tw[1] & rptr$journey_arrival_time <= tw[2]))
  } else {
    expect_true(all(rptr$journey_departure_time >= tw[1] & rptr$journey_departure_time <= tw[2]))
  }
  return(rptr)
}

test_that("raptor travel times", {
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
  
  actual_tbl = raptor.(stop_times, transfers, test_from_stop_ids, time_range = 3600, keep = "shortest")
  expect_identical(actual_tbl$to_stop_id[1:2], c("stop1a", "stop1b"))
  
  check = dplyr::inner_join(actual_tbl, expected_tbl, c("from_stop_id", "to_stop_id"))
  expect_equal(check$travel_time, check$travel_time_expected)
  
  set.seed(1)
  st = stop_times[sample(seq_len(nrow(stop_times))),]
  act2 = raptor.(st, transfers, test_from_stop_ids, time_range = 3600, keep = "shortest")
  expect_equal(actual_tbl, act2)
})

test_that("only param stop_ids are returned as from_stop_ids", {
  r = raptor.(stop_times, transfers, "stop3a")
  expect_equal(unique(r$from_stop_id), "stop3a")
})

test_that("ea and tt return the same result for one departure", {
  shortest = raptor.(stop_times, transfers, test_from_stop_ids,
                    time_range = 60,
                    keep = "shortest")[order(to_stop_id)]

  shortest_tt <- shortest$travel_time

  earliest_arrival = raptor.(stop_times, transfers, test_from_stop_ids,
                            time_range = 60,
                            keep = "earliest")[order(to_stop_id)]
  earliest_arrival_tt <- earliest_arrival$journey_arrival_time - 7*3600

  check = inner_join(shortest[,1:3], earliest_arrival[,1:3], c("from_stop_id", "to_stop_id")) %>%
    filter(!to_stop_id %in% test_from_stop_ids)
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

  actual_tbl = raptor.(stop_times_0710, transfers, "stop1a",
                      time_range = c("07:09:50", "07:10:30"),
                      keep = "shortest")

  check = dplyr::full_join(actual_tbl, expected_tbl, "to_stop_id")
  expect_equal(check$travel_time, check$expected_travel_time)
})

test_that("parameters are checked", {
  st = stop_times
  tr = transfers
  # keeps
  expect_no_condition(raptor(st, tr, c("stop1a", "stop1b"), keep = "all"))
  expect_no_condition(raptor(st, tr, c("stop1a", "stop1b"), keep = "shortest"))
  expect_no_condition(raptor(st, tr, c("stop1a", "stop1b"), keep = "earliest"))
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

test_that("pick transfers from attributes", {
  fst = filter_stop_times(gtfs_routing, "2018-10-01", 7*3600)
  r1 = raptor.(fst, stop_ids = "stop5")
  r2 = raptor.(gtfs_routing$stop_times, gtfs_routing$transfers, stop_ids = "stop5")
  expect_equal(r1, r2)
  expect_error(raptor.(gtfs_routing$stop_times, stop_ids = "stop5"), 'argument "transfers" is missing, with no default')
  expect_error(raptor.(gtfs_routing, stop_ids = "stop5"), "Travel times cannot be calculated with a tidygtfs object")
})

test_that("earliest arrival times", {
  r = raptor.(stop_times, transfers, "stop2", keep = "earliest")
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
  r = raptor.(stop_times, NULL, test_from_stop_ids, keep = "earliest")
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
  r = raptor.(stop_times, transfers, "stop2", keep = "all")
  setorder(r, travel_time)
  expect_equal(r[to_stop_id == "stop3a"]$transfers, c(0,0))
  expect_equal(r[to_stop_id == "stop4"]$transfers, c(1,1))
  expect_equal(r[to_stop_id == "stop8a"]$transfers, c(1,1))
  expect_equal(r[to_stop_id == "stop8b"]$transfers, c(1,1))
})

test_that("only max_transfers are used", {
  expect_equal(max(raptor.(stop_times, transfers, test_from_stop_ids, max_transfers = 0)$transfers), 0)
  expect_equal(max(raptor.(stop_times, transfers, test_from_stop_ids, max_transfers = 1)$transfers), 1)
  expect_equal(max(raptor.(stop_times, transfers, test_from_stop_ids, max_transfers = NULL)$transfers), 1)
})

test_that("raptor from stop without departures", {
  expect_warning(raptor.(stop_times_0711, transfers, "stop2"))
  expect_equal(nrow(raptor.(stop_times_0711, transfers, "stop4")), 1)
})

test_that("empty return data.table has the same columns as correct", {
  r1 = suppressWarnings(raptor.(stop_times_0711, transfers, "stop2"))
  r2 = raptor.(stop_times_0711, transfers, "stop3a")
  expect_equal(colnames(r1), colnames(r2))
})

test_that("raptor errors without any stop_ids", {
  expect_error(raptor.(stop_times, transfers))
})

test_that("raptor travel times with arrival=TRUE", {
  rptr = raptor.(stop_times, transfers, stop_ids = "stop4", 
                arrival = TRUE, keep = "shortest")
  expected = dplyr::tribble(~departure_time, ~arrival_time, ~from_stop_id,
                            17*60 - 10,   37*60, "stop1a",
                            17*60 - 00,   37*60, "stop1b",
                            10*60 - 00,   37*60, "stop2",
                            29*60 - 00,   37*60, "stop3a",
                            29*60 - 10,   37*60, "stop3b",
                           -15*60 - 00,  -15*60, "stop4",
                            15*60 - 00,   37*60, "stop5",
                            21*60 - 00,   37*60, "stop6",
                            26*60 - 00,   41*60, "stop7",
                            32*60 - 00,   41*60, "stop8a",
                            32*60 - 10,   41*60, "stop8b")
  expected$departure_time <- expected$departure_time+7*3600
  expected$arrival_time <- expected$arrival_time+7*3600
  
  comp = inner_join(as_tibble(rptr), expected, "from_stop_id")
  expect_equal(comp$journey_arrival_time, comp$arrival_time)
  expect_equal(comp$journey_departure_time, comp$departure_time)
  expect_equal(unique(rptr$to_stop_id), "stop4")
})

test_that("raptor with arrival=TRUE and reduced time_range", {
  expected = dplyr::tribble(~arrival_time, ~departure_time, ~from_stop_id,
                            41*60, 17*60 - 10, "stop1a",
                            41*60, 17*60 - 00, "stop1b",
                            41*60, 10*60 - 00, "stop2",
                            41*60, 23*60 - 10, "stop3a",
                            41*60, 23*60 - 00, "stop3b",
                            38*60, 38*60 - 00, "stop4",
                            41*60, 15*60 - 00, "stop5",
                            41*60, 22*60 - 00, "stop6",
                            41*60, 26*60 - 00, "stop7",
                            41*60, 32*60 - 00, "stop8a",
                            41*60, 32*60 - 10, "stop8b"
  )
  expected$arrival_time <- expected$arrival_time+7*3600
  expected$departure_time <- expected$departure_time+7*3600
  
  rptr = raptor.(stop_times, transfers, stop_ids = "stop4", 
                arrival = TRUE, time_range = c("07:38:00", "07:45:00"), 
                keep = "shortest")

  comp = inner_join(as_tibble(rptr), expected, "from_stop_id")
  expect_equal(comp$journey_arrival_time, comp$arrival_time)
  expect_equal(comp$journey_departure_time, comp$departure_time)
  expect_equal(unique(rptr$to_stop_id), "stop4")
})

test_that("raptor with with time_range vector", {
  r1.1 = raptor.(stop_times, transfers, "stop1a", time_range = c("07:00:00", "07:05:00"))
  expect_length(unique(r1.1$journey_departure_time), 2)
  r1.2 = raptor.(stop_times, transfers, "stop1b", time_range = c("07:11:00", "07:17:00"))
  expect_length(unique(r1.2$journey_departure_time), 3)

  r2.1 = raptor.(stop_times, transfers, "stop2", time_range = c("07:00:00", "07:05:00"))
  expect_equal(r2.1$journey_arrival_time[r2.1$to_stop_id == "stop8b"], 24*60+7*3600)
  r2.2 = raptor.(stop_times, transfers, "stop2", time_range = c("07:05:00", "07:10:00"))
  expect_equal(nrow(r2.2[r2.2$to_stop_id == "stop8b"]), 2)
  r2.3 = raptor.(stop_times, transfers, "stop2", time_range = c("07:05:01", "07:10:00"))
  expect_equal(r2.3$journey_arrival_time[r2.3$to_stop_id == "stop8b"], 24*60+7*3600)

  # with arrival
  r8.1 = raptor.(stop_times, transfers, "stop8b", time_range = c("07:00:00", "07:30:00"), arrival = TRUE)
  
  # behavior change after v1.7.1: stop8a has arrival time with min_transfer_time 10s
  expect_equal(
    sort(unique(r8.1$journey_arrival_time)-7*3600),
    c(0, 10, 24*60, 29*60))
  
  # behavior change after v1.7.1: transfer to 8a needs 10s and is outside of initial 0s time range
  r8.2 = raptor.(stop_times, transfers, "stop8b", time_range = c("07:32:00", "07:32:10"), arrival = TRUE) 
  expect_equal(sort(unique(r8.2$from_stop_id)), c("stop1a", "stop1b", "stop5", "stop6", "stop7", "stop8a", "stop8b"))
  
  # short time_ranges
  no_connections = raptor.(stop_times, transfers, "stop1a", time_range = c("07:09:00", "07:09:00")) %>% filter(to_stop_id == "stop4")
  expect_equal(nrow(no_connections), 0)
  one_connection = raptor.(stop_times, transfers, "stop1a", time_range = c("07:10:00", "07:10:00")) %>% filter(to_stop_id == "stop4")
  expect_equal(nrow(one_connection), 1)
  one_connection_with_transfer = raptor.(stop_times, transfers, "stop1a", time_range = c("07:11:50", "07:12:00")) %>% filter(to_stop_id == "stop4")
  expect_equal(nrow(one_connection_with_transfer), 1)
  expect_equal(one_connection_with_transfer$transfers, 1)
  three_connections = raptor.(stop_times, transfers, "stop1a", time_range = c("07:10:00", "07:20:00")) %>% filter(to_stop_id == "stop4")
  expect_equal(nrow(three_connections), 3)
})

test_that("latest arrivals are correct", {
  r0 = raptor.(stop_times, transfers, time_range = 7200, stop_ids = "stop1b", arrival = FALSE, keep = "all")
  r1 = raptor.(stop_times, transfers, time_range = 7200, stop_ids = "stop1b", arrival = FALSE, keep = "latest")
  expect_equal(r1[which(r1$to_stop_id == "stop4")]$journey_arrival_time, 37*60+7*3600)
  expect_equal(r1[which(r1$to_stop_id == "stop3a")]$journey_arrival_time, 28*60+7*3600)

  r2 = raptor.(stop_times, transfers, stop_ids = "stop4", arrival = TRUE, keep = "latest")
  expect_equal(r2[which(r2$from_stop_id == "stop1a")]$journey_arrival_time, 45*60+7*3600)
  expect_equal(r2[which(r2$from_stop_id == "stop4")]$journey_arrival_time, 7.75*3600)

  r6 = raptor.(stop_times, transfers, time_range = 7200, stop_ids = "stop6", keep = "latest")
  expect_equal(r6[which(r6$to_stop_id == "stop4")]$journey_arrival_time, 41*60+7*3600)
  r6 = raptor.(stop_times, transfers, time_range = 7200, stop_ids = "stop6", keep = "all")
})

test_that("set_num_times w/o hms or num", {
  local_gtfs_path = system.file("extdata", "routing.zip", package = "tidytransit")
  g2 = read_gtfs(local_gtfs_path)
  g_st_dt = as.data.table(g2$stop_times)
  set_num_times(g_st_dt)
  expect_true(all(c("arrival_time_num", "departure_time_num") %in% colnames(g_st_dt)))
})

test_that("filter feed without min/max time", {
  st.1 = filter_stop_times(gtfs_routing, "2018-10-01")
  st.2 = filter_stop_times(gtfs_routing, "2018-10-01", "00:00:00", 999*3600)
  expect_true(all(st.1 == st.2))
})

test_that("routing with missing NA", {
  gtfs_routing2 = read_gtfs(system.file("extdata", "routing-NA-times.zip", package = "tidytransit"))
  fst1 = filter_stop_times(gtfs_routing, "2018-10-01", 7*3600, 24*3600)
  fst2 = filter_stop_times(gtfs_routing2, "2018-10-01", 7*3600, 24*3600)

  tts1a = raptor.(gtfs_routing$stop_times, gtfs_routing$transfers, "stop1b")
  tts1b = raptor.(fst1, attributes(fst1)$transfers, "stop1b")
  tts2 = raptor.(fst2, attributes(fst2)$transfers, "stop1b")

  expect_equal(tts1a, tts2)
  expect_equal(tts1b, tts2)
})

test_that("raptor considers each stop_id as a separate starting journey", {
  x1 = bind_rows(
    raptor(stop_times, transfers, "stop1a", keep = "all"),
    raptor(stop_times, transfers, "stop4", keep = "all")) %>% 
    arrange(from_stop_id, to_stop_id, travel_time, journey_departure_time, journey_arrival_time, transfers)
  
  x2 = raptor(stop_times, transfers, c("stop1a", "stop4"), keep = "all") %>% 
    arrange(from_stop_id, to_stop_id, travel_time, journey_departure_time, journey_arrival_time, transfers)
  
  expect_identical(x1, x2)
  
  y1 = bind_rows(
    raptor(stop_times, transfers, "stop1a", keep = "all"),
    raptor(stop_times, transfers, "stop1b", keep = "all")) %>% 
    arrange(from_stop_id, to_stop_id, travel_time, journey_departure_time, journey_arrival_time, transfers)
  
  y2 = raptor(stop_times, transfers, c("stop1a", "stop1b"), keep = "all") %>% 
    arrange(from_stop_id, to_stop_id, travel_time, journey_departure_time, journey_arrival_time, transfers)
  
  expect_identical(y1, y2)
})

test_that("initial transfers among from_stop_ids exist", {
  possible_routes = read.csv(test_path("possible_routes.csv"), sep = ";") %>% 
    filter(connection %in% c("yes", "via_sibling", "to_sibling"))

  all_stops = raptor.(stop_times, transfers, unique(stop_times$stop_id), keep = "all")
  
  stop_pairs = all_stops %>% 
    dplyr::as_tibble() %>% 
    dplyr::distinct(from_stop_id, to_stop_id) %>% 
    dplyr::mutate(raptor_route = TRUE) %>% 
    dplyr::full_join(possible_routes, c("from_stop_id", "to_stop_id"))

  expect_identical(sort(unique(stop_pairs$connection)), c("to_sibling", "via_sibling", "yes"))
  expect_true(!anyNA(stop_pairs$raptor_route))
})

test_that("separate_starts=TRUE", {  
  possible_routes = read.csv(test_path("possible_routes.csv"), sep = ";") %>% 
        filter(connection %in% c("yes"))
  
  separate_starts = raptor(stop_times, transfers, unique(stop_times$stop_id), 
                           keep = "all", separate_starts = TRUE)
  stop_pairs = separate_starts %>% 
    dplyr::as_tibble() %>% 
    dplyr::distinct(from_stop_id, to_stop_id) %>% 
    dplyr::mutate(raptor_route = TRUE) %>% 
    dplyr::full_join(possible_routes, c("from_stop_id", "to_stop_id"))
  
  expect_identical(unique(stop_pairs$connection), "yes")
  expect_true(!anyNA(stop_pairs$raptor_route))
})

test_that("in-seat transfers", {
  # split route B at stop 6
  stop_times_4 = bind_rows(stop_times, stop_times[7,])
  stop_times_4[c(5:7),"trip_id"] <- "routeB1"
  stop_times_4[c(8:9, 24),"trip_id"] <- "routeB2"
  stop_times_4[c(24, 8:9), "stop_sequence"] <- 1:3
  stop_times_4[c(7,24), c("arrival_time", "departure_time")] <- hms::hms(0, 20, 7)
  transfers_4 = data.frame(from_stop_id = "stop6", to_stop_id = "stop6",
                           from_trip_id = "routeB1", to_trip_id = "routeB2",
                           transfer_type = 4) %>% bind_rows(transfers)
  
  # regular
  r0 = raptor(stop_times, transfers, "stop5")
  expect_identical(
    r0$to_stop_id, c("stop5", "stop6", "stop7", "stop3a", "stop3b", "stop8a", "stop8b", "stop4"))
  
  r_no_inseat = raptor(stop_times_4, transfers, "stop5")
  expect_identical(r_no_inseat$to_stop_id, c("stop5", "stop6", "stop7", "stop8a", "stop8b", "stop4"))
  
  r_with_inseat = raptor(stop_times_4, transfers_4, "stop5")
  expect_identical(
    r_with_inseat$to_stop_id, c("stop5", "stop6", "stop7", "stop3a", "stop3b", "stop8a", "stop8b", "stop4"))
  
  # arrival
  a0 = raptor(stop_times, transfers, "stop3a", arrival = TRUE, time_range = c("07:28:00", "07:28:00"))
  a_no_inseat = raptor(stop_times_4, transfers, "stop3a", arrival = TRUE, time_range = c("07:28:00", "07:28:00"))
  a_with_inseat = raptor(stop_times_4, transfers_4, "stop3a", arrival = TRUE, time_range = c("07:28:00", "07:28:00"))

  expect_identical(a0$from_stop_id, c("stop3a", "stop6", "stop5", "stop1a", "stop1b"))
  expect_identical(a_no_inseat$from_stop_id, c("stop3a", "stop6"))
  expect_identical(a_with_inseat$from_stop_id, c("stop3a", "stop6", "stop5", "stop1a", "stop1b"))
  
  # with stop_id change
  st_4 = stop_times_4
  st_4[c(5:7),"trip_id"] <- "routeB1"
  st_4[c(8:9, 24),"trip_id"] <- "routeB2"
  st_4[c(24, 8:9), "stop_sequence"] <- 1:3
  st_4[c(7,24), c("arrival_time", "departure_time")] <- hms::hms(0, 20, 7)
  st_4[24,"stop_id"] <- "stop6x"
  tr_4 = transfers_4
  tr_4[1,"to_stop_id"] <- "stop6x"

  # note that stop6x is not actually "visited" and is missing from the returned table
  r_with_inseat2 = raptor(st_4, tr_4, "stop5")
  expect_identical(r_with_inseat2, r_with_inseat)
  
  # transfer type 1
  tr_1 = tr_4[,c("from_stop_id", "to_stop_id", "transfer_type", "min_transfer_time")]
  tr_1$transfer_type[1] <- 1L
  r_with_inseat3 = raptor(st_4, tr_1, "stop5")
  expect_identical(r_with_inseat3, r_with_inseat)
})

test_that("pickup_type=1", {
  st = stop_times
  st$pickup_type <- 0L
  st[st$stop_id == "stop2", "pickup_type"] <- c(1L, 0L)
  r1 = raptor(stop_times, transfers, "stop2") %>% 
    filter(to_stop_id == "stop3a")
  expect_identical(nrow(r1), 2L)
  r2 = raptor(st, transfers, "stop2") %>% 
    filter(to_stop_id == "stop3a")
  expect_identical(r2$journey_departure_time, (7*3600 + 10*60))
})

test_that("drop_off_type=1", {
  st = stop_times
  st$drop_off_type <- 0L
  st[st$stop_id == "stop3b", "drop_off_type"] <- c(1L)
  r1 = raptor(stop_times, transfers, "stop1b")
  r2 = raptor(st, transfers, "stop1b")
  missing_journeys = anti_join(r1, r2, c("from_stop_id", "to_stop_id", "journey_departure_time"))
  expect_identical(missing_journeys$to_stop_id, c("stop3b", "stop3b", "stop3a", "stop3a"))
  expect_identical(missing_journeys$journey_arrival_time, 7*3600+c(18*60,23*60,18*60+10,23*60+10))
})

rm("gtfs_routing", "local_gtfs_path", "raptor.", "stop_times", "transfers",
   "stop_times_0710", "stop_times_0711", "stop_times_0715", "test_from_stop_ids")
