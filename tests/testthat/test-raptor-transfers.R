test_that("in-seat transfers", {
  gtfs_routing = read_gtfs(system.file("extdata", "routing.zip", package = "tidytransit"))
  stop_times = gtfs_routing$stop_times
  transfers = gtfs_routing$transfers
  
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
  expect_identical(r_with_inseat$transfers[8], 0L)
  
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
  
  # transfer type 1 with trips
  tr_1 = tr_4
  tr_1$transfer_type[1] <- 1L
  r_with_inseat3 = raptor(st_4, tr_1, "stop5")
  expect_identical(r_with_inseat3[,1:5], r_with_inseat[,1:5])
  expect_identical(r_with_inseat3$transfers, c(0L, 0L, rep(1L, 6)))
  
  # without trips, transfer to stop6x is possible
  tr_1 <- tr_1[,c("from_stop_id", "to_stop_id", "transfer_type", "min_transfer_time")]
  tr_1$transfer_type[1] <- 1L
  r_with_inseat4 = raptor(st_4, tr_1, "stop5")
  expect_identical(nrow(r_with_inseat4[to_stop_id == "stop6x"]), 1L)
  expect_identical(r_with_inseat4[to_stop_id != "stop6x"], r_with_inseat3)
  
  # transfer type 0 
  tr_0 = transfers
  tr_0$transfer_type <- 0L
  tr_0$min_transfer_time <- NULL
  tr0 = transfers
  tr0$min_transfer_time <- 0L
  expect_identical(raptor(stop_times, tr_0, "stop5"),
                   raptor(stop_times, tr0, "stop5"))
})

test_that("trip pair transfers", {
  transfers <- read.csv(text = "from_stop_id,to_stop_id,from_trip_id,to_trip_id,transfer_type,min_transfer_time
B1,B2,,,2,10
B1,B1,AB,BC,4,
B1,B3,X,Y,1,", na.strings = "")
  
  stop_times <- read.csv(text = "trip_id,stop_id,arrival_time,departure_time
AB,A,,10
AB,B1,20,
BC,B1,,20
BC,C,50,
BD,B2,,30
BD,D,40,
X,A,,60
X,B1,80,
Y,B3,,80
Y,E,90,", na.strings = "")
  
  # C is only reachable with in-seat transfer on B1
  # D is reachable with walk transfer B1->B2
  # E is only reachable with walk transfer from trip X
  rptr = raptor(stop_times, transfers, "A")
  expect_identical(rptr$transfers, c(0L, 0L, 0L, 0L, 1L, 0L, 1L, 0L))
  expect_identical(rptr$to_stop_id, c("A", "B1", "B2", "B1", "D", "B2", "E", "C"))
  shortest = raptor(stop_times, transfers, "A", keep = "shortest")
  expect_identical(shortest$transfers, c(0L, 0L, 0L,  1L, 1L, 0L))
  expect_identical(shortest$to_stop_id, c("A", "B1", "B2", "D", "E", "C"))
  
  # C with actual transfer for type 5
  tr5 = transfers
  tr5[2, "transfer_type"] <- 5L
  rptr = raptor(stop_times, tr5, "A")
  expect_identical(rptr$transfers[8], 1L)
  expect_identical(rptr$to_stop_id[8], "C")
})
