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
B1,B2,,,2,7
B1,B1,AB,BC,4,
B1,B3,X,Y1,1", na.strings = "")
  
  stop_times <- read.csv(text = "trip_id,stop_id,arrival_time,departure_time
AB,A,,10
AB,B1,20,
BC,B1,,20
BC,C,50,
BD,B2,,30
BD,D,40,
X,A,,60
X,B1,80,
Y1,B3,,80
Y1,E,90,
Y2,B3,170
Y2,E,180", na.strings = "")
  
  # C is only reachable with in-seat transfer on B1
  # D is reachable with walk transfer B1->B2
  # E is only reachable with walk transfer from trip X
  rptr = raptor(stop_times, transfers, "A") %>% arrange(to_stop_id)
  expect_identical(rptr$transfers, c(0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L))
  expect_identical(rptr$to_stop_id, c("A", "B1", "B1", "B2", "B2", "C", "D", "E"))
  shortest = raptor(stop_times, transfers, "A", keep = "shortest")
  expect_identical(shortest$transfers, c(0L, 0L, 0L,  1L, 1L, 0L))
  expect_identical(shortest$to_stop_id, c("A", "B1", "B2", "D", "E", "C"))
  
  # C with actual transfer for type 5
  tr5 = transfers
  tr5[2, "transfer_type"] <- 5L
  rptr = raptor(stop_times, tr5, "A")
  expect_identical(rptr$transfers[8], 1L)
  expect_identical(rptr$to_stop_id[8], "C")
  
  # suppress trip transfer to E, add walk transfer
  e0 = filter(raptor(stop_times, transfers, "A"), to_stop_id == "E")
  expect_identical(e0$journey_arrival_time, 90)
  tr3 = transfers
  tr3[3, "transfer_type"] <- 3L
  e1 = filter(raptor(stop_times, tr3, "A"), to_stop_id == "E")
  expect_identical(nrow(e1), 0L)
  
  tr3 <- rbind(tr3, list("B1","B3",NA,NA,2L,100L))
  e3 = filter(raptor(stop_times, tr3, "A"), to_stop_id == "E")
  expect_identical(e3$travel_time, 170)
  expect_identical(e3$transfers, 1L)
})

test_that("error for from_route_id without trip_ids", {
  transfers <- read.csv(text = "from_stop_id,to_stop_id,from_trip_id,to_trip_id,from_route_id,to_route_id,transfer_type,
B1,B2,trip1,trip2,route1,route2,1
C1,C2,,,route1,route2,1", na.strings = "")

  expect_no_warning(setup_transfers(transfers[1,], TRUE))
  
  tr1 = expect_warning(setup_transfers(transfers, FALSE),
                       "transfers.txt contains unsupported route-to-route transfers (will be ignored)",
                       fixed = TRUE)
  expect_identical(nrow(tr1), 1L)
  transfers[2,"from_route_id"] <- NA
  tr2 = expect_warning(setup_transfers(transfers, FALSE),
                       "transfers.txt contains unsupported route-to-route transfers (will be ignored)",
                       fixed = TRUE)
  expect_identical(tr2, tr1)
  
  transfers[2,c(3,6)] <- list("trip_ID", NA)
  tr3 = expect_warning(setup_transfers(transfers, FALSE),
                 "from_trip_id-to_trip_id pairs with one NA value found in transfers (will be ignored)",
                 fixed = TRUE)
  expect_identical(tr3, tr1)
})
