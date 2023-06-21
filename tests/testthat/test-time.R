context("Time manipulation")

create_empty_gtfs_obj <- function() {
  g <- list(agency = data.frame())
  gtfsio::new_gtfs(g)
}

test_that("convert_times_to_hms() works with valid data", {
  gtest <- create_empty_gtfs_obj()
  gtest$stop_times <- data.table::data.table(
    arrival_time = c("08:00:00", "14:00:00", "26:10:00"),
    departure_time = c("08:00:10", "14:00:20", "26:10:30"))
  gtest$frequencies = data.table::data.table(
    start_time = c("06:00:00"),
    end_time = c("12:00:00")
  )

  gtest <- convert_times_to_hms(gtest)
  
  expect_is(gtest$stop_times$arrival_time, "hms")
  expect_is(gtest$stop_times$departure_time, "hms")
  expect_false(is.na(gtest$stop_times$arrival_time[3]))
  expect_equal(gtest$stop_times$departure_time[3], 
               hms::hms(26 * 3600 + 10 * 60 + 30))
  
  expect_is(gtest$frequencies$start_time, "hms")
  expect_is(gtest$frequencies$end_time, "hms")
})

test_that("set_dates_services() uses the right dates", {
  gtest <- create_empty_gtfs_obj()
  gtest$calendar <- dplyr::tibble(
    service_id = "s1",
    monday = 1,
    tuesday = 0,
    wednesday = 1,
    thursday = 0,
    friday = 0,
    saturday = 0,
    sunday = 0,
    start_date = as.Date("2018-01-01"), # monday
    end_date = as.Date("2018-01-31")) # wednesday

  set_dates_services(gtest)
  
  date_service <- set_dates_services(gtest)$.$dates_services
  
  expect_true(as.Date("2018-01-01") %in% date_service$date)
  expect_false(as.Date("2018-01-02") %in% date_service$date)
  expect_true(as.Date("2018-01-31") %in% date_service$date)
})

test_that("set_dates_services() works with additions and exceptions", { 
  gtest <- create_empty_gtfs_obj()
  gtest$calendar <- dplyr::tibble(
    service_id = c("wdays", "wend"),
    monday = c(1, 0),
    tuesday = c(1, 0),
    wednesday = c(1, 0),
    thursday = c(1, 0),
    friday = c(1, 1),
    saturday = c(0, 1),
    sunday = c(0, 1),
    start_date = c(as.Date("2018-02-01"), 
                   as.Date("2018-04-01")),
    end_date = c(as.Date("2018-04-30"), 
                 as.Date("2018-04-30")))
  gtest$calendar_dates <- dplyr::tibble(
    service_id = c("wdays", "wend"),
    date = c(as.Date("2018-03-14"), as.Date("2018-02-26")),
    exception_type = c(2, 1)
  )
  
  date_service <- set_dates_services(gtest)$.$dates_services
  
  # exception
  mar14 <- date_service[
    date_service$date == as.Date("2018-06-13"),]
  expect_equal(nrow(mar14), 0)
  
  # addition
  feb26 <- date_service[
    date_service$date == as.Date("2018-02-26"),] # monday
  expect_equal(nrow(feb26), 2)
  
  # overlaps
  apr05 <- date_service[
    date_service$date == as.Date("2018-04-05"), ]
  expect_equal(apr05 %>% dplyr::group_by(date) %>% 
                 dplyr::count() %>% 
                 dplyr::pull(n), 1)
  apr06 <- date_service[
    date_service$date == as.Date("2018-04-06"), ] # friday
  expect_equal(apr06 %>% 
               dplyr::group_by(date) %>% 
               dplyr::count() %>% 
               dplyr::pull(n), 2)
  
  range <- date_service %>% 
    dplyr::group_by(service_id) %>% 
    dplyr::summarise(min = min(date), max = max(date))
  expect_equal(range[
    range$service_id == "wdays", "min"], 
    dplyr::tibble(min = as.Date("2018-02-01")))
  expect_equal(
    range[range$service_id == "wend", "max"], 
    dplyr::tibble(max = as.Date("2018-04-29")))
})

test_that("parse dates", {
  x = "20180429"
  y = parse_gtfsio_date(x)
  expect_is(y, "Date")
  z = date_as_gtfsio_char(y)
  expect_equal(x, z)
})

test_that("set_dates_services w/o calendar", {
  gpath = system.file("extdata", "sample-feed-calendar_dates.zip", package = "tidytransit")
  gcal = read_gtfs(gpath)
  expect_equal(gcal$.$dates_services$date, as.Date(c("2007-01-01", "2007-06-06")))
})

test_that("interpolate stop_times", {
  st_seq = dplyr::as_tibble(rbind(
    data.frame(trip_id = "A", stop_sequence = 1:4, 
               arrival_time = hms::hms(c(NA,2,NA,10)*60),
               departure_time = hms::hms(c(0,2,NA,NA)*60)
               ),
    data.frame(trip_id = "B", stop_sequence = 1:3, 
               arrival_time = hms::hms(c(10,NA,20)*60),
               departure_time = hms::hms(c(10,NA,20)*60)
    ),
    data.frame(trip_id = "C", stop_sequence = 1:3, 
               arrival_time = hms::hms(c(0,1,2)*60),
               departure_time = hms::hms(c(0,1,2)*60)
    )
  ))
  
  st_shapes = st_seq
  st_shapes$shape_dist_traveled <- c(0,2,9,10, 0,0.1,3, 0,1,2)
  seq1 = interpolate_stop_times(st_seq)
  expect_equal(nrow(st_seq), nrow(seq1))
  expect_equal(as.numeric(seq1$departure_time[c(3,6)]), 60*c(6,15))
  shapes1 = interpolate_stop_times(st_shapes)
  expect_equal(as.numeric(shapes1$departure_time[c(3,6)]), 60*c(9, 10+0.1*((20-10)/3)))
  noshapes1 = interpolate_stop_times(st_shapes, use_shape_dist = FALSE)
  expect_equal(noshapes1$arrival_time, seq1$arrival_time)
  
  .index = !is.na(st_seq$arrival_time) & !is.na(st_seq$departure_time)
  expect_true(all(as.data.frame(st_seq)[.index,] == as.data.frame(seq1)[.index,]))  
  
  gtfs_duke2 = interpolate_stop_times(gtfs_duke)
  expect_equal(class(gtfs_duke2$stop_times), class(gtfs_duke$stop_times))
  expect_false(any(is.na(gtfs_duke2$stop_times$arrival_time)))
  expect_false(any(is.na(gtfs_duke2$stop_times$departure_time)))
})

test_that("approx_NA", {
  y = c(6,NA,10)
  expect_equal(approx_NA(y), c(6,8,10))
  expect_equal(approx_NA(y, c(6,9.5,10)), c(6,9.5,10))
})
