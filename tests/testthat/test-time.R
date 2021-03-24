context("Time manipulation")

create_empty_gtfs_obj <- function() {
  g <- list(agency = data.frame())
  gtfsio::new_gtfs(g)
}

test_that("set_hms_times() works with valid data", {
  gtest <- create_empty_gtfs_obj()
  gtest$stop_times <- data.table::data.table(
    arrival_time = c("08:00:00", "14:00:00", "26:10:00"),
    departure_time = c("08:00:10", "14:00:20", "26:10:30"))
  gtest$frequencies = data.table::data.table(
    start_time = c("06:00:00"),
    end_time = c("12:00:00")
  )

  gtest <- set_hms_times(gtest)
  
  expect_is(gtest$stop_times$arrival_time, "hms")
  expect_is(gtest$stop_times$departure_time, "hms")
  expect_false(is.na(gtest$stop_times$arrival_time[3]))
  expect_equal(gtest$stop_times$departure_time[3], 
               hms::hms(26 * 3600 + 10 * 60 + 30))
  
  expect_is(gtest$frequencies$start_time, "hms")
  expect_is(gtest$frequencies$end_time, "hms")
})

test_that("set_date_service_table() uses the right dates", {
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
    start_date = lubridate::ymd("20180101"), # monday
    end_date = lubridate::ymd("20180131")) # wednesday

  set_date_service_table(gtest)
  
  date_service <- set_date_service_table(gtest)$.$date_service_table
  
  expect_true(lubridate::ymd("20180101") %in% date_service$date)
  expect_false(lubridate::ymd("20180102") %in% date_service$date)
  expect_true(lubridate::ymd("20180131") %in% date_service$date)
})

test_that("set_date_service_table() works with additions and exceptions", { 
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
    start_date = c(lubridate::ymd("20180201"), 
                   lubridate::ymd("20180401")),
    end_date = c(lubridate::ymd("20180430"), 
                 lubridate::ymd("20180430")))
  gtest$calendar_dates <- dplyr::tibble(
    service_id = c("wdays", "wend"),
    date = c(lubridate::ymd("20180314"), lubridate::ymd("20180226")),
    exception_type = c(2, 1)
  )
  
  date_service <- set_date_service_table(gtest)$.$date_service_table
  
  # exception
  mar14 <- date_service[
    date_service$date == lubridate::ymd("20180613"),]
  expect_equal(nrow(mar14), 0)
  
  # addition
  feb26 <- date_service[
    date_service$date == lubridate::ymd("20180226"),] # monday
  expect_equal(nrow(feb26), 2)
  
  # overlaps
  apr05 <- date_service[
    date_service$date == lubridate::ymd("20180405"), ]
  expect_equal(apr05 %>% dplyr::group_by(date) %>% 
                 dplyr::count() %>% 
                 dplyr::pull(n), 1)
  apr06 <- date_service[
    date_service$date == lubridate::ymd("20180406"), ] # friday
  expect_equal(apr06 %>% 
               dplyr::group_by(date) %>% 
               dplyr::count() %>% 
               dplyr::pull(n), 2)
  
  range <- date_service %>% 
    dplyr::group_by(service_id) %>% 
    dplyr::summarise(min = min(date), max = max(date))
  expect_equal(range[
    range$service_id == "wdays", "min"], 
    dplyr::tibble(min = lubridate::ymd("20180201")))
  expect_equal(
    range[range$service_id == "wend", "max"], 
    dplyr::tibble(max = lubridate::ymd("20180429")))
})

