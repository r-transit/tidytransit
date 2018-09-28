context('Time manipulation')

create_empty_gtfs_obj <- function() {
  g <- list()
  class(g) <- "gtfs"
  attributes(g)$validation_result <- data.frame()
  return(g)
}

test_that('set_hms_times() works with valid data', {
  gtest <- create_empty_gtfs_obj()
  gtest$stop_times_df <- dplyr::tibble(
    arrival_time = c("08:00:00", "14:00:00", "26:10:00"),
    departure_time = c("08:00:10", "14:00:20", "26:10:30"))
  gtest$frequencies_df = dplyr::tibble(
    start_time = c("06:00:00"),
    end_time = c("12:00:00")
  )

  gtest <- tidytransit::set_hms_times(gtest)  
  
  expect_is(gtest$stop_times_df$arrival_time_hms, "hms")
  expect_is(gtest$stop_times_df$departure_time_hms, "hms")
  expect_is(gtest$stop_times_df$arrival_time, "character")
  expect_is(gtest$stop_times_df$departure_time, "character")
  expect_false(is.na(gtest$stop_times_df$arrival_time_hms[3]))
  expect_equal(gtest$stop_times_df$departure_time_hms[3], hms::hms(26*3600+10*60+30))
  
  expect_is(gtest$frequencies_df$start_time_hms, "hms")
  expect_is(gtest$frequencies_df$end_time_hms, "hms")
  expect_is(gtest$frequencies_df$start_time, "character")
  expect_is(gtest$frequencies_df$end_time, "character")
})
