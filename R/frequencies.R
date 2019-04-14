#' Get Stop Frequency
#' 
#' Note that some GTFS feeds contain a frequency data frame already. 
#' Consider using this instead, as it will be more accurate than what 
#' tidytransit calculates. 
#' 
#' 
#' @param gtfs_obj a list of gtfs dataframes as read by read_gtfs().
#' @param start_hour (optional) an integer indicating the start hour (default 7)
#' @param end_hour (optional) an integer indicating the end hour (default 20)
#' @param service_id (optional) a string from the calendar dataframe identifying a particular service id, or a set of them
#' @param dow (optional) integer vector indicating which days of week to calculate for. default is weekday, e.g. c(1,1,1,1,1,0,0)
#' @param by_route default TRUE, if FALSE then calculate headway for any line coming through the stop in the same direction on the same schedule. 
#' @param wide (optional) if true, then return a wide rather than tidy data frame
#' @export
#' @return a gtfs_obj with a dataframe of stops (gtfs_obj$.$stops_frequency) with a "Trips" variable representing the count trips taken through each stop for a route within a given time frame
#' @importFrom dplyr %>%
#' @importFrom rlang .data !! := quo enquo
#' @importFrom stats median sd
#' @importFrom tidyr spread
#' @examples 
#' data(gtfs_obj)
#' gtfs_obj <- get_stop_frequency(gtfs_obj)
#' x <- order(gtfs_obj$stops_frequency$headway)
#' head(gtfs_obj$stops_frequency_df[x,])

get_stop_frequency <- function(gtfs_obj,
                            start_hour=6,
                            end_hour=22,
                            service_ids=c(),
                            dow=c(1, 1, 1, 1, 1, 0, 0),
                            by_route=TRUE,
                            wide=FALSE) {
  gtfs_obj <- set_hms_times(gtfs_obj)
  trips <- gtfs_obj$trips
  stop_times <- gtfs_obj$stop_times
  calendar <- gtfs_obj$calendar

  stop_times <- filter_stop_times_by_hour(stop_times,
                                          start_hour,
                                          end_hour)
  if (length(service_ids)==0) {
    service_ids <- service_by_dow(calendar, dow)
  }
  trips <- trips %>%
    dplyr::filter(.data$service_id %in% service_ids) %>%
      count_service_trips()
  stop_time_trips <- dplyr::inner_join(stop_times,
                                trips,
                                by = "trip_id")
  if (by_route == FALSE) {
    stop_time_trips <- stop_time_trips %>%
      dplyr::group_by(.data$direction_id,
                      .data$stop_id,
                      .data$service_id) %>%
      most_frequent_service() %>%
      dplyr::summarise(departures = dplyr::n())
  }
  else if (by_route == TRUE) {
  stop_time_trips <- stop_time_trips %>%
    dplyr::group_by(.data$route_id,
                    .data$direction_id,
                    .data$stop_id,
                    .data$service_id) %>%
    most_frequent_service() %>%
      dplyr::summarise(departures = dplyr::n())
  }
  # TODO we should only use seconds
  # or hms objects to avoid confusion
  t1 <- end_hour - start_hour
  minutes1 <- 60 * t1
  stop_time_trips$headway <-
    round(minutes1 / stop_time_trips$departures,
          digits = 4)
  if (wide == TRUE){
    stop_time_trips <- stop_time_trips %>%
      dplyr::select(-.data$departures, -.data$service) %>%
      tibble::rowid_to_column() %>%
      tidyr::spread(.data$direction, .data$headway, sep = "_")
  }
  stops_frequency <- stop_time_trips %>% 
    tibble::as_tibble()
  gtfs_obj$.$stops_frequency <- stops_frequency
  return(gtfs_obj)
}

#' Get Route Frequency
#' 
#' Note that some GTFS feeds contain a frequency data frame already. 
#' Consider using this instead, as it will be more accurate than what 
#' tidytransit calculates. 
#' 
#' should take: 
#' @param gtfs_obj a list of gtfs dataframes as read by the trread package.
#' @param start_hour (optional) an integer, default 6 (6 am)
#' @param end_hour (optional) an integer, default 22 (10 pm)
#' @param quiet default FALSE. whether to echo process messages
#' @param service_id (optional) a string from the calendar dataframe identifying a particular service schedule.
#' @param dow (optional) an integer vector with days of week. monday=1. default: c(1,1,1,1,1,0,0)
#' @return a gtfs_obj with a dataframe of routes with variables (gtfs_obj$.$routes_frequency) for headway/frequency for a route within a given time frame
#' @export
#' @examples 
#' data(gtfs_obj)
#' gtfs_obj <- get_route_frequency(gtfs_obj)
#' x <- order(gtfs_obj$routes_frequency$median_headways)
#' head(gtfs_obj$routes_frequency[x,])

get_route_frequency <- function(gtfs_obj,
                            start_hour=6,
                            end_hour=22,
                            quiet = FALSE,
                            service_id = c(),
                            dow=c(1, 1, 1, 1, 1, 0, 0)) {
  if (!quiet) message("Calculating route and stop headways.")
  if( !is.null(gtfs_obj$frequencies) ) {  print("NOTE: A pre-calculated frequencies dataframe exists for this feed already--consider using that.") } 
  # TODO use set_* if gtfs_obj is changed/appended?
  gtfs_obj <- get_stop_frequency(gtfs_obj, start_hour, 
                                 end_hour, service_id, dow)  
  if (dim(gtfs_obj$.$stops_frequency)[[1]]!=0) {
    gtfs_obj$.$routes_frequency <- gtfs_obj$.$stops_frequency %>%
      dplyr::group_by_("route_id") %>%
      dplyr::summarise(median_headways = 
                         as.integer(round(median(.data$headway),0)),
                       mean_headways = 
                         as.integer(round(mean(.data$headway),0)),
                       st_dev_headways = 
                         round(sd(.data$headway),2),
                       stop_count = dplyr::n())
  } else {
    warning("failed to calculate frequency--
            try passing a service_id from calendar_df")
  }
  return(gtfs_obj)
}

