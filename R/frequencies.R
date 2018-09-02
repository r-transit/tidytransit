#' Get Stop Frequency
#' @param gtfs_obj a list of gtfs dataframes as read by read_gtfs().
#' @param start_hour (optional) an integer indicating the start hour (default 7)
#' @param end_hour (optional) an integer indicating the end hour (default 20)
#' @param dow (optional) integer vector indicating which days of week to calculate for. default is weekday, e.g. c(1,1,1,1,1,0,0)
#' @param by_route default TRUE, if FALSE then calculate headway for any line coming through the stop in the same direction on the same schedule. 
#' @param wide (optional) if true, then return a wide rather than tidy data frame
#' @export
#' @return a dataframe of stops with a "Trips" variable representing the count trips taken through each stop for a route within a given time frame
#' @importFrom dplyr %>%
#' @importFrom rlang .data !! := quo enquo
#' @importFrom stats median sd
#' @importFrom tidyr spread
#' @examples 
#' data(gtfs_obj)
#' stop_frequency_summary <- get_stop_frequency(gtfs_obj, by_route=FALSE)
#' x <- order(stop_frequency_summary$headway)
#' head(stop_frequency_summary[x,])

get_stop_frequency <- function(gtfs_obj,
                            start_hour=6,
                            end_hour=22,
                            dow=c(1,1,1,1,1,0,0),
                            by_route=TRUE,
                            wide=FALSE) {
  trips <- gtfs_obj$trips_df 
  stop_times <- gtfs_obj$stop_times_df
  calendar <- gtfs_obj$calendar_df

  stop_times <- filter_stop_times_by_hour(stop_times, 
                                          start_hour, 
                                          end_hour)

  service_ids <- service_by_dow(calendar,dow)
  
  trips <- trips %>% 
    dplyr::filter(.data$service_id %in% service_ids) %>%
      count_service_trips()
  
  stop_time_trips <- dplyr::inner_join(stop_times,
                                trips, 
                                by="trip_id")
  if(by_route==FALSE){
    stop_time_trips <- stop_time_trips %>%
      dplyr::group_by(.data$direction_id,
                      .data$stop_id,
                      .data$service_id) %>%
      most_frequent_service() %>%
      dplyr::summarise(departures = n())
  } 
  else if(by_route==TRUE) {
  stop_time_trips <- stop_time_trips %>%
    dplyr::group_by(.data$route_id,
                    .data$direction_id,
                    .data$stop_id,
                    .data$service_id) %>%
    most_frequent_service() %>%
      dplyr::summarise(departures = n())
  }
  t1 <- end_hour - start_hour
  minutes1 <- 60*t1
  stop_time_trips$headway <- round(minutes1/stop_time_trips$departures,digits=4)
  
  if(wide==TRUE){
    stop_time_trips <- stop_time_trips %>%
      dplyr::select(-.data$departures,-.data$service) %>%
      tibble::rowid_to_column() %>%
      tidyr::spread(.data$direction, .data$headway, sep="_")
  }
  return(stop_time_trips %>% tibble::as_tibble())
}

#' Get Route Frequency
#' 
#' should take: 
#' @param gtfs_obj a list of gtfs dataframes as read by the trread package.
#' @param start_hour (optional) an integer, default 6 (6 am)
#' @param end_hour (optional) an integer, default 22 (10 pm)
#' @param dow (optional) an integeger vector with days of week. monday=1. default: c(1,1,1,1,1,0,0)
#' @return route_headways a dataframe of route headways
#' @export
#' @examples 
#' data(gtfs_obj)
#' get_route_frequency_summary <- get_route_frequency(gtfs_obj)
#' x <- order(get_route_frequency_summary$median_headways)
#' head(get_route_frequency_summary[x,])

get_route_frequency <- function(gtfs_obj,
                            start_hour=6,
                            end_hour=22,
                            dow=c(1,1,1,1,1,0,0)) {
  stop_frequency_df <- get_stop_frequency(gtfs_obj,
                                      start_hour, 
                                      end_hour,
                                      dow)  
  
  if (dim(stop_frequency_df)[[1]]!=0) {
    get_route_frequency_df <- stop_frequency_df %>%
      dplyr::group_by_('route_id') %>%
      dplyr::summarise(median_headways = as.integer(round(median(.data$headway),0)),
                       mean_headways = as.integer(round(mean(.data$headway),0)),
                       st_dev_headways = round(sd(.data$headway),2),
                       stop_count = n())
  } else
  {
    warning("agency gtfs has no published service for the specified period")
  }
  return(get_route_frequency_df)
}

