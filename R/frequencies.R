#' Get Stop Frequency
#' @param gtfs_obj a list of gtfs dataframes as read by read_gtfs().
#' @param start_hour (optional) an integer indicating the start hour (default 7)
#' @param end_hour (optional) an integer indicating the end hour (default 20)
#' @param service_id (optional) a string from the calendar_df dataframe identifying a particular service schedule.
#' @param dow (optional) integer vector indicating which days of week to calculate for. default is weekday, e.g. c(1,1,1,1,1,0,0)
#' @param by_route default TRUE, if FALSE then calculate headway for any line coming through the stop in the same direction on the same schedule. 
#' @param wide (optional) if true, then return a wide rather than tidy data frame
#' @export
#' @return a gtfs_obj with a dataframe of stops with a "Trips" variable representing the count trips taken through each stop for a route within a given time frame
#' @importFrom dplyr %>%
#' @importFrom rlang .data !! := quo enquo
#' @importFrom stats median sd
#' @importFrom tidyr spread
#' @examples 
#' data(gtfs_obj)
#' gtfs_obj <- get_stop_frequency(gtfs_obj)
#' x <- order(gtfs_obj$stops_frequency_df$headway)
#' head(gtfs_obj$stops_frequency_df[x,])

get_stop_frequency <- function(gtfs_obj,
                            start_hour=6,
                            end_hour=22,
                            service_id = "",
                            dow=c(1,1,1,1,1,0,0),
                            by_route=TRUE,
                            wide=FALSE) {
  trips <- gtfs_obj$trips_df 
  stop_times <- gtfs_obj$stop_times_df
  calendar <- gtfs_obj$calendar_df

  stop_times <- filter_stop_times_by_hour(stop_times, 
                                          start_hour, 
                                          end_hour)

  if(service_id == ""){
    service_ids <- service_by_dow(calendar,dow)
  }
  else {
    service_ids <- calendar[calendar$service_id==service_id,]$service_id
  }
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
      dplyr::summarise(departures = dplyr::n())
  } 
  else if(by_route==TRUE) {
  stop_time_trips <- stop_time_trips %>%
    dplyr::group_by(.data$route_id,
                    .data$direction_id,
                    .data$stop_id,
                    .data$service_id) %>%
    most_frequent_service() %>%
      dplyr::summarise(departures = dplyr::n())
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
  stops_frequency_df <- stop_time_trips %>% 
    tibble::as_tibble()
  
  gtfs_obj$stops_frequency_df <- stops_frequency_df
  
  return(gtfs_obj)
}

#' Get Route Frequency
#' 
#' should take: 
#' @param gtfs_obj a list of gtfs dataframes as read by the trread package.
#' @param start_hour (optional) an integer, default 6 (6 am)
#' @param end_hour (optional) an integer, default 22 (10 pm)
#' @param quiet default FALSE. whether to echo process messages
#' @param service_id (optional) a string from the calendar_df dataframe identifying a particular service schedule.
#' @param dow (optional) an integeger vector with days of week. monday=1. default: c(1,1,1,1,1,0,0)
#' @return a gtfs_obj with a dataframe of routes with variables for headway/frequency for a route within a given time frame
#' @export
#' @examples 
#' data(gtfs_obj)
#' gtfs_obj <- get_route_frequency(gtfs_obj)
#' x <- order(gtfs_obj$routes_frequency_df$median_headways)
#' head(gtfs_obj$routes_frequency_df[x,])

get_route_frequency <- function(gtfs_obj,
                            start_hour=6,
                            end_hour=22,
                            quiet = FALSE,
                            service_id = "",
                            dow=c(1,1,1,1,1,0,0)) {
  if(!quiet) message('Calculating route and stop headways.')
  gtfs_obj <- get_stop_frequency(gtfs_obj,start_hour,end_hour,service_id,dow)  
  
  if (dim(gtfs_obj$stops_frequency_df)[[1]]!=0) {
    gtfs_obj$routes_frequency_df <- gtfs_obj$stops_frequency_df %>%
      dplyr::group_by_('route_id') %>%
      dplyr::summarise(median_headways = as.integer(round(median(.data$headway),0)),
                       mean_headways = as.integer(round(mean(.data$headway),0)),
                       st_dev_headways = round(sd(.data$headway),2),
                       stop_count = dplyr::n())
  } else
  {
    warning("failed to calculate frequency--try passing a service_id from calendar_df")
  }
  return(gtfs_obj)
}

