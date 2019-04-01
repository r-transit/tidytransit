#' Get the most frequent service for a set of trips. 
#' 
#' @param trips gtfs dataframe with a service_trips count column grouped by the service_id, route, etc of interest
#' @return trips gtfs data frame within groups that are in the most frequent service window
#' @keywords internal
#' @importFrom dplyr %>%
#' @keywords internal
most_frequent_service <- function(trips) {
  trips %>%
    dplyr::top_n(1, .data$service_trips)
}

#' Filter a gtfs calendar dataframe to service ids for specific days of the week.
#' 
#' @param gtfs_object object made by join_all_gtfs_tables
#' @param dow default to "weekday" (1,1,1,1,1,0,0)
#' @return service ids that match the schedule specified
#' @keywords internal
service_by_dow <- function(calendar,
                           dow=c(1,1,1,1,1,0,0)){
  calendar <- subset(calendar, 
                        calendar$monday == dow[1] & 
                        calendar$tuesday == dow[2] & 
                        calendar$wednesday == dow[3] & 
                        calendar$thursday == dow[4] & 
                        calendar$friday == dow[5] &
                        calendar$saturday == dow[6] &
                        calendar$sunday == dow[7])
  return(calendar$service_id)
}

#' Summarise the number of trips per service
#' 
#' @param trips trips data frame
#' @return count of service by id
#' @export
#' @keywords internal
count_service_trips <- function(trips) {
  trips %>%
    dplyr::group_by(.data$service_id) %>% 
      dplyr::mutate(service_trips = dplyr::n()) %>%
        tibble::as_tibble()
}

#' Calculate servicepatterns for the gtfs_obj
#' 
#' @param gtfs_obj feed
#' @return modified gtfs_obj with added servicepatterns list and a table linking trips and patterns (trip_servicepatterns)
#' @keywords internal
#' @export
set_servicepatterns <- function(gtfs_obj) {
  if(!exists("date_service_table", gtfs_obj)) {
    gtfs_obj$date_service_table <- get_date_service_table(gtfs_obj)
  }
  
  get_servicepattern <- function(dates) {
    paste(sort(dates), collapse = "_")
  }
  
  get_servicepattern_id <- function(dates) {
    id <- openssl::sha224(get_servicepattern(dates))
    id <- paste0("p_", substr(id, 0, 9))
    return(id)
  }
  
  # find servicepattern_ids for all trips
  trip_servicepatterns <- gtfs_obj$date_service_table %>% 
    inner_join(select(gtfs_obj$trips, service_id, trip_id), by = "service_id") %>% 
    group_by(trip_id, service_id) %>%
    summarise(
      servicepattern_id = get_servicepattern_id(date)
    ) %>% ungroup()
  
  # find dates for servicepatterns
  date_servicepattern_table <- gtfs_obj$date_service_table %>% 
    left_join(trip_servicepatterns, by = "service_id") %>% 
    mutate(servicepattern_id = factor(servicepattern_id))
  
  servicepattern_ids = unique(trip_servicepatterns$servicepattern_id)

  fetch_dates <- function(x) {
    date_servicepattern_table %>% filter(servicepattern_id == x) %>% pull(date) %>% unique()  
  }
  
  servicepatterns <- lapply(servicepattern_ids, fetch_dates)
  names(servicepatterns) <- servicepattern_ids
  
  # assing to gtfs_obj
  gtfs_obj$trip_servicepatterns <- trip_servicepatterns
  gtfs_obj$servicepatterns <- servicepatterns
  
  return(gtfs_obj)
}
