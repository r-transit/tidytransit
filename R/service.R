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