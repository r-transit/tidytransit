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

#' Calculate servicepattern for the gtfs_obj
#' 
#' @param gtfs_obj gtfs feed
#' @param id_prefix all ids start with this string
#' @param hash_length length the hash should be cut to with substr(). Use -1 if the full hash should be used
#' @param hash_algo hashing algorithm used by digest
#' @return modified gtfs_obj with added servicepattern list and a table linking trips and pattern (trip_servicepatterns)
#' @keywords internal
#' @importFrom dplyr group_by summarise ungroup left_join
#' @importFrom digest digest
#' @export
set_servicepattern <- function(gtfs_obj, hash_algo = "md5", id_prefix = "s_", hash_length = 7) {
  if(!exists("date_service_table", gtfs_obj)) {
    gtfs_obj <- set_date_service_table(gtfs_obj)
  }

  get_servicepattern_id <- function(dates) {
    hash <- digest::digest(dates, hash_algo)
    id <- paste0(id_prefix, substr(hash, 0, hash_length))
    return(id)
  }
  
  if(hash_length < 1) {
    get_servicepattern_id <- function(dates) {
      hash <- digest::digest(dates, hash_algo)
      id <- paste0(id_prefix, hash)
      return(id)
    }
  }
  
  # find servicepattern_ids for all services
  service_pattern <- gtfs_obj$.$date_service_table %>% 
    group_by(service_id) %>%
    summarise(
      servicepattern_id = get_servicepattern_id(date)
    ) %>% ungroup()

  # find dates for servicepattern
  date_servicepattern_table <- gtfs_obj$.$date_service_table %>% 
    left_join(service_pattern, by = "service_id") %>% 
    group_by(date, servicepattern_id) %>% 
    summarise() %>% ungroup()

  # assing to gtfs_obj
  gtfs_obj$.$service_pattern <- service_pattern
  gtfs_obj$.$date_servicepattern_table <- date_servicepattern_table
  
  return(gtfs_obj)
}
