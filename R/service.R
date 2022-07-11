#' Calculate servicepattern ids for a gtfs feed
#' 
#' Each trip has a defined number of dates it runs on. This set of dates is called a 
#' service pattern in tidytransit. Trips with the same servicepattern id run on the same
#' dates. In general, \code{service_id} can work this way but it is not enforced by the
#' GTFS standard.
#' 
#' @param gtfs_obj gtfs feed (tidygtfs object)
#' @param id_prefix all servicepattern id will start with this string
#' @param hash_algo hashing algorithm used by digest
#' @param hash_length length the hash should be cut to with substr(). Use -1 if the full hash should be used
#' @return modified gtfs_obj with added servicepattern list and a table linking 
#'         trips and pattern (trip_servicepatterns)
#' 
#' @importFrom dplyr group_by summarise ungroup left_join
#' @importFrom digest digest
#' @importFrom rlang .data
#' @export
set_servicepattern <- function(gtfs_obj, id_prefix = "s_", hash_algo = "md5", hash_length = 7) {
  get_servicepattern_id <- function(dates) {
    hash <- digest(dates, hash_algo)
    id <- paste0(id_prefix, substr(hash, 0, hash_length))
    return(id)
  }
  
  if(hash_length < 1) {
    get_servicepattern_id <- function(dates) {
      hash <- digest(dates, hash_algo)
      id <- paste0(id_prefix, hash)
      return(id)
    }
  }
  
  # find servicepattern_ids for all services
  servicepattern_id <- NULL # prevents CMD chek note on non-visible binding
  service_pattern <- gtfs_obj$.$dates_services %>% 
    group_by(service_id) %>%
    summarise(
      servicepattern_id = get_servicepattern_id(.data$date)
    ) %>% ungroup()

  # find dates for servicepattern
  dates_servicepatterns <- gtfs_obj$.$dates_services %>% 
    left_join(service_pattern, by = "service_id") %>% 
    group_by(date, servicepattern_id) %>% 
    summarise() %>% ungroup()

  # assign to gtfs_obj
  gtfs_obj$.$servicepatterns <- service_pattern
  gtfs_obj$.$dates_servicepatterns <- dates_servicepatterns
  
  return(gtfs_obj)
}
