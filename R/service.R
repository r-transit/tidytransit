#' Calculate service pattern ids for a GTFS feed
#' 
#' Each trip has a defined number of dates it runs on. This set of dates is called a 
#' service pattern in tidytransit. Trips with the same `servicepattern` id run on the same
#' dates. In general, `service_id` can work this way but it is not enforced by the
#' GTFS standard.
#' 
#' @param gtfs_obj gtfs feed (tidygtfs object)
#' @param id_prefix all servicepattern ids will start with this string
#' @param hash_algo hashing algorithm used by [digest::digest()]
#' @param hash_length length the hash should be cut to with `substr()`. Use `-1` if the full 
#'                    hash should be used
#' @return modified gtfs_obj with added servicepattern list and a table linking 
#'         trips and pattern (trip_servicepatterns), added to `gtfs_obj$.` sublist.
#' 
#' @importFrom dplyr group_by summarise ungroup left_join
#' @importFrom digest digest
#' @importFrom rlang .data
#' @export
set_servicepattern <- function(gtfs_obj, id_prefix = "s_", hash_algo = "md5", hash_length = 7) {
  if(!feed_has_non_empty_table(gtfs_obj, "calendar") && !feed_has_non_empty_table(gtfs_obj, "calendar_dates")) {
    warning("No dates defined in feed", call. = FALSE)
    return(gtfs_obj)
  }
  stopifnot(is.numeric(hash_length), length(hash_length) == 1)
  
  # find servicepattern_ids for all services
  servicepattern_id <- NULL # prevents CMD check note on non-visible binding
  service_patterns <- gtfs_obj$.$dates_services %>% 
    group_by(service_id) %>%
    summarise(
      servicepattern_id = digest(date, hash_algo)
    ) %>% ungroup()
  
  # trim hash
  if(hash_length > 0) {
    n_ids = length(unique(service_patterns$servicepattern_id))
    service_patterns$servicepattern_id <- substr(service_patterns$servicepattern_id, 0, hash_length)
    if(length(unique(service_patterns$servicepattern_id)) != n_ids) {
      stop("hash_length (", hash_length, ") too small", call. = FALSE)
    }
  }
  service_patterns$servicepattern_id <- paste0(id_prefix, service_patterns$servicepattern_id)
  
  # find dates for servicepatterns
  dates_servicepatterns <- gtfs_obj$.$dates_services %>% 
    left_join(service_patterns, by = "service_id") %>% 
    group_by(date, servicepattern_id) %>% 
    summarise() %>% ungroup()

  # assign to gtfs_obj
  gtfs_obj$.[["servicepatterns"]] <- service_patterns
  gtfs_obj$.[["dates_servicepatterns"]] <- dates_servicepatterns
  
  return(gtfs_obj)
}
