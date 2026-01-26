# raptor()
assert_routable_stop_times = function(stop_times, warn_on_demand = TRUE) {
  if(!all(c("arrival_time", "departure_time") %in% colnames(stop_times))) {
    stop("`stop_times` must have `arrival_time` and `departure_time` columns for routing", call. = FALSE)
  }
  if(sum(is.na(stop_times[["arrival_time"]]) & is.na(stop_times[["departure_time"]])) == nrow(stop_times)) {
    if(has_on_demand(stop_times)) {
      stop("Feed contains on-demand services which are not supported by tidytransit routing", call. = FALSE)
    } else {
      stop("No arrival and departure times found in `stop_times`", call. = FALSE)
    }
  }
  if(has_on_demand(stop_times) && warn_on_demand) {
    warning("Feed contains on-demand services which are not supported by tidytransit routing", call. = FALSE)
  }
  invisible(TRUE)
}

has_on_demand = function(stop_times) {
  any(c("start_pickup_drop_off_window", "end_pickup_drop_off_window") %in% colnames(stop_times))
}

# filter_stop_times()
assert_routable_feed = function(g) {
  assert_routable_stop_times(g$stop_times, FALSE)
  
  # missing dates
  if(is.null(g[["."]][["dates_services"]])) {
    stop("No valid dates defined in feed", call. = FALSE)
  }
  
  # stops
  if(is.null(g[["stops"]][["stop_id"]]) || is.null(g[["stop_times"]][["stop_id"]])) {
    stop("`stops` and `stop_times` must have a `stop_id` column", call. = FALSE)
  }
  
  invisible(TRUE)
}

# travel_times()
catch_deprecated_max_departure_time = function(...) {
  dots = list(...)
  if("max_departure_time" %in% names(dots)) {
    stop("The parameter `max_departure_time` is deprecated, use `time_range`", call. = FALSE)
  }
}

check_stop_dists = function(stops, stop_dist_check) {
  .time_prev = Sys.time()
  stop_dists = stop_group_distances(stops, "stop_name", max_only = TRUE)
  .time_post = Sys.time()
  
  if(max(stop_dists$dist_max) > stop_dist_check) {
    stop("Some stops with the same name are more than ", stop_dist_check, " meters apart, see ?stop_group_distances().\n",
         "Using travel_times() might lead to unexpected results. Set stop_dist_check=FALSE to ignore this error.",
         call. = FALSE)
  }
  
  .time_check = as.numeric(difftime(.time_post, .time_prev, units = "secs"))
  if(.time_check > 1) {
    message("Stop distance check took longer than 1 second (", round(.time_check, 1), # nocov
            "s). Set stop_dist_check=FALSE to skip it.") # nocov
  }
}
