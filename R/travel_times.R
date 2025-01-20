#' Calculate shortest travel times from a stop to all reachable stops
#'
#' Function to calculate the shortest travel times from a stop (given by `stop_name`)
#' to all other stop_names of a feed. `filtered_stop_times` needs to be created before with
#' [filter_stop_times()] or [filter_feed_by_date()].
#'
#' This function allows easier access to [raptor()] by using stop names instead of ids and
#' returning shortest travel times by default.
#'
#' Note however that stop_name might not be a suitable identifier for a feed. It is possible
#' that multiple stops have the same name while not being related or geographically close to
#' each other. [stop_group_distances()] and [cluster_stops()] can help identify and fix
#' issues with stop_names.
#'
#' @param filtered_stop_times stop_times data.table (with transfers and stops tables as
#'                            attributes) created with [filter_stop_times()] where the
#'                            departure or arrival time has been set.
#' @param stop_name Stop name for which travel times should be calculated. A vector with
#'                  multiple names can be used.
#' @param time_range Either a range in seconds or a vector containing the minimal and maximal 
#'                   departure time (i.e. earliest and latest possible journey departure time) 
#'                   as seconds or "HH:MM:SS" character. If `arrival` is TRUE, `time_range` 
#'                   describes the time window when journeys should end at `stop_name`.
#' @param arrival If FALSE (default), all journeys _start_ from `stop_name`. If
#'                TRUE, all journeys _end_ at `stop_name`.
#' @param max_transfers The maximum number of transfers. No limit if `NULL`
#' @param max_departure_time Deprecated. Use `time_range` to set the latest
#'                           possible departure time.
#' @param return_coords Returns stop coordinates (lon/lat) as columns. Default is FALSE.
#' @param return_DT travel_times() returns a data.table if TRUE. Default is FALSE which
#'                  returns a `tibble/tbl_df`.
#' @param stop_dist_check stop_names are not structured identifiers like
#'                        stop_ids or parent_stations, so it's possible that
#'                        stops with the same name are far apart. travel_times()
#'                        errors if the distance among stop_ids with the same name is
#'                        above this threshold (in meters).
#'                        Use FALSE to turn check off. However, it is recommended to
#'                        either use [raptor()] or fix the feed (see [cluster_stops()])
#'                        in case of warnings.
#'
#' @return A table with travel times to/from all stops reachable by `stop_name` and their
#'         corresponding journey departure and arrival times.
#'
#' @importFrom data.table fifelse
#' @export
#' @examples \donttest{
#' library(dplyr)
#' 
#' # 1) Calculate travel times from two closely related stops
#' # The example dataset gtfs_duke has missing times (allowed in gtfs) which is
#' # why we run interpolate_stop_times beforehand
#' gtfs = interpolate_stop_times(gtfs_duke)
#'
#' tts1 = gtfs %>%
#'   filter_feed_by_date("2019-08-26") %>%
#'   travel_times(c("Campus Dr at Arts Annex (WB)", "Campus Dr at Arts Annex (EB)"),
#'                time_range = c("14:00:00", "15:30:00"))
#'
#' # you can use either filter_feed_by_date or filter_stop_times to prepare the feed
#' # the result is the same
#' tts2 = gtfs %>%
#'  filter_stop_times("2019-08-26", "14:00:00") %>%
#'  travel_times(c("Campus Dr at Arts Annex (WB)", "Campus Dr at Arts Annex (EB)"),
#'               time_range = 1.5*3600) # 1.5h after 14:00
#'
#' all(tts1 == tts2)
#' # It's recommended to store the filtered feed, since it can be time consuming to
#' # run it for every travel time calculation, see the next example steps
#'
#' # 2) separate filtering and travel time calculation for a more granular analysis
#' # stop_names in this feed are not restricted to an area, create clusters of stops to fix
#' nyc_path <- system.file("extdata", "nyc_subway.zip", package = "tidytransit")
#' nyc <- read_gtfs(nyc_path)
#' nyc <- cluster_stops(nyc, group_col = "stop_name", cluster_colname = "stop_name")
#' 
#' # Use journeys departing after 7 AM with arrival time before 9 AM on 26th June
#' stop_times <- filter_stop_times(nyc, "2018-06-26", 7*3600, 9*3600)
#' 
#' # Calculate travel times from "34 St - Herald Sq"
#' tts <- travel_times(stop_times, "34 St - Herald Sq", return_coords = TRUE)
#' 
#' # only keep journeys under one hour for plotting
#' tts <- tts %>% filter(travel_time <= 3600)
#' 
#' # travel time to Queensboro Plaza is 810 seconds, 13:30 minutes
#' tts %>%
#'   filter(to_stop_name == "Queensboro Plaza") %>%
#'   mutate(travel_time = hms::hms(travel_time))
#' 
#' # plot a simple map showing travel times to all reachable stops
#' # this can be expanded to isochron maps
#' library(ggplot2)
#' ggplot(tts) + geom_point(aes(x=to_stop_lon, y=to_stop_lat, color = travel_time))
#' }
travel_times = function(filtered_stop_times,
                        stop_name,
                        time_range = 3600,
                        arrival = FALSE,
                        max_transfers = NULL,
                        max_departure_time = NULL,
                        return_coords = FALSE,
                        return_DT = FALSE,
                        stop_dist_check = 300) {
  travel_time <- journey_arrival_time <- journey_departure_time <- NULL
  stop_names = stop_name; rm(stop_name)
  if(inherits(filtered_stop_times, "tidygtfs")) {
    gtfs_obj = filtered_stop_times
    if(is.null(attributes(gtfs_obj$stop_times)$extract_date)) {
      stop("Travel times cannot be calculated with an unfiltered tidygtfs object. Use filter_feed_by_date().")
    }

    filtered_stop_times <- gtfs_obj$stop_times
    transfers = gtfs_obj$transfers
    stops = stops_as_dt(gtfs_obj$stops)
  } else {
    if(!all(c("stops", "transfers") %in% names(attributes(filtered_stop_times)))) {
      stop("Stops and transfers not found in filtered_stop_times attributes. Use filter_stop_times() to prepare data or use raptor() for lower level access.")
    }
    transfers = attributes(filtered_stop_times)$transfers
    stops = attributes(filtered_stop_times)$stops
  }

  # TODO remove max_departure_time_check
  time_range <- check_max_departure_time(max_departure_time, arrival, time_range, missing(time_range), filtered_stop_times)

  # get stop_ids of names
  stop_ids = stops$stop_id[which(stops$stop_name %in% stop_names)]
  if(length(stop_ids) == 0) {
    stop("Stop name '", stop_names, "' not found in stops table")
  }

  # Check stop_name integrity
  if(length(stop_ids) > 1 && !is.null(stop_dist_check) && !isFALSE(stop_dist_check)) {
    stop_dists = stop_group_distances(stops, "stop_name")

    if(max(stop_dists$dist_max) > stop_dist_check) {
      stop("Some stops with the same name are more than ", stop_dist_check, " meters apart, see stop_group_distances().\n",
           "Using travel_times() might lead to unexpected results. Set stop_dist_check=FALSE to ignore this error.")
    }
  }

  # raptor in travel_times ####
  rptr = raptor(stop_times = filtered_stop_times,
                transfers = transfers,
                stop_ids = stop_ids,
                max_transfers = max_transfers,
                arrival = arrival,
                time_range = time_range,
                keep = "shortest")

  # minimal travel_time by stop_name ####
  .select_stops = function(prefix) {
    x = stops[,paste0("stop_", c("name", "id", "lon", "lat"))[1:data.table::fifelse(return_coords, 4, 2)], with=FALSE]
    colnames(x) <- paste0(prefix, colnames(x))
    return(x)
  }

  rptr_names = merge(.select_stops("from_"), rptr, by = "from_stop_id")
  rptr_names <- merge(.select_stops("to_"), rptr_names, by = "to_stop_id")

  # keep minimal travel time for each stop name ####
  keep_by = ifelse(arrival, "from_stop_name", "to_stop_name")
  setorder(rptr_names, travel_time)
  rptr_names <- rptr_names[, .SD[1], by = keep_by]
  rptr_names[,journey_arrival_time := hms::hms(journey_arrival_time)]
  rptr_names[,journey_departure_time := hms::hms(journey_departure_time)]

  rptr_names <- rptr_names[,c("from_stop_name", "to_stop_name",
                              "travel_time", "journey_departure_time",
                              "journey_arrival_time", "transfers",
                              "from_stop_id", "to_stop_id",
                              "from_stop_lon", "from_stop_lat",
                              "to_stop_lon", "to_stop_lat")[1:data.table::fifelse(return_coords,12,8)],
                           with = FALSE]

  if(!return_DT) {
    rptr_names <- dplyr::as_tibble(rptr_names)
  }

  return(rptr_names)
}

#' Filter a `stop_times` table for a given date and timespan.
#'
#' @param gtfs_obj gtfs feed (tidygtfs object)
#' @param extract_date date to extract trips from this day (Date or "YYYY-MM-DD" string)
#' @param min_departure_time (optional) The earliest departure time. Can be given as "HH:MM:SS",
#'                           hms object or numeric value in seconds.
#' @param max_arrival_time (optional) The latest arrival time. Can be given as "HH:MM:SS",
#'                         hms object or numeric value in seconds.
#'
#' @return Filtered `stop_times` data.table for [travel_times()] and [raptor()].
#'
#' @export
#' @examples
#' feed_path <- system.file("extdata", "routing.zip", package = "tidytransit")
#' g <- read_gtfs(feed_path)
#'
#' # filter the sample feed
#' stop_times <- filter_stop_times(g, "2018-10-01", "06:00:00", "08:00:00")
filter_stop_times = function(gtfs_obj,
                             extract_date,
                             min_departure_time,
                             max_arrival_time) {
  if(!feed_has_non_empty_table(gtfs_obj, "stop_times")) {
    stop("gtfs_obj has no stop_times")
  }
  gtfs_obj$stop_times <- replace_NA_times(gtfs_obj$stop_times)

  departure_time_num <- arrival_time_num <- NULL
  if(is.character(extract_date)) {
    extract_date <- as.Date(extract_date)
  }
  if(missing(min_departure_time)) {
    min_departure_time <- 0
  } else if(is.character(min_departure_time)) {
    min_departure_time <- hhmmss_to_seconds(min_departure_time)
  }
  if(missing(max_arrival_time)) {
    max_arrival_time <- max(gtfs_obj$stop_times$arrival_time, na.rm = TRUE)+1
  } else if(is.character(max_arrival_time)) {
    max_arrival_time <- hhmmss_to_seconds(max_arrival_time)
  }
  min_departure_time <- as.numeric(min_departure_time)
  max_arrival_time <- as.numeric(max_arrival_time)

  if(max_arrival_time <= min_departure_time) {
    stop("max_arrival_time is before min_departure_time")
  }

  # check transfers
  if(feed_contains(gtfs_obj, "transfers")) {
    transfers <- gtfs_obj[["transfers"]]
  } else {
    warning("No transfers found in feed, travel_times() or raptor() might produce unexpected results")
    transfers <- data.frame()
  }

  # trips running on day
  service_ids = filter(gtfs_obj$.$dates_services, date == extract_date)
  if(nrow(service_ids) == 0) {
    stop("No stop_times on ", extract_date)
  }
  trips = inner_join(gtfs_obj$trips, service_ids, by = "service_id")
  trips = as.data.table(unique(trips[,c("trip_id")]))

  # prepare stop_times
  stop_times_dt = as.data.table(gtfs_obj$stop_times)
  stop_times_dt <- stop_times_dt[trips, on = "trip_id"]
  set_num_times(stop_times_dt)
  stop_times_dt <- stop_times_dt[departure_time_num >= min_departure_time &
                                   arrival_time_num <= max_arrival_time,]
  setindex(stop_times_dt, "stop_id")
  if(nrow(stop_times_dt) == 0) {
    stop("No stop times between min_departure_time and max_arrival_time")
  }

  # store stops and transfers in attributes
  attributes(stop_times_dt)$stops <- stops_as_dt(gtfs_obj$stops)
  attributes(stop_times_dt)$transfers <- transfers
  attributes(stop_times_dt)$extract_date <- extract_date
  attributes(stop_times_dt)$min_departure_time <- min_departure_time
  attributes(stop_times_dt)$max_arrival_time <- max_arrival_time

  return(stop_times_dt)
}

stops_as_dt = function(gtfs_stops) {
  stops_dt = as.data.table(gtfs_stops)
  stops_dt <- stops_dt[,c("stop_id", "stop_name", "stop_lon", "stop_lat")]
  setkey(stops_dt, "stop_id")
  setindex(stops_dt, "stop_name")
  stops_dt
}

check_max_departure_time = function(max_departure_time, arrival, time_range, missing_time_range, filtered_stop_times) {
  if(!is.null(max_departure_time)) {
    warning("max_departure_time is deprecated, use time_range")
    if(!missing_time_range) {
      stop("cannot set max_departure_time and time_range")
    }
    if(arrival) {
      stop("cannot set max_departure_time and arrival=TRUE")
    }
    if(is.character(max_departure_time)) {
      max_departure_time <- hhmmss_to_seconds(max_departure_time)
    }
    min_departure_time = min(filtered_stop_times$departure_time_num)
    stopifnot(max_departure_time > min_departure_time)
    time_range <- max_departure_time - min_departure_time
  }
  return(time_range)
}
