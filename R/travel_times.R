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
#' @param arrival If `FALSE` (default), all journeys _start_ from `stop_name`. If
#'                `TRUE`, all journeys _end_ at `stop_name`.
#' @param max_transfers The maximum number of transfers. No limit if `NULL`
#' @param return_coords Returns stop coordinates (lon/lat) as columns if `TRUE`. Default is `FALSE`.
#' @param return_DT travel_times() returns a data.table if `TRUE`. Default is `FALSE` which
#'                  returns a `tibble/tbl_df`.
#' @param stop_dist_check stop_names are not structured identifiers like stop_ids or 
#'                        parent_stations, so it is possible that stops with the same name are 
#'                        far apart from each other.
#'                        travel_times() errors if the distance among stop_ids with the same 
#'                        name is above this threshold (in meters).
#'                        Use `FALSE` to turn check off. However, it is recommended to
#'                        either use [raptor()] or fix the feed (see [cluster_stops()])
#'                        in case of warnings.
#' @param ... ignored
#'
#' @return A table with travel times to/from all stops reachable by `stop_name` and their
#'         corresponding journey departure and arrival times.
#'
#' @importFrom data.table fifelse
#' @importFrom hms hms
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
                        return_coords = FALSE,
                        return_DT = FALSE,
                        stop_dist_check = 300,
                        ...) {
  travel_time <- journey_arrival_time <- journey_departure_time <- NULL
  stop_names = stop_name
  rm(stop_name)
  catch_deprecated_max_departure_time(...)
  if(inherits(filtered_stop_times, "tidygtfs")) {
    gtfs_obj = filtered_stop_times
    if(is.null(attributes(gtfs_obj$stop_times)$extract_date)) {
      stop("Travel times cannot be calculated with an unfiltered tidygtfs object. Use filter_feed_by_date().",
           call. = FALSE)
    }

    filtered_stop_times <- gtfs_obj$stop_times
    transfers = gtfs_obj$transfers
    stops = stops_as_dt(gtfs_obj$stops)
  } else {
    if(!all(c("stops", "transfers") %in% names(attributes(filtered_stop_times)))) {
      stop("Stops and transfers not found in filtered_stop_times attributes. Use filter_stop_times() to prepare data or use raptor() for lower level access.",
           call. = FALSE)
    }
    transfers = attributes(filtered_stop_times)$transfers
    stops = attributes(filtered_stop_times)$stops
  }

  # get stop_ids of names
  stop_ids = stops$stop_id[which(stops$stop_name %in% stop_names)]
  if(length(stop_ids) == 0) {
    stop("Stop name not found in stops table: ", toString(stop_names), call. = FALSE)
  }

  # Check stop_name integrity
  if(!is.null(stop_dist_check) && !isFALSE(stop_dist_check)) {
    check_stop_dists(stops, stop_dist_check)
  }

  # raptor in travel_times ####
  rptr = raptor(stop_times = filtered_stop_times,
                transfers = transfers,
                stop_ids = stop_ids,
                max_transfers = max_transfers,
                arrival = arrival,
                time_range = time_range,
                keep = "shortest",
                separate_starts = TRUE)

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
  rptr_names[,journey_arrival_time := hms(journey_arrival_time)]
  rptr_names[,journey_departure_time := hms(journey_departure_time)]

  rptr_names <- rptr_names[,c("from_stop_name", "to_stop_name",
                              "travel_time", "journey_departure_time",
                              "journey_arrival_time", "transfers",
                              "from_stop_id", "to_stop_id",
                              "from_stop_lon", "from_stop_lat",
                              "to_stop_lon", "to_stop_lat")[1:data.table::fifelse(return_coords,12,8)],
                           with = FALSE]

  if(!return_DT) {
    rptr_names <- as_tibble(rptr_names)
  }

  return(rptr_names)
}

stops_as_dt = function(gtfs_stops) {
  cns = intersect(colnames(gtfs_stops), c("stop_id", "stop_name", "stop_lon", "stop_lat"))
  stopifnot(c("stop_id") %in% colnames(gtfs_stops))
  stops_dt = as.data.table(gtfs_stops)
  stops_dt <- stops_dt[, cns, with = FALSE]
  setkey(stops_dt, "stop_id")
  if("stop_name" %in% colnames(stops_dt)) {
    setindex(stops_dt, "stop_name")
  }
  return(stops_dt)
}
