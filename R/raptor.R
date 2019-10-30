#' Calculate travel times from one stop to all reachable stops
#' 
#' `raptor` finds the minimal travel time and/or earliest arrival time for all 
#' stops in `stop_times` with journeys departing from `from_stop_ids` within 
#' `departure_time_range`.
#' 
#' With a modified [Round-Based Public Transit Routing Algorithm](https://www.microsoft.com/en-us/research/publication/round-based-public-transit-routing) 
#' (RAPTOR) using data.table earliest arrival times for all stops are calculated. If two 
#' journeys arrive at the same time, the one with the later departure time and thus shorter 
#' travel time is kept. By default, all journeys within `departure_time_range` that arrive 
#' at a stop are returned in a table. Journeys are defined by a departure stop_id, a 
#' departure, arrival and travel time. Note that the exact journeys (with each intermediate 
#' stop and route id for example) is _not_ returned.
#'
#' For most cases, `stop_times` needs to be filtered as it should only contain trips 
#' happening on a single day and departures later than a given journey start time, see 
#' [filter_stop_times()].
#' 
#' The algorithm scans all trips until it exceeds `max_transfers` or all trips
#' in `stop_times` have been visited.
#' 
#' @param stop_times A (prepared) stop_times table from a gtfs feed. Prepared means
#'                   that all stop time rows before the desired journey departure time
#'                   should be removed. The table should also only include departures 
#'                   happening on one day. Use [filter_stop_times()] for easier preparation. 
#' @param transfers Transfers table from a gtfs feed. In general no preparation is needed.
#' @param from_stop_ids Atomic char vector with stop_ids from where a journey should start
#' @param departure_time_range All departures from the first departure of 
#'                             stop_times (not necessarily a from_stop) within 
#'                             `departure_time_range` (in seconds) are considered.
#' @param max_transfers Maximum number of transfers allowed, no limit (NULL) as default.
#' @param keep One of c("all", "shortest", "earliest"). By default `all` journeys 
#'             arriving at a stop are returned. With `shortest` the 
#'             journey with shortest travel time is returned. With `earliest` the 
#'             journey arriving at a stop the earliest is returned.
#'
#' @return A data.table with travel times to all stop_ids reachable from `from_stop_ids` 
#'         and their corresponding journey departure and arrival times.
#'
#' @seealso [travel_times()] 
#' 
#' @import data.table
#' @export
#' @examples \donttest{
#' nyc_path <- system.file("extdata", "google_transit_nyc_subway.zip", package = "tidytransit")
#' nyc <- read_gtfs(nyc_path, local=TRUE)
#'
#' # you can use initial walk times to different stops in walking distance (arbitrary example values)
#' stop_ids_harlem_st <- c("301", "301N", "301S")
#' stop_ids_155_st <- c("A11", "A11N", "A11S", "D12", "D12N", "D12S")
#' walk_times <- data.frame(stop_id = c(stop_ids_harlem_st, stop_ids_155_st), 
#'                          walk_time = c(rep(600, 3), rep(410, 6)), stringsAsFactors = F)
#' 
#' # Use journeys departing after 7 AM with arrival time before 11 AM on 26th of June
#' stop_times <- filter_stop_times(nyc, "2018-06-26", 7*3600, 9*3600)
#' 
#' # calculate all journeys departing from Harlem St or 155 St between 7:00 and 7:30
#' rptr <- raptor(stop_times, nyc$transfers, walk_times$stop_id, departure_time_range = 1800,
#'                keep = "all")
#' 
#' # add walk times to travel times
#' rptr <- left_join(rptr, walk_times, by=c("journey_departure_stop_id" = "stop_id"))
#' rptr$travel_time_incl_walk <- rptr$travel_time + rptr$walk_time
#' 
#' # get minimal travel times (with walk times) for all stop_ids 
#' shortest_travel_times <- setDT(rptr)[order(travel_time_incl_walk)][, .SD[1], by = "stop_id"]
#' hist(shortest_travel_times$travel_time, breaks = 360)
#' }
raptor = function(stop_times,
                  transfers,
                  from_stop_ids,
                  departure_time_range = 3600,
                  max_transfers = NULL,
                  keep = "all") {
  from_stop_id <- departure_time_num <- marked <- journey_departure_time <- journey_departure_stop_id <- NULL
  wait_time_to_departure <- marked_departure_time_num <- arrival_time_num <- min_transfer_time <- NULL
  to_stop_id <- travel_time <- min_arrival_time <- NULL
  # stop ids need to be a character vector
  # use data.table for faster manipulation
  stop_times_dt <- setup_stop_times(stop_times)
  transfers_dt <- setup_transfers(transfers)
  if(is.data.frame(from_stop_ids)) { from_stop_ids <- from_stop_ids[[1]] }

  if(is.null(keep) || 
     !(keep %in% c("shortest", "earliest", "all"))) {
    stop(paste0(keep, " is not a supported optimization type, all times are returned"))
  }
  
  if(all(!(from_stop_ids %in% stop_times$stop_id))) {
    warning(paste0("No departures from ", paste0(from_stop_ids, collapse = ", ")))
    return(data.table(stop_id = from_stop_ids, min_arrival_time = 0, journey_departure_time = 0))
  }
  
  # setup work data frame "rptr"
  rptr_colnames = c("stop_id", "marked", "min_arrival_time", "journey_departure_time", "journey_departure_stop_id", "transfers")
  
  # get earliest departure time for from_stop_ids and set them as journey start times
  rptr = stop_times_dt[stop_id %in% from_stop_ids]
  if(!is.numeric(departure_time_range)) {
    stop("departure_time_range is not numeric. Needs to be the time range in seconds after the first departure of stop_times")
  }
  if(departure_time_range < 1) {
    stop("departure_time_range is less than 1")
  }
  max_departure_time = min(stop_times_dt$departure_time_num) + departure_time_range
  rptr <- rptr[departure_time_num <= max_departure_time]

  # setup columns
  rptr[, marked := TRUE]
  rptr[, min_arrival_time := rptr$departure_time_num - 1]
  rptr[, journey_departure_time := departure_time_num]
  rptr[, journey_departure_stop_id := stop_id]
  rptr[, transfers := 0]
  rptr <- rptr[, rptr_colnames, with = F]
  
  # raptor loop ####
  k = 0
  while(any(rptr$marked)) {
    # select marked stops
    rptr_marked <- rptr[marked == TRUE]
    # unmark stops
    rptr[,marked := FALSE]
    
    # Get departures that happen on marked trips
    setkey(rptr_marked, stop_id) # not really useful
    departures_marked = stop_times_dt[rptr_marked, on = "stop_id", allow.cartesian = TRUE]
    # only use trips/departures that happen after the stops min_arrival_time
    departures_marked <- departures_marked[departure_time_num > min_arrival_time,]
    departures_marked[,wait_time_to_departure := departure_time_num - journey_departure_time]
    # use each departure/trip once (for the closest journey_departure_time)
    setorder(departures_marked, wait_time_to_departure)
    departures_marked <- departures_marked[, .SD[1], by=c("stop_id", "trip_id")]
    # get trips from marked departures
    setorder(departures_marked, departure_time_num)
    trips_marked <- departures_marked[, .SD[1], by=c("trip_id", "journey_departure_time")]
    trips_marked <- trips_marked[, c("trip_id", "stop_id", "departure_time_num", "journey_departure_time", "journey_departure_stop_id")]
    setnames(trips_marked, c("trip_id", "departure_time_num"), c("trip_id", "marked_departure_time_num"))
    setkey(trips_marked, trip_id)
    
    # all arrival_times in the marked trips are possibly better than the
    # current min_arrival_times in rptr (thus candidates)
    arrival_candidates = stop_times_dt[trips_marked, on = "trip_id", allow.cartesian = TRUE]
    arrival_candidates[, transfers := k]
    # keep arrival_times that actually happen after the trip has
    # left its marked stop (departure_time > marked_departure_time)
    arrival_candidates <- arrival_candidates[departure_time_num > marked_departure_time_num]
    setkey(arrival_candidates, stop_id)
    
    # leave loop if there are no candidates left
    if(nrow(arrival_candidates) == 0) {	break }
    
    # arrival candidats are marked for the next loop (if they are acutally better)
    arrival_candidates[,marked := TRUE]
    # renaming and use same columns as rptr
    arrival_candidates[,min_arrival_time := arrival_time_num]
    arrival_candidates <- arrival_candidates[, rptr_colnames, with = F]
    
    # Find all transfers for arrival candidates
    if(!is.null(transfers_dt)) {
      transfer_candidates = merge(
        arrival_candidates,
        transfers_dt,
        by.x = "stop_id",
        by.y = "from_stop_id",
        allow.cartesian = TRUE
      )
      # arrival_time needs to be calculated
      transfer_candidates[,min_arrival_time := (min_arrival_time + min_transfer_time)]
      transfer_candidates[,transfers := k+1]
      transfer_candidates[,stop_id := to_stop_id]
      transfer_candidates <- transfer_candidates[, rptr_colnames, with = F]
      
      arrival_candidates <- rbindlist(list(arrival_candidates, transfer_candidates), use.names = F)
    }
    
    # Bind all candidates and table with the current best times (rptr)
    rptr <- rbindlist(list(rptr, arrival_candidates), use.names = F)
    
    # Keep earliest arrival times for each stop. Sort by min_arrival_time
    # and rank all arrivals for each stop_id and journey_departure_time.
    # Ordering and sequencing is faster than rank/frank for large tables
    # with groups
    setorder(rptr, min_arrival_time)
    rptr <- rptr[, .SD[1], by = c("stop_id", "journey_departure_time")]
    rptr <- rptr[, rptr_colnames, with = F]
    # slightly faster than:
    # 	rptr[, rank := seq_len(.N), by = c("stop_id", "journey_departure_time")]
    # 	rptr <- rptr[.rank1, on = "rank"]
   
    # iteration is finished, the remaining marked stops have been improved
    k <- k+1
    if(!is.null(max_transfers) && k > max_transfers) { break }
  }
  
  # fix min_arrival_times of from_stops
  rptr[stop_id %in% from_stop_ids, min_arrival_time := min_arrival_time + 1]
  
  # calculate travel_time
  rptr[,travel_time := min_arrival_time - journey_departure_time]
  
  # optimize, only keep the "best" arrivals
  if(keep == "shortest") {
    setorder(rptr, travel_time, min_arrival_time)
    rptr <- rptr[, .SD[1], by="stop_id"]
  } else if(keep == "earliest") {
    setorder(rptr, min_arrival_time, travel_time)
    rptr <- rptr[, .SD[1], by="stop_id"]
  }
  
  # return result table
  rptr <- rptr[,c("stop_id", "travel_time", "journey_departure_stop_id", "journey_departure_time", "min_arrival_time", "transfers")]
  return(rptr)
}

#' Calculate shortest travel times from a stop to all reachable stops
#' 
#' Function to calculate the shortest travel times from a stop (give by `from_stop_name`) 
#' to all other stops of a feed. `filtered_stop_times` needs to be created before with 
#' [filter_stop_times()].
#' 
#' This function allows easier access to [raptor()] by using stop names instead of ids and 
#' returning shortest travel times by default.
#' 
#' @param filtered_stop_times stop_times data.table (with transfers and stops tables as 
#'                            attributes) created with [filter_stop_times()] where the 
#'                            deparuture time has been set.
#' @param from_stop_name stop name from which travel times should be calculated. A vector 
#'                       with multiple names is accepted.
#' @param departure_time_range All departures within this range in seconds after the first 
#'                             departure of `filtered_stop_times` are considered for 
#'                             journeys.
#' @param max_transfers The maximimum number of transfers
#' @param max_departure_time Either set this parameter or `departure_time_range`. Only 
#'                           departures before `max_departure_time` are used. Accepts 
#'                           "HH:MM:SS" or seconds as numerical value.
#' @param return_DT travel_times() returns a data.table if TRUE. Default is FALSE which 
#'                  returns a tibble/tbl_df.
#'                           
#' @return A table with travel times to all stops reachable from `from_stop_name` and their
#'         corresponding journey departure and arrival times.
#'         
#' @export
#' @examples \donttest{
#' nyc_path <- system.file("extdata", "google_transit_nyc_subway.zip", package = "tidytransit")
#' nyc <- read_gtfs(nyc_path, local=TRUE)
#' 
#' # Use journeys departing after 7 AM with arrival time before 9 AM on 26th June
#' stop_times <- filter_stop_times(nyc, "2018-06-26", 7*3600, 9*3600)
#' 
#' tts <- travel_times(stop_times, "34 St - Herald Sq")
#' tts <- tts %>% filter(travel_time <= 3600)
#' 
#' # travel time to Queensboro Plaza is 810 seconds, 13:30 minutes
#' tts %>% filter(stop_name == "Queensboro Plaza") %>% dplyr::pull(travel_time) %>% hms::hms()
#' 
#' # plot a simple map showing travel times to all reachable stops
#' # this can be expanded to isochron maps
#' library(ggplot2)
#' ggplot(tts) + geom_point(aes(x=stop_lon, y=stop_lat, color = travel_time))
#' }
travel_times = function(filtered_stop_times,
                        from_stop_name,
                        departure_time_range = 3600,
                        max_transfers = NULL,
                        max_departure_time = NULL,
                        return_DT = FALSE) {
  travel_time <- min_arrival_time <- journey_departure_time <- NULL
  if("gtfs" %in% class(filtered_stop_times)) {
    stop("Travel times cannot be calculated on a gtfs object. Use filter_stop_times().")
  }
  if(!all(c("stops", "transfers") %in% names(attributes(filtered_stop_times)))) {
    stop("Stops and transfers not found in filtered_stop_times attributes. Use filter_stop_times() to prepare data or use raptor() for lower level access.")
  }
  if(!is.null(max_departure_time)) {
    if(departure_time_range != 3600) {
      warning("departure_time_range and max_departure_time are set. Only max_departure_time is used.")
    }
    if(is.character(max_departure_time)) {
      max_departure_time <- hhmmss_to_seconds(max_departure_time)
    }
    min_departure_time = min(filtered_stop_times$departure_time_num)
    stopifnot(max_departure_time > min_departure_time)
    departure_time_range <- max_departure_time - min_departure_time
  }
  transfers = attributes(filtered_stop_times)$transfers
  stops = attributes(filtered_stop_times)$stops
  
  # get stop_ids of names
  from_stop_ids = stops$stop_id[which(stops$stop_name %in% from_stop_name)]
  
  if(length(from_stop_ids) == 0) {
    stop(paste0("Stop name '", from_stop_name, "' not found in stops table"))
  }
  
  rptr = raptor(filtered_stop_times,
                transfers,
                from_stop_ids,
                departure_time_range = departure_time_range,
                keep = "shortest")
  
  # minimal travel_time by stop_name
  rptr_names = merge(stops, rptr, by = "stop_id")
  setorder(rptr_names, travel_time)
  rptr_names <- rptr_names[, .SD[1], by = "stop_name"]
  rptr_names[,min_arrival_time := hms::hms(min_arrival_time)]
  rptr_names[,journey_departure_time := hms::hms(journey_departure_time)]
  
  rptr_names <- rptr_names[,c("stop_name", "travel_time", "journey_departure_time",
                              "min_arrival_time", "transfers", "stop_id", "stop_lon", "stop_lat")]
  
  if(!return_DT) {
    rptr_names <- tibble::as_tibble(rptr_names)
  }
  
  return(rptr_names)
}

#' Filter a `stop_times` table for a given date and timespan. 
#' 
#' @param gtfs_obj a gtfs feed
#' @param extract_date date to extract trips from in YYYY-MM-DD format
#' @param min_departure_time The minimal departure time. Can be given as "HH:MM:SS", 
#'                           hms object or numeric value in seconds.
#' @param max_arrival_time The latest arrival time. Can be given as "HH:MM:SS", 
#'                         hms object or numeric value in seconds
#' 
#' @seealso This function creates filtered `stop_times` for [travel_times()] and [raptor()].
#'                         
#' @export                
#' @examples 
#' feed_path <- system.file("extdata", "sample-feed-fixed.zip", package = "tidytransit")
#' g <- read_gtfs(feed_path, local=TRUE)
#' 
#' # Consider precalculating date_service_table for the feed.
#' g <- set_date_service_table(g)
#' 
#' # filter the sample feed
#' stop_times <- filter_stop_times(g, "2007-01-06", "06:00:00", "08:00:00")
filter_stop_times = function(gtfs_obj,
                             extract_date,
                             min_departure_time,
                             max_arrival_time) {
  departure_time_num <- arrival_time_num <- NULL
  stopifnot(is_gtfs_obj(gtfs_obj))
  if(is.character(extract_date)) {
    extract_date <- readr::parse_date(extract_date)
  }
  if(is.character(min_departure_time)) {
    min_departure_time <- readr::parse_time(min_departure_time)
  }
  if(is.character(max_arrival_time)) {
    max_arrival_time <- readr::parse_time(max_arrival_time)
  }
  min_departure_time <- as.numeric(min_departure_time)
  max_arrival_time <- as.numeric(max_arrival_time)
  
  if(max_arrival_time <= min_departure_time) {
    stop("max_arrival_time is before min_departure_time")
  }
  
  # trips runnin on day
  if(!exists(".", where = gtfs_obj) || !exists("date_service_table", where = gtfs_obj$.)) {
    gtfs_obj <- set_date_service_table(gtfs_obj)
  }
  service_ids = filter(gtfs_obj$.$date_service_table, date == extract_date)
  if(nrow(service_ids) == 0) {
    stop(paste0("No stop_times on ", extract_date))
  }
  trip_ids = inner_join(gtfs_obj$trips, service_ids, by = "service_id")
  trip_ids <- unique(trip_ids$trip_id)
  
  # prepare stop_times
  stop_times_dt <- as.data.table(gtfs_obj$stop_times)
  setkey(stop_times_dt, trip_id)
  set_num_times(stop_times_dt)
  stop_times_dt <- stop_times_dt[trip_id %in% trip_ids,]
  stop_times_dt <- stop_times_dt[departure_time_num >= min_departure_time &
                                   arrival_time_num <= max_arrival_time,]
  setindex(stop_times_dt, "stop_id")
  if(nrow(stop_times_dt) == 0) {
    stop("No stop times between min_departure_time and max_arrival_time")
  }
  
  # store stops and transfers in attributes
  stops_dt = as.data.table(gtfs_obj$stops)
  stops_dt <- stops_dt[,c("stop_id", "stop_name", "stop_lon", "stop_lat")]
  setkey(stops_dt, "stop_id")
  setindex(stops_dt, "stop_name")
  attributes(stop_times_dt)$stops <- stops_dt
  
  attributes(stop_times_dt)$transfers <- setup_transfers(gtfs_obj$transfers)
  
  return(stop_times_dt)
}

setup_stop_times = function(stop_times) {
  if(!is.data.table(stop_times)) {
    stop_times <- as.data.table(stop_times)
  }
  set_num_times(stop_times)
  if(is.null(key(stop_times)) || "trip_id" != key(stop_times)) {
    setkeyv(stop_times, "trip_id") # faster than key on stop_id
  }
  if(is.null(indices(stop_times)) || !("stop_id" %in% indices(stop_times))) {
    setindex(stop_times, "stop_id")
  }
  return(stop_times)
}

setup_transfers = function(transfers) {
  transfer_type <- min_transfer_time <- NULL
  if(is.null(transfers) || nrow(transfers) == 0) {
    return(NULL)
  }
  if(!is.data.table(transfers)) {
    transfers <- as.data.table(transfers)
  }
  transfers <- transfers[transfer_type != "3"]
  transfers[is.na(min_transfer_time), min_transfer_time := 0]
  setkey(transfers, "from_stop_id")
  return(transfers)
}

#' @import data.table
set_num_times = function(stop_times_dt) {
  arrival_time <- arrival_time_num <- departure_time <- departure_time_num <- NULL
  stopifnot(is.data.table(stop_times_dt))
  if(all(c("arrival_time_num", "departure_time_num") %in% colnames(stop_times_dt))) {
    invisible(stop_times_dt)
  } else if(all(c("arrival_time_hms", "departure_time_hms") %in% colnames(stop_times_dt))) {
    stop_times_dt[,arrival_time_num := as.numeric(arrival_time_hms)]
    stop_times_dt[,departure_time_num := as.numeric(departure_time_hms)]
    invisible(stop_times_dt)
  } else {
    stop_times_dt[,arrival_time_num := hhmmss_to_seconds(arrival_time)]
    stop_times_dt[,departure_time_num := hhmmss_to_seconds(departure_time)]
    invisible(stop_times_dt)
  }
}
