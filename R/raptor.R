#' Calculate travel times from one stop to all reachable stops
#' 
#' `raptor` finds the minimal travel time, earliest or latest arrival time for all 
#' stops in `stop_times` with journeys departing from `stop_ids` within 
#' `time_range`.
#' 
#' With a modified [Round-Based Public Transit Routing Algorithm](https://www.microsoft.com/en-us/research/publication/round-based-public-transit-routing) 
#' (RAPTOR) using data.table, earliest arrival times for all stops are calculated. If two 
#' journeys arrive at the same time, the one with the later departure time and thus shorter 
#' travel time is kept. By default, all journeys departing within `time_range` that arrive 
#' at a stop are returned in a table. If you want all journeys _arriving_ at stop_ids within
#' the specified time range, set `arrival` to TRUE.
#' 
#' Journeys are defined by a "from" and "to" stop_id, a departure, arrival and travel time. 
#' Note that the exact journeys (with each intermediate stop and route ids for example) is 
#' _not_ returned.
#' 
#' For most cases, `stop_times` needs to be filtered, as it should only contain trips 
#' happening on a single day and departures later than a given journey start time, see 
#' [filter_stop_times()]. The algorithm scans all trips until it exceeds `max_transfers` 
#' or all trips in `stop_times` have been visited.
#' 
#' @param stop_times A (prepared) stop_times table from a gtfs feed. Prepared means
#'                   that all stop time rows before the desired journey departure time
#'                   should be removed. The table should also only include departures 
#'                   happening on one day. Use [filter_stop_times()] for easier preparation. 
#' @param transfers Transfers table from a gtfs feed. In general no preparation is needed.
#' @param stop_ids Character vector with stop_ids from where journeys should start (or end)
#' @param arrival If FALSE (default), all journeys _start_ from `stop_ids`. If
#'                TRUE, all journeys _end_ at `stop_ids`.
#' @param time_range Departure or arrival time range in seconds. All departures from the 
#'                   first departure of stop_times (not necessarily from stop_id in 
#'                   `stop_ids`) within `time_range` are considered. If `arrival` is TRUE, 
#'                   all arrivals within `time_range` before the latest arrival time of 
#'                   stop_times are considered.
#' @param max_transfers Maximum number of transfers allowed, no limit (NULL) as default.
#' @param keep One of c("all", "shortest", "earliest", "latest"). By default, `all` journeys 
#'             arriving at a stop are returned. With `shortest` the journey with shortest 
#'             travel time is returned. With `earliest` the journey arriving at a stop the 
#'             earliest is returned, `latest` works accordingly.
#'
#' @return A data.table with journeys (departure, arrival and travel time) to/from all 
#'         stop_ids reachable by `stop_ids`.
#'
#' @seealso [travel_times()] for an easier access to travel time calculations via stop_names.
#' 
#' @import data.table
#' @export
#' @examples \donttest{
#' nyc_path <- system.file("extdata", "google_transit_nyc_subway.zip", package = "tidytransit")
#' nyc <- read_gtfs(nyc_path)
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
#' rptr <- raptor(stop_times, nyc$transfers, walk_times$stop_id, time_range = 1800,
#'                keep = "all")
#' 
#' # add walk times to travel times
#' rptr <- left_join(rptr, walk_times, by=c("from_stop_id" = "stop_id"))
#' rptr$travel_time_incl_walk <- rptr$travel_time + rptr$walk_time
#' 
#' # get minimal travel times (with walk times) for all stop_ids 
#' shortest_travel_times <- setDT(rptr)[order(travel_time_incl_walk)][, .SD[1], by = "to_stop_id"]
#' hist(shortest_travel_times$travel_time, breaks = 360)
#' }
raptor = function(stop_times,
                  transfers,
                  stop_ids,
                  arrival = FALSE,
                  time_range = 3600,
                  max_transfers = NULL,
                  keep = "all") {
  from_stop_id <- departure_time_num <- marked <- journey_departure_time <- from_stop_id <- NULL
  wait_time_to_departure <- marked_departure_time_num <- arrival_time_num <- min_transfer_time <- NULL
  to_stop_id <- travel_time <- journey_arrival_time <- trnsfrs_from_stop_id <- NULL

  if(!is.character(stop_ids) && !is.null(stop_ids)) {
    stop("stop_ids must be a character vector (or NULL)")
  }

  # check and params ####
  # stop ids need to be a character vector
  # use data.table for faster manipulation
  # copy necessary as we change/rename columns by reference
  stop_times_dt <- as.data.table(stop_times)
  stop_times_dt <- setup_stop_times(stop_times_dt, reverse = arrival)
  transfers_dt <- as.data.table(transfers)
  transfers_dt <- setup_transfers(transfers_dt)

  from_stop_ids = stop_ids
  nonexistent_stop_ids = setdiff(from_stop_ids, c(stop_times_dt$to_stop_id, 
                                                  transfers_dt$trnsfrs_from_stop_id, transfers_dt$trnsfrs_to_stop_id))
  if(length(nonexistent_stop_ids) > 0) {
    from_stop_ids <- setdiff(from_stop_ids, nonexistent_stop_ids)
    if(length(from_stop_ids) == 0) {
      warning("Stop not found in stop_times or transfers: ", paste(nonexistent_stop_ids, collapse = ", "))
      empty_dt = data.table(from_stop_id = character(0), to_stop_id = character(0), travel_time = numeric(0),
        journey_departure_time = numeric(0), journey_arrival_time = character(0), transfers = numeric(0))
      return(empty_dt)
    }
  }
  if(is.null(keep) || 
     !(keep %in% c("shortest", "earliest", "all", "latest"))) {
    stop(paste0(keep, " is not a supported optimization type, use one of: all, shortest, earliest, latest"))
  }
  if(!is.numeric(time_range)) {
    stop("time_range is not numeric. Needs to be the time range in seconds after the first departure of stop_times")
  }
  if(time_range < 1) {
    stop("time_range is less than 1")
  }
  min_departure_time = min(stop_times_dt$departure_time_num)
  max_departure_time = min_departure_time + time_range
  if(is.null(max_transfers)) {
    max_transfers <- 999999
  } else if(max_transfers < 0) {
    stop("max_transfers is less than 0")
  }
  
  # set up arrivals at from_stops ####
  
  # find stops reachable by transfer from from_stops
  transfer_stops = data.frame() # only needed for nrow below
  if(!is.null(transfers_dt) && max_transfers > 0) {
    transfer_stops <- transfers_dt[trnsfrs_from_stop_id %in% from_stop_ids]
  }

  # init_stops contains from_stops and stops reachable by transfer
  rptr_colnames = c("to_stop_id", "marked", "journey_arrival_time", "journey_departure_time", "from_stop_id", "transfers")
  
  init_stops = data.table(
    to_stop_id = c(from_stop_ids, transfer_stops$trnsfrs_to_stop_id),
    marked = F,
    journey_arrival_time = c(rep(min_departure_time, length(from_stop_ids)), 
                         min_departure_time+transfer_stops$min_transfer_time),
    journey_departure_time = rep(min_departure_time, 
                                 length(from_stop_ids)+nrow(transfer_stops)),
    from_stop_id = c(from_stop_ids, transfer_stops$trnsfrs_from_stop_id),
    transfers = c(rep(0, length(from_stop_ids)), rep(1, nrow(transfer_stops)))
  )

  # mark all departures from init_stops ####
  init_departures = stop_times_dt[init_stops, on = "to_stop_id"]
  init_departures[, journey_departure_time := departure_time_num]
  init_departures <- init_departures[!is.na(journey_departure_time)]
  init_departures[, journey_arrival_time := journey_departure_time ]
  init_departures[, from_stop_id := to_stop_id]
  init_departures[, marked := TRUE]
  init_departures[, transfers := 0]
  init_departures <- init_departures[, rptr_colnames, with = F]

  # rptr: work data frame ####
  rptr <- rbind(init_stops, init_departures)
  rptr <- rptr[, rptr_colnames, with = F]
  rptr <- distinct(rptr)

  # raptor loop works with departure > arrival 
  rptr[, journey_arrival_time := journey_arrival_time-1]
  
  # only keep departures within time range for initial loop
  rptr <- rptr[journey_departure_time <= max_departure_time]
  
  # raptor loop ####
  k = 0
  while(any(rptr$marked)) {
    # select marked stops
    rptr_marked <- rptr[marked == TRUE]
    # unmark stops
    rptr[,marked := FALSE]
    
    # Get departures that happen on marked trips
    setkey(rptr_marked, to_stop_id) # not really useful
    departures_marked = stop_times_dt[rptr_marked, on = "to_stop_id", allow.cartesian = TRUE]
    # only use trips/departures that happen after the stops journey_arrival_time
    departures_marked <- departures_marked[departure_time_num > journey_arrival_time,]
    departures_marked[,wait_time_to_departure := departure_time_num - journey_departure_time]
    # use each departure/trip once (for the closest journey_departure_time)
    setorder(departures_marked, wait_time_to_departure)
    departures_marked <- departures_marked[, .SD[1], by=c("to_stop_id", "trip_id")]
    
    # get trips from marked departures
    setorder(departures_marked, departure_time_num)
    trips_marked <- departures_marked[, .SD[1], by=c("trip_id", "journey_departure_time")]
    trips_marked <- trips_marked[, c("trip_id", "to_stop_id", "departure_time_num", "journey_departure_time", "from_stop_id")]
    setnames(trips_marked, c("trip_id", "departure_time_num"), c("trip_id", "marked_departure_time_num"))
    setkey(trips_marked, trip_id)
    
    # all arrival_times in the marked trips are possibly better than the
    # current journey_arrival_times in rptr (thus candidates)
    arrival_candidates = stop_times_dt[trips_marked, on = "trip_id", allow.cartesian = TRUE]
    arrival_candidates[, transfers := k]
    # keep arrival_times that actually happen after the trip has
    # left its marked stop (departure_time > marked_departure_time)
    arrival_candidates <- arrival_candidates[departure_time_num > marked_departure_time_num]
    setkey(arrival_candidates, to_stop_id)
    
    # leave loop if there are no candidates left
    if(nrow(arrival_candidates) == 0) {	break }
    
    # arrival candidats are marked for the next loop (if they are acutally better)
    arrival_candidates[,marked := TRUE]
    # renaming and use same columns as rptr
    arrival_candidates[,journey_arrival_time := arrival_time_num]
    arrival_candidates <- arrival_candidates[, rptr_colnames, with = F]
    
    # Find all transfers for arrival candidates
    if(!is.null(transfers_dt) && (k+1) <= max_transfers) {
      transfer_candidates = merge(
        arrival_candidates,
        transfers_dt,
        by.x = "to_stop_id",
        by.y = "trnsfrs_from_stop_id",
        allow.cartesian = TRUE
      )
      transfer_candidates[, to_stop_id := NULL]
      setnames(transfer_candidates, old = "trnsfrs_to_stop_id", new = "to_stop_id")
     
      # arrival_time needs to be calculated
      transfer_candidates[,journey_arrival_time := (journey_arrival_time + min_transfer_time)]
      transfer_candidates[,transfers := k+1]
      transfer_candidates <- transfer_candidates[, rptr_colnames, with = F]
      
      arrival_candidates <- rbindlist(list(arrival_candidates, transfer_candidates), use.names = F)
    }
    
    # Bind all candidates and table with the current best times (rptr)
    rptr <- rbindlist(list(rptr, arrival_candidates), use.names = F)
    
    # Keep earliest arrival times for each stop. Sort by journey_arrival_time
    # and rank all arrivals for each stop_id and journey_departure_time.
    # Ordering and sequencing is faster than rank/frank for large tables
    # with groups
    setorder(rptr, journey_arrival_time)
    rptr <- rptr[, .SD[1], by = c("to_stop_id", "journey_departure_time")]
    rptr <- rptr[, rptr_colnames, with = F]
    # slightly faster than:
    # 	rptr[, rank := seq_len(.N), by = c("stop_id", "journey_departure_time")]
    # 	rptr <- rptr[.rank1, on = "rank"]
    
    # iteration is finished, the remaining marked stops have been improved
    k <- k+1
    if(k > max_transfers) { break }
  }

  # fix journey_arrival_times of from_stops
  rptr[to_stop_id %in% init_stops$to_stop_id, journey_arrival_time := journey_arrival_time + 1]

  # only keep one arrival (earliest) for initial stops
  rptr <- rptr[!to_stop_id %in% init_stops$to_stop_id]
  rptr <- rbind(init_stops, rptr)
  
  # calculate travel_time
  rptr[,travel_time := journey_arrival_time - journey_departure_time]

  # reverse arrival/departure times ####
  if(arrival) {
    max_time = 604800
    arrival_tmp = max_time - rptr$journey_arrival_time
    rptr[,journey_arrival_time := (max_time - journey_departure_time)]
    rptr[,journey_departure_time := arrival_tmp]
    stop_tmp = rptr$to_stop_id
    rptr[,to_stop_id := from_stop_id]
    rptr[,from_stop_id := stop_tmp]
  }
  
  # create results ####
  
  # optimize, only keep the "best" arrivals
  keep_by = ifelse(arrival, "from_stop_id", "to_stop_id")

  if(keep == "shortest") {
    setorder(rptr, travel_time, journey_arrival_time)
    rptr <- rptr[, .SD[1], by = keep_by]
  } else if(keep == "earliest") {
    setorder(rptr, journey_arrival_time, travel_time)
    rptr <- rptr[, .SD[1], by = keep_by]
  } else if(keep == "latest") {
    setorder(rptr, -journey_arrival_time, travel_time)
    rptr <- rptr[, .SD[1], by = keep_by]
  }
  
  # build result table
  rptr <- rptr[, c("from_stop_id", "to_stop_id", "travel_time", 
                   "journey_departure_time", "journey_arrival_time", "transfers")]
  return(rptr)
}

#' Calculate shortest travel times from a stop to all reachable stops
#' 
#' Function to calculate the shortest travel times from a stop (given by `stop_name`) 
#' to all other stops of a feed. `filtered_stop_times` needs to be created before with 
#' [filter_stop_times()].
#' 
#' This function allows easier access to [raptor()] by using stop names instead of ids and 
#' returning shortest travel times by default.
#' 
#' @param filtered_stop_times stop_times data.table (with transfers and stops tables as 
#'                            attributes) created with [filter_stop_times()] where the 
#'                            departure or arrival time has been set.
#' @param stop_name Stop name for which travel times should be calculated. A vector with 
#'                  multiple names is accepted.
#' @param time_range All departures within this range in seconds after the first departure 
#'                   of `filtered_stop_times` are considered for journeys. If arrival is 
#'                   TRUE, all journeys arriving within time range before the latest arrival 
#'                   of `filtered_stop_times` are considered.                   
#' @param arrival If FALSE (default), all journeys _start_ from `stop_name`. If
#'                TRUE, all journeys _end_ at `stop_name`.
#' @param max_transfers The maximimum number of transfers
#' @param max_departure_time Either set this parameter or `time_range`. Only departures 
#'                           before `max_departure_time` are used. Accepts "HH:MM:SS" or 
#'                           seconds as a numerical value. Unused if `arrival` is TRUE.
#' @param return_coords Returns stop coordinates as columms. Default is FALSE.                            
#' @param return_DT travel_times() returns a data.table if TRUE. Default is FALSE which 
#'                  returns a tibble/tbl_df.
#'                           
#' @return A table with travel times to/from all stops reachable by `stop_name` and their
#'         corresponding journey departure and arrival times.
#'
#' @export
#' @examples \donttest{
#' nyc_path <- system.file("extdata", "google_transit_nyc_subway.zip", package = "tidytransit")
#' nyc <- read_gtfs(nyc_path)
#' 
#' # Use journeys departing after 7 AM with arrival time before 9 AM on 26th June
#' stop_times <- filter_stop_times(nyc, "2018-06-26", 7*3600, 9*3600)
#' 
#' tts <- travel_times(stop_times, "34 St - Herald Sq", return_coords = TRUE)
#' tts <- tts %>% filter(travel_time <= 3600)
#' 
#' # travel time to Queensboro Plaza is 810 seconds, 13:30 minutes
#' tts %>% filter(to_stop_name == "Queensboro Plaza") %>% dplyr::pull(travel_time) %>% hms::hms()
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
                        return_DT = FALSE) {
  travel_time <- journey_arrival_time <- journey_departure_time <- NULL
  if("gtfs" %in% class(filtered_stop_times)) {
    stop("Travel times cannot be calculated on a gtfs object. Use filter_stop_times().")
  }
  if(!all(c("stops", "transfers") %in% names(attributes(filtered_stop_times)))) {
    stop("Stops and transfers not found in filtered_stop_times attributes. Use filter_stop_times() to prepare data or use raptor() for lower level access.")
  }
  if(!is.null(max_departure_time) && !arrival) {
    if(!missing(time_range)) {
      stop("time_range and max_departure_time are set. Only one of them is allowed.")
    }
    if(is.character(max_departure_time)) {
      max_departure_time <- hhmmss_to_seconds(max_departure_time)
    }
    min_departure_time = min(filtered_stop_times$departure_time_num)
    stopifnot(max_departure_time > min_departure_time)
    time_range <- max_departure_time - min_departure_time
  }
  transfers = attributes(filtered_stop_times)$transfers
  stops = attributes(filtered_stop_times)$stops
  
  # get stop_ids of names
  stop_ids = NULL
  if(!is.null(stop_name)) {
    stop_ids = stops$stop_id[which(stops$stop_name %in% stop_name)]
    if(length(stop_ids) == 0) {
      stop(paste0("Stop name '", stop_name, "' not found in stops table"))
    }
  }
  
  # raptor in travel_times ####
  rptr = raptor(filtered_stop_times,
                transfers,
                stop_ids = stop_ids,
                arrival = arrival,
                time_range = time_range,
                keep = "shortest")

  # minimal travel_time by stop_name ####
  .select_stops = function(prefix) {
    x = stops[,paste0("stop_", c("name", "id", "lon", "lat"))[1:fifelse(return_coords, 4, 2)], with=FALSE]
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
                              "to_stop_lon", "to_stop_lat")[1:fifelse(return_coords,12,8)], 
                           with = FALSE]
  
  if(!return_DT) {
    rptr_names <- tibble::as_tibble(rptr_names)
  }
  
  return(rptr_names)
}

#' Filter a `stop_times` table for a given date and timespan. 
#' 
#' @param gtfs_obj a gtfs feed
#' @param extract_date date to extract trips from in YYYY-MM-DD format
#' @param min_departure_time The earliest departure time. Can be given as "HH:MM:SS", 
#'                           hms object or numeric value in seconds.
#' @param max_arrival_time The latest arrival time. Can be given as "HH:MM:SS", 
#'                         hms object or numeric value in seconds
#' 
#' This function creates filtered `stop_times` for [travel_times()] and [raptor()]. If you
#' want to filter a feed multiple times it is faster to precalculate date_service_table with
#' [set_date_service_table()].
#' 
#' @export                
#' @examples 
#' feed_path <- system.file("extdata", "sample-feed-fixed.zip", package = "tidytransit")
#' g <- read_gtfs(feed_path)
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
    min_departure_time <- hhmmss_to_seconds(min_departure_time)
  }
  if(is.character(max_arrival_time)) {
    max_arrival_time <- hhmmss_to_seconds(max_arrival_time)
  }
  min_departure_time <- as.numeric(min_departure_time)
  max_arrival_time <- as.numeric(max_arrival_time)
  
  if(max_arrival_time <= min_departure_time) {
    stop("max_arrival_time is before min_departure_time")
  }
  
  # trips running on day
  if(!feed_contains(gtfs_obj, "date_service_table")) {
    message("Consider using set_date_service_table beforehand if you filter this feed multiple times")
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
  
  attributes(stop_times_dt)$transfers <- gtfs_obj$transfers
  
  return(stop_times_dt)
}

setup_stop_times = function(stop_times, reverse = FALSE) {
  arrival_time_num <- departure_time_num <- NULL
  stopifnot(is.data.table(stop_times))
  set_num_times(stop_times)
  setnames(x = stop_times, new = "to_stop_id", old = "stop_id")
  if(reverse) {
    max_time = 604800
    arrival_time_tmp = stop_times$arrival_time_num
    stop_times[, arrival_time_num := (max_time - departure_time_num)]
    stop_times[, departure_time_num := (max_time - arrival_time_tmp)]
  }
  if(is.null(key(stop_times)) || "trip_id" != key(stop_times)) {
    setkeyv(stop_times, "trip_id") # faster than key on stop_id
  }
  if(is.null(indices(stop_times)) || !("stop_id" %in% indices(stop_times))) {
    setindex(stop_times, "to_stop_id")
  }
  return(stop_times)
}

setup_transfers = function(transfers) {
  stopifnot(is.data.table(transfers))
  transfer_type <- min_transfer_time <- trnsfrs_from_stop_id <- trnsfrs_to_stop_id <- NULL
  if(is.null(transfers) || nrow(transfers) == 0) {
    return(NULL)
  }
  if(!"trnsfrs_from_stop_id" %in% colnames(transfers)) {
    setnames(x = transfers, new = "trnsfrs_from_stop_id", old = "from_stop_id")
  }
  if(!"trnsfrs_to_stop_id" %in% colnames(transfers)) {
    setnames(x = transfers, new = "trnsfrs_to_stop_id", old = "to_stop_id")
  }
  transfers <- transfers[transfer_type != "3"]
  transfers[is.na(min_transfer_time), min_transfer_time := 0]
  setkey(transfers, "trnsfrs_from_stop_id")
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