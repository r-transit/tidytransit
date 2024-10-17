#' Calculate travel times from one stop to all reachable stops
#'
#' `raptor` finds the minimal travel time, earliest or latest arrival time for all
#' stops in `stop_times` with journeys departing from `stop_ids` within
#' `time_range`.
#'
#' With a modified [Round-Based Public Transit Routing Algorithm](https://www.microsoft.com/en-us/research/publication/round-based-public-transit-routing/)
#' (RAPTOR) using data.table, earliest arrival times for all stops are calculated. If two
#' journeys arrive at the same time, the one with the later departure time and thus shorter
#' travel time is kept. By default, all journeys departing within `time_range` that arrive
#' at a stop are returned in a table. If you want all journeys _arriving_ at stop_ids within
#' the specified time range, set `arrival` to TRUE.
#'
#' Journeys are defined by a "from" and "to" stop_id, a departure, arrival and travel time.
#' Note that exact journeys (with each intermediate stop and route ids for example) are
#' _not_ returned.
#'
#' For most cases, `stop_times` needs to be filtered, as it should only contain trips
#' happening on a single day, see [filter_stop_times()]. The algorithm scans all trips 
#' until it exceeds `max_transfers` or all trips in `stop_times` have been visited.
#'
#' @param stop_times A (prepared) stop_times table from a gtfs feed. Prepared means
#'                   that all stop time rows before the desired journey departure time
#'                   should be removed. The table should also only include departures
#'                   happening on one day. Use [filter_stop_times()] for easier preparation.
#' @param transfers Transfers table from a gtfs feed. In general no preparation
#'                  is needed. Can be omitted if stop_times has been prepared with 
#'                  [filter_stop_times()].
#' @param stop_ids Character vector with stop_ids from where journeys should start (or end).
#'                 It is recommended to only use stop_ids that are related to each other,
#'                 like different platforms in a train station or bus stops that are
#'                 reasonably close to each other.
#' @param arrival If FALSE (default), all journeys _start_ from `stop_ids`. If
#'                TRUE, all journeys _end_ at `stop_ids`.
#' @param time_range Either a range in seconds or a vector containing the minimal and maximal 
#'                   departure time (i.e. earliest and latest possible journey departure time) 
#'                   as seconds or "HH:MM:SS" character. If `arrival` is TRUE, `time_range` 
#'                   describes the time window when journeys should end at `stop_ids`.
#' @param max_transfers Maximum number of transfers allowed, no limit (NULL) as default.
#' @param keep One of c("all", "shortest", "earliest", "latest"). By default, `all` journeys
#'             between stop_ids are returned. With `shortest` only the journey with the 
#'             shortest travel time is returned. With `earliest` the journey arriving at a 
#'             stop the earliest is returned, `latest` works accordingly.
#'
#' @return A data.table with journeys (departure, arrival and travel time) to/from all
#'         stop_ids reachable by `stop_ids`.
#'
#' @seealso [travel_times()] for an easier access to travel time calculations via stop_names.
#'
#' @import data.table
#' @export
#' @examples \donttest{
#' nyc_path <- system.file("extdata", "nyc_subway.zip", package = "tidytransit")
#' nyc <- read_gtfs(nyc_path)
#'
#' # you can use initial walk times to different stops in walking distance (arbitrary example values)
#' stop_ids_harlem_st <- c("301", "301N", "301S")
#' stop_ids_155_st <- c("A11", "A11N", "A11S", "D12", "D12N", "D12S")
#' walk_times <- data.frame(stop_id = c(stop_ids_harlem_st, stop_ids_155_st),
#'                          walk_time = c(rep(600, 3), rep(410, 6)), stringsAsFactors = FALSE)
#'
#' # Use journeys departing after 7 AM with arrival time before 11 AM on 26th of June
#' stop_times <- filter_stop_times(nyc, "2018-06-26", 7*3600, 9*3600)
#'
#' # calculate all journeys departing from Harlem St or 155 St between 7:00 and 7:30
#' rptr <- raptor(stop_times, nyc$transfers, walk_times$stop_id, time_range = 1800,
#'                keep = "all")
#'
#' # add walk times to travel times
#' rptr <- merge(rptr, walk_times, by.x = "from_stop_id", by.y = "stop_id")
#' rptr$travel_time_incl_walk <- rptr$travel_time + rptr$walk_time
#'
#' # get minimal travel times (with walk times) for all stop_ids
#' library(data.table)
#' shortest_travel_times <- setDT(rptr)[order(travel_time_incl_walk)][, .SD[1], by = "to_stop_id"]
#' hist(shortest_travel_times$travel_time, breaks = seq(0,2*60)*60)
#' }
raptor = function(stop_times,
                  transfers,
                  stop_ids,
                  arrival = FALSE,
                  time_range = 3600,
                  max_transfers = NULL,
                  keep = "all") {
  from_stop_id <- journey_departure_stop_id <- NULL
  to_stop_id <- journey_arrival_stop_id <- NULL
  journey_departure_time <- journey_arrival_time <- travel_time <- min_transfer_time <- NULL
  i.journey_departure_stop_id <- i.travel_time <- NULL
  if(inherits(stop_times, "tidygtfs")) {
    stop("Travel times cannot be calculated with a tidygtfs object")
  }

  # filter_stop_times stores transfers in attributes
  if(missing(transfers)) {
    if(!is.null(attributes(stop_times)$transfers)) {
      transfers <- attributes(stop_times)$transfers
    } else {
      stop('argument "transfers" is missing, with no default')
    }
  }

  # Prepare departure timespans
  time_window = setup_time_window(time_range, arrival, stop_times)

  # 1) check and params ####
  # stop ids need to be a character vector
  # use data.table for faster manipulation
  # copy necessary as we change/rename columns by reference
  stop_times_dt = as.data.table(replace_NA_times(stop_times))
  stop_times_dt <- setup_stop_times(stop_times_dt, arrival, time_window)
  transfers_dt = as.data.table(transfers)
  transfers_dt <- setup_transfers(transfers_dt)
  if(!is.character(stop_ids)) {
    stop("stop_ids must be a character vector")
  }
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
  if(is.null(keep) || !(keep %in% c("shortest", "earliest", "all", "latest"))) {
    stop(keep, " is not a supported optimization type, use one of: all, shortest, earliest, latest")
  }
  if(is.null(max_transfers)) {
    max_transfers <- 999999
  } else if(max_transfers < 0) {
    stop("max_transfers is less than 0")
  }

  .i = ifelse(keep == "latest", 2, 1)
  .arrival_sign = ifelse(arrival, -1, 1)
  # 2) set up when/where journeys begin ####
  initial_stops = data.table(journey_arrival_stop_id = from_stop_ids,
                             journey_arrival_time = time_window[.i]*.arrival_sign,
                             journey_departure_time = time_window[.i]*.arrival_sign,
                             journey_departure_stop_id = from_stop_ids,
                             transfers = 0, travel_time = 0)
  initial_transfers = find_initial_transfers(initial_stops, transfers_dt, max_transfers, arrival)

  # 3) run raptor ####
  rptr = raptor_core(initial_stops, initial_transfers, stop_times_dt, transfers_dt, max_transfers)

  # reverse-engineer initial transfers ####
  if(nrow(initial_transfers) > 0) {
    rptr <- rbindlist(list(rptr, initial_transfers[,colnames(rptr), with = FALSE])) # in case no departures happened on transferrable stops
    rptr_no_transfers = rptr[initial_stops, on = "journey_departure_stop_id"]
    rptr_no_transfers[,`:=`(from_stop_id = journey_departure_stop_id, to_stop_id = journey_arrival_stop_id)]

    transfer_from_id <- transfer_time <- NULL
    .initial_transfers = as.data.table(initial_transfers)[,c(4,1,6)]
    colnames(.initial_transfers) <- c("transfer_from_id", "transfer_to_id", "transfer_time")
    rptr_with_initial_transfers = .initial_transfers[rptr, on = c("transfer_to_id" = "journey_departure_stop_id"), allow.cartesian = TRUE]
    rptr_with_initial_transfers <- rptr_with_initial_transfers[!is.na(transfer_from_id)]
    rptr_with_initial_transfers[, `:=`(from_stop_id = transfer_from_id, to_stop_id = journey_arrival_stop_id,
                                       journey_departure_time = journey_departure_time-transfer_time, travel_time = travel_time+transfer_time)]

    initial_stops[,`:=`(from_stop_id = journey_departure_stop_id, to_stop_id = journey_arrival_stop_id)]
  } else {
    initial_stops[,`:=`(from_stop_id = journey_departure_stop_id, to_stop_id = journey_arrival_stop_id)]
    rptr_no_transfers = initial_stops[FALSE,]
    rptr_with_initial_transfers = rptr[,`:=`(from_stop_id = journey_departure_stop_id, to_stop_id = journey_arrival_stop_id)]
  }

  final_cns = c("from_stop_id", "to_stop_id", "travel_time", "journey_departure_time", "journey_arrival_time", "transfers")
  raptor_result = rbindlist(list(initial_stops[,final_cns, with = FALSE],
                                 rptr_no_transfers[,final_cns, with = FALSE],
                                 rptr_with_initial_transfers[,final_cns, with = FALSE]))
  raptor_result[, transfers := as.integer(transfers)]

  # revert times and switch from<->to
  if(arrival) {
    setnames(raptor_result, c("from_stop_id", "to_stop_id"), c("to_stop_id", "from_stop_id"))
    arrival_tmp = raptor_result$journey_arrival_time
    raptor_result[,journey_arrival_time := -journey_departure_time]
    raptor_result[,journey_departure_time := -arrival_tmp]
  }

  # remove journeys departing or arriving outside time window (mostly for initial transfers)
  if(!arrival) {
    raptor_result <- raptor_result[journey_departure_time >= time_window[1] & journey_departure_time <= time_window[2]]
  } else {
    raptor_result <- raptor_result[journey_arrival_time >= time_window[1] & journey_arrival_time <= time_window[2]]
  }

  # 5) optimize, only keep the "best" journeys ####
  # add dummy journeys for initial stops
  keep_by = c("from_stop_id", "to_stop_id")
  if(keep == "shortest") {
    setorder(raptor_result, travel_time, journey_arrival_time)
    raptor_result <- raptor_result[, .SD[1], by = keep_by]
  } else if(keep == "earliest") {
    setorder(raptor_result, journey_arrival_time, travel_time)
    raptor_result <- raptor_result[, .SD[1], by = keep_by]
  } else if(keep == "latest") {
    setorder(raptor_result, -journey_arrival_time, travel_time)
    raptor_result <- raptor_result[, .SD[1], by = keep_by]
  }

  # combine journey_origins to same stops (different departures)
  raptor_result <- rbindlist(list(
    raptor_result[from_stop_id != to_stop_id,],
    raptor_result[from_stop_id == to_stop_id][, .SD[1], by = c("from_stop_id", "to_stop_id")][,colnames(raptor_result), with = FALSE]))

  # 6) return result table ####
  raptor_result <- raptor_result[, final_cns, with=FALSE]
  return(raptor_result)
}

find_initial_transfers = function(initial_stops, transfers_dt, max_transfers, arrival) {
  journey_arrival_stop_id <- journey_arrival_time <- journey_departure_time <- min_transfer_time <- travel_time <- NULL
  initial_transfers = initial_stops[FALSE,]
  if(!is.null(transfers_dt) && max_transfers > 0) {
    initial_transfers = merge(
      initial_stops,
      transfers_dt,
      by.x = "journey_departure_stop_id",
      by.y = "trnsfrs_from_stop_id",
      allow.cartesian = TRUE
    )
    initial_transfers[,journey_arrival_stop_id := NULL]
    setnames(initial_transfers, "trnsfrs_to_stop_id", "journey_arrival_stop_id")
    initial_transfers[,journey_arrival_time := journey_arrival_time+min_transfer_time]
    initial_transfers[, travel_time := journey_arrival_time-journey_departure_time]
  }

  return(initial_transfers[,colnames(initial_stops), with = FALSE])
}

raptor_core = function(initial_stops, initial_transfers, stop_times_dt, transfers_dt,
                       max_transfers) {
  arrival_time_num <- departure_time_num  <- travel_time <- NULL
  journey_departure_stop_id <- journey_departure_time <- to_stop_id <- journey_arrival_time <- NULL
  marked <- marked_departure_time_num <- transfers <- min_transfer_time <- NULL

  rptr_colnames = c("to_stop_id", "marked", "journey_arrival_time", "journey_departure_time", "journey_departure_stop_id", "transfers")

  # initial departures within time range
  .init_stops = rbindlist(list(initial_stops, initial_transfers))
  departures_marked = stop_times_dt[.init_stops[,"journey_arrival_stop_id"], on = c("to_stop_id" = "journey_arrival_stop_id"), allow.cartesian = TRUE]

  # setup tables
  departures_marked[, `:=`(journey_departure_time = departure_time_num, journey_arrival_time = departure_time_num, journey_departure_stop_id = to_stop_id, marked_departure_time_num = departure_time_num, transfers = 0, marked = TRUE)]
  rptr = as.data.table(departures_marked[,rptr_colnames, with = FALSE])
  departures_marked <- departures_marked[, c("trip_id", "marked_departure_time_num", "journey_departure_stop_id", "journey_departure_time")]

  # raptor loop
  k = 0
  while(any(rptr$marked)) {
    if(k > 0) {
      # select marked stops
      rptr_marked <- rptr[marked == TRUE]
      # unmark stops
      rptr[,marked := FALSE]

      # remove initial transfers with anti join
      if(!is.null(transfers_dt)) {
        rptr_marked <- rptr_marked[!initial_transfers, on = c("journey_departure_stop_id", "to_stop_id" = "journey_arrival_stop_id")]
        rptr_marked <- rptr_marked[to_stop_id != journey_departure_stop_id,]
      }

      # Get departures that happen on marked stops
      departures_marked = stop_times_dt[rptr_marked, on = "to_stop_id", allow.cartesian = TRUE]
      # only use trips/departures that happen after the stop's journey_arrival_time
      departures_marked <- departures_marked[departure_time_num > journey_arrival_time,]
      setnames(departures_marked, "departure_time_num", "marked_departure_time_num")
      departures_marked <- departures_marked[, c("trip_id", "marked_departure_time_num", "journey_departure_stop_id", "journey_departure_time")]
    }

    trips_marked = departures_marked[, .SD[1], by = c("trip_id", "journey_departure_stop_id", "journey_departure_time")]

    # all arrival_times in the marked trips are possibly better than the
    # current journey_arrival_times in rptr (thus candidates)
    arrival_candidates = stop_times_dt[trips_marked, on = "trip_id", allow.cartesian = TRUE]
    arrival_candidates[, transfers := k]
    # keep arrival_times that actually happen after the trip has
    # left its marked stop (departure_time > marked_departure_time)
    arrival_candidates <- arrival_candidates[departure_time_num > marked_departure_time_num]
    setkey(arrival_candidates, to_stop_id)

    # leave loop if there are no candidates left
    if(nrow(arrival_candidates) == 0) { break }

    # arrival candidates are marked for the next loop (if they are actually better)
    arrival_candidates[,marked := TRUE]
    # renaming and use same columns as rptr
    arrival_candidates[, journey_arrival_time := arrival_time_num]
    arrival_candidates <- arrival_candidates[, rptr_colnames, with = FALSE]

    # Find all transfers for arrival candidates
    if(!is.null(transfers_dt) && k <= max_transfers) {
      transfer_candidates = merge(
        arrival_candidates,
        transfers_dt,
        by.x = "to_stop_id",
        by.y = "trnsfrs_from_stop_id",
        allow.cartesian = TRUE
      )
      transfer_candidates[,to_stop_id := NULL]
      transfer_candidates[,marked := TRUE]
      setnames(transfer_candidates, old = "trnsfrs_to_stop_id", new = "to_stop_id")

      # arrival_time needs to be calculated
      transfer_candidates[,journey_arrival_time := (journey_arrival_time + min_transfer_time)]
      transfer_candidates[,transfers := k]
      transfer_candidates <- transfer_candidates[, rptr_colnames, with = FALSE]

      arrival_candidates <- rbindlist(list(arrival_candidates, transfer_candidates))
    }

    # Bind all candidates and table with the current best times
    rptr <- rbindlist(list(rptr, arrival_candidates), use.names = FALSE)

    # Keep earliest arrival times for each stop. Sort by journey_arrival_time
    # and rank all arrivals for each stop_id and journey_departure_stop/time.
    # Ordering and sequencing is faster than rank/frank for large tables
    # with groups
    setorder(rptr, journey_arrival_time)
    rptr <- rptr[, .SD[1], by = c("to_stop_id", "journey_departure_stop_id", "journey_departure_time")]
    rptr <- rptr[, rptr_colnames, with = FALSE]

    # iteration is finished, the remaining marked stops have been improved
    k <- k+1
    if(k > max_transfers) { break }
  }

  # calculate travel_time
  rptr[,travel_time := journey_arrival_time - journey_departure_time]
  rptr[,marked := NULL]
  setnames(rptr, "to_stop_id", "journey_arrival_stop_id")

  return(rptr)
}

setup_stop_times = function(stop_times, arrival, time_window) {
  arrival_time_num <- departure_time_num <- NULL
  stopifnot(is.data.table(stop_times))
  set_num_times(stop_times)
  setnames(x = stop_times, new = "to_stop_id", old = "stop_id")
  if(arrival) {
    arrival_tmp = stop_times$arrival_time_num
    stop_times[,arrival_time_num := -departure_time_num]
    stop_times[,departure_time_num := -arrival_tmp]
  }
  if(is.null(key(stop_times)) || "trip_id" != key(stop_times)) {
    setkeyv(stop_times, "trip_id") # faster than key on stop_id
  }
  if(is.null(indices(stop_times)) || !("stop_id" %in% indices(stop_times))) {
    setindex(stop_times, "to_stop_id")
  }
  
  # trim times
  if(arrival) {
    stop_times <- stop_times[arrival_time_num <= time_window[2],]
  } else {
    stop_times <- stop_times[departure_time_num >= time_window[1],]
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

setup_time_window = function(time_range, arrival, stop_times) {
  if(length(time_range) == 1) {
    if(!is.numeric(time_range)) {
      stop("time_range is not numeric")
    }
    if(time_range < 1) {
      stop("time_range is less than 1")
    }
    if(!arrival) {
      if(!is.null(attributes(stop_times)$min_departure_time)) {
        min_departure_time = attributes(stop_times)$min_departure_time
      } else {
        min_departure_time = as.numeric(min(stop_times$departure_time, na.rm = TRUE))
      }
      max_departure_time = min_departure_time + time_range
      time_window = c(min_departure_time, max_departure_time)
    } else {
      if(!is.null(attributes(stop_times)$max_arrival_time)) {
        min_departure_time = -attributes(stop_times)$max_arrival_time
      } else {
        min_departure_time = -as.numeric(max(stop_times$arrival_time, na.rm = TRUE))
      }
      max_departure_time = min_departure_time + time_range
      time_window = sort(abs(c(min_departure_time, max_departure_time)))
    }
  } else if(length(time_range) == 2) {
    if(is.character(time_range)) {
      time_range <- hhmmss_to_seconds(time_range)
    }
    time_range <- as.numeric(time_range)
    time_window = sort(time_range)
  } else {
    stop("Cannot handle time_range with length != 1 or 2")
  }

  return(time_window)
}

#' @import data.table
set_num_times = function(stop_times_dt) {
  arrival_time <- arrival_time_num <- departure_time <- departure_time_num <- NULL
  stopifnot(is.data.table(stop_times_dt))
  if(all(c("arrival_time_num", "departure_time_num") %in% colnames(stop_times_dt))) {
    return(invisible(stop_times_dt))
  }
  stop_times_dt[,arrival_time_num := as.numeric(arrival_time)]
  stop_times_dt[,departure_time_num := as.numeric(departure_time)]
  invisible(stop_times_dt)
}
