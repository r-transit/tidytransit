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
#' @param separate_starts If `FALSE` (default), returns all initial transfers among the 
#'   specified `stop_ids`. If `TRUE` each stop_id is calculated independently. This  
#'   can lead to faster computation times and is useful when the resulting times between 
#'   `from` and `to_stop_ids` will be aggregated later (e.g. by `stop_name` 
#'   in [travel_times()]).
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
                  keep = "all",
                  separate_starts = FALSE) {
  from_stop_id <- to_stop_id <- . <- raptor_departure_stop <- NULL
  raptor_arrival_time <- raptor_departure_time <- journey_time <- transfer_offset <- NULL
  raptor_min_departure_time <- raptor_max_departure_time <- NULL
  journey_arrival_time <- journey_departure_time <- travel_time <- NULL
  if(inherits(stop_times, "tidygtfs")) {
    stop("Travel times cannot be calculated with a tidygtfs object, see filter_stop_times()", call. = FALSE)
  }

  # filter_stop_times stores transfers in attributes
  if(missing(transfers)) {
    if(!is.null(attributes(stop_times)$transfers)) {
      transfers <- attributes(stop_times)$transfers
    } else {
      stop('argument "transfers" is missing, with no default', call. = FALSE)
    }
  }

  # 1) check and params ####
  # use data.table for faster manipulation
  # copy necessary as we change/rename columns by reference
  time_window = setup_time_window(time_range, arrival, stop_times)
  keep <- setup_keep(keep)
  max_transfers <- setup_max_transfers(max_transfers)
  separate_starts <- setup_separate_starts(separate_starts)
  stop_times_dt = setup_stop_times(stop_times, arrival, time_window)
  transfers_dt = setup_transfers(transfers, arrival)
  
  stop_ids <- setup_stop_ids(stop_ids, stop_times_dt, transfers_dt)
  if(is.data.table(stop_ids)) return(stop_ids)
  
  # 2) set up when/where journeys begin ####
  journeys_init = find_journeys(stop_ids, transfers_dt, time_window, arrival, max_transfers, separate_starts)

  # 3) run raptor ####
  rptr = raptor_core(journeys_init, stop_times_dt, transfers_dt, max_transfers)

  # 4) combine initial journeys with raptor result
  result_cns = c("from_stop_id", "to_stop_id", "travel_time", "journey_departure_time", "journey_arrival_time", "transfers")
  if(separate_starts) {
    raptor_result = rptr[, from_stop_id := raptor_departure_stop]
  } else {
    raptor_result = merge(journeys_init, rptr, by = "raptor_departure_stop", allow.cartesian = TRUE)
    raptor_result <- raptor_result[raptor_departure_time >= raptor_min_departure_time & 
                                     raptor_departure_time <= raptor_max_departure_time,]
  }
  raptor_result[, `:=`(journey_departure_time = raptor_departure_time,
                       journey_arrival_time = raptor_arrival_time)]
  raptor_result[, `:=`(travel_time = raptor_arrival_time - journey_departure_time)]
  raptor_result <- raptor_result[, result_cns, with = FALSE]

  journeys_init[, `:=`(to_stop_id = raptor_departure_stop, 
                       journey_departure_time = journey_time,
                       journey_arrival_time = journey_time,
                       travel_time = transfer_offset, transfers = 0L)]
  
  raptor_result <- rbindlist(list(raptor_result, 
                                  journeys_init[,colnames(raptor_result), with = FALSE]))
  
  # revert times and switch from<->to
  if(arrival) {
    setnames(raptor_result, c("from_stop_id", "to_stop_id"), c("to_stop_id", "from_stop_id"))
    raptor_result[, `:=`(journey_departure_time = -journey_departure_time, journey_arrival_time = -journey_arrival_time)]
    raptor_result[, c("journey_arrival_time", "journey_departure_time") := .(journey_departure_time, journey_arrival_time)]
  }

  # 5) optimize, only keep the "best" journeys ####
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
  } else {
      setorder(raptor_result, travel_time, journey_arrival_time)
  }

  # combine journeys from origin stops (different departures), using sorting from before
  raptor_result <- rbindlist(list(
    raptor_result[from_stop_id == to_stop_id][, .SD[1], by = c("from_stop_id", "to_stop_id")][,colnames(raptor_result), with = FALSE],
    raptor_result[from_stop_id != to_stop_id,]))

  # 6) return result table ####
  raptor_result <- raptor_result[, result_cns, with=FALSE]
  return(raptor_result)
}

find_journeys = function(from_stop_ids, transfers_dt, time_window, arrival, max_transfers, separate_starts) {
  raptor_max_departure_time <- raptor_min_departure_time <- raptor_departure_stop <- NULL
  transfer_type <- min_transfer_time <- transfer_offset <- journey_time <- NULL

  if(arrival) time_window <- -time_window[2:1]
  
  # from_stops
  initial_stops = data.table(from_stop_id = from_stop_ids,
                             raptor_departure_stop = from_stop_ids,
                             raptor_min_departure_time = time_window[1],
                             raptor_max_departure_time = time_window[2],
                             transfer_offset = 0)
  
  # stops reachable via transfer from from_stops
  initial_transfers = initial_stops[FALSE,]
  if(max_transfers > 0 && nrow(transfers_dt) > 0) {
    walk_transfers = transfers_dt[transfer_type %in% c(0, 2),]
    if(nrow(walk_transfers) > 0) {
      cns = setdiff(colnames(initial_stops), "raptor_departure_stop")
      initial_transfers = merge(
        initial_stops[,cns, with = FALSE], walk_transfers,
        by.x = "from_stop_id", by.y = "trnsfrs_from_stop_id", allow.cartesian = TRUE)
      setnames(initial_transfers, "trnsfrs_to_stop_id", "raptor_departure_stop")
      initial_transfers[, raptor_min_departure_time := raptor_min_departure_time + min_transfer_time]
      initial_transfers[, transfer_offset := min_transfer_time]
      initial_transfers <- initial_transfers[
        raptor_min_departure_time >= time_window[1] & raptor_max_departure_time <= time_window[2] &
          raptor_min_departure_time <= time_window[2] & raptor_max_departure_time >= time_window[1],]
    }
  }

  if(separate_starts) {
    initial_transfers <- initial_transfers[!raptor_departure_stop %in% from_stop_ids,]
  }

  # combine
  journeys_init = rbindlist(list(initial_stops, initial_transfers[,colnames(initial_stops), with = FALSE]))

  if(!arrival) {
    journeys_init[, `:=`(journey_time = raptor_min_departure_time-transfer_offset)]
  } else {
    journeys_init[, `:=`(journey_time = raptor_max_departure_time-transfer_offset)]
  }

  return(journeys_init)
}

raptor_core = function(journeys_init, 
                       stop_times_dt, transfers_dt,
                       max_transfers) {
  stopifnot(!is.null(journeys_init),
            !is.null(transfers_dt), !is.null(stop_times_dt))
  raptor_min_departure_time <- raptor_max_departure_time <- exact_departure <- marked <- travel_time <- NULL
  departure_time_num <- arrival_time_num <- marked_departure_time_num <- from_stop_id <- to_stop_id <- NULL
  raptor_departure_time <- raptor_arrival_time <- arrival_trip_id <- transfers <- transfer_type <- min_transfer_time <- NULL

  # from_stop_id: input value, rptr_dep...: actual stop_id after initial transfer
  rptr_colnames = c("to_stop_id", "marked", "exact_departure", "raptor_arrival_time", "arrival_trip_id",
                    "raptor_departure_time", "raptor_departure_stop", "transfers")

  # find initial departures
  rptr = merge(stop_times_dt,
               journeys_init[, c("raptor_departure_stop", "raptor_min_departure_time", "raptor_max_departure_time")],
               by.x = "to_stop_id", by.y = "raptor_departure_stop", allow.cartesian = TRUE)
  rptr <- rptr[departure_time_num >= raptor_min_departure_time & departure_time_num <= raptor_max_departure_time,]

  rptr[, `:=`(raptor_departure_time = departure_time_num,
              raptor_arrival_time = departure_time_num, 
              raptor_departure_stop = to_stop_id)]
  rptr[,`:=`(marked = TRUE, transfers = 0L, arrival_trip_id = NA, exact_departure = TRUE)]
  
  rptr <- rptr[, .SD[1], by = c("raptor_departure_stop", "raptor_arrival_time")]
  rptr <- rptr[, rptr_colnames, with = FALSE]
  rptr <- unique(rptr)
  
  # transfers
  walk_transfers = data.table()
  direct_transfers = data.table()
  if(nrow(transfers_dt) > 0) {
    stopifnot("transfer_type" %in% colnames(transfers_dt))
    walk_transfers <- transfers_dt[transfer_type %in% c(0L, 2L),]
    direct_transfers <- transfers_dt[transfer_type %in% c(1L, 4L, 5L),]
  }
  rm(transfers_dt)

  # raptor loop
  k = 0L
  while(any(rptr$marked)) {
    # select marked stops
    rptr_marked <- rptr[marked == TRUE]
    # unmark stops
    rptr[,marked := FALSE]
    
    # mark departures for in-seat transfers
    if(nrow(direct_transfers) > 0) {
      direct_transf_marked = merge(rptr_marked, direct_transfers, 
                                   by.x = c("to_stop_id", "arrival_trip_id"), 
                                   by.y = c("trnsfrs_from_stop_id", "from_trip_id"))
      direct_transf_marked[, `:=`(to_stop_id = NULL, exact_departure = TRUE)]
      setnames(direct_transf_marked, "trnsfrs_to_stop_id", "to_stop_id")
      direct_transf_marked <- direct_transf_marked[, rptr_colnames, with = FALSE]
      
      rptr_marked <- rbindlist(list(rptr_marked, direct_transf_marked))
    }
    
    # Get departures that happen on marked stops
    departures_marked = stop_times_dt[rptr_marked, on = "to_stop_id", allow.cartesian = TRUE]
    
    # use trips/departures that happen after the stop's arrival_time 
    # or only the exact departure if it's predefined (journey start or in-seat transfer)
    departures_marked <- departures_marked[(!exact_departure & departure_time_num > raptor_arrival_time) |
                                             (exact_departure & departure_time_num == raptor_arrival_time),]

    setnames(departures_marked, "departure_time_num", "marked_departure_time_num")
    departures_marked <- departures_marked[, c("trip_id", "marked_departure_time_num", "raptor_departure_stop", "raptor_departure_time")]
    
    # only use unique trip_ids (for each departure stop/time)
    departures_marked <- departures_marked[, .SD[1], by = c("trip_id", "raptor_departure_stop", "raptor_departure_time")]

    # all arrival_times in the marked trips are possibly better than the
    # current journey_arrival_times in rptr (thus candidates)
    arrival_candidates = stop_times_dt[departures_marked, on = "trip_id", allow.cartesian = TRUE]
    arrival_candidates[, transfers := k]
    
    # keep arrival_times that actually happen after the trip has
    # left its marked stop (departure_time > marked_departure_time)
    arrival_candidates <- arrival_candidates[departure_time_num > marked_departure_time_num]
    setkey(arrival_candidates, to_stop_id)

    # leave loop if there are no candidates left
    if(nrow(arrival_candidates) == 0) { break }

    # arrival candidates are marked for the next loop (if they are actually better)
    arrival_candidates[,`:=`(marked = TRUE, exact_departure = FALSE,
                             raptor_arrival_time = arrival_time_num,
                             arrival_trip_id = trip_id)]
    arrival_candidates <- arrival_candidates[, rptr_colnames, with = FALSE]

    # Find all transfers for arrival candidates
    if(k <= max_transfers && nrow(walk_transfers) > 0) {
      transfer_candidates = merge(
        arrival_candidates,
        walk_transfers,
        by.x = "to_stop_id",
        by.y = "trnsfrs_from_stop_id",
        allow.cartesian = TRUE
      )
      transfer_candidates[,to_stop_id := NULL]
      transfer_candidates[,marked := TRUE]
      setnames(transfer_candidates, old = "trnsfrs_to_stop_id", new = "to_stop_id")

      # arrival_time needs to be calculated
      transfer_candidates[,raptor_arrival_time := (raptor_arrival_time + min_transfer_time)]
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
    setorder(rptr, raptor_arrival_time)
    rptr <- rptr[, .SD[1], by = c("to_stop_id", "raptor_departure_stop", "raptor_departure_time")]
    rptr <- rptr[, rptr_colnames, with = FALSE]

    # iteration is finished, the remaining marked stops have been improved
    k <- k+1L
    if(k > max_transfers) { break }
  }

  rptr[,`:=`(exact_departure = NULL, marked = NULL, arrival_trip_id = NULL)]

  return(rptr)
}

setup_stop_times = function(stop_times, arrival, time_window) {
  arrival_time_num <- departure_time_num <- . <- NULL
  
  stop_times_dt = as.data.table(replace_NA_times(stop_times))
  set_num_times(stop_times_dt)
  setnames(x = stop_times_dt, new = "to_stop_id", old = "stop_id")
  if(arrival) {
    stop_times_dt[,c("arrival_time_num", "departure_time_num") := .(-departure_time_num, -arrival_time_num)]
  }
  if(is.null(key(stop_times_dt)) || "trip_id" != key(stop_times_dt)) {
    setkeyv(stop_times_dt, "trip_id") # faster than key on stop_id
  }
  if(is.null(indices(stop_times_dt)) || !("stop_id" %in% indices(stop_times_dt))) {
    setindex(stop_times_dt, "to_stop_id")
  }
  
  # trim times
  if(arrival) {
    stop_times_dt <- stop_times_dt[arrival_time_num <= time_window[2],]
  } else {
    stop_times_dt <- stop_times_dt[departure_time_num >= time_window[1],]
  }
  
  return(stop_times_dt)
}

setup_transfers = function(transfers, arrival) {
  transfer_type <- min_transfer_time <- from_trip_id <- to_trip_id <- . <- NULL
  
  transfers_dt = as.data.table(transfers)
  transfers_dt <- transfers_dt[transfer_type != 3,]

  if(nrow(transfers_dt) == 0) {
    # only check nrow for transfers in raptor
    return(transfers_dt)
  }
  if(!"trnsfrs_from_stop_id" %in% colnames(transfers_dt)) {
    setnames(x = transfers_dt, new = "trnsfrs_from_stop_id", old = "from_stop_id")
  }
  if(!"trnsfrs_to_stop_id" %in% colnames(transfers_dt)) {
    setnames(x = transfers_dt, new = "trnsfrs_to_stop_id", old = "to_stop_id")
  }

  # checks for inseat or direct transfers
  if(any(transfers_dt[["transfer_type"]] %in% c(4, 5))) {
    stopifnot(all(c("from_trip_id", "to_trip_id") %in% colnames(transfers_dt)))
    .check = transfers_dt[transfer_type %in% c(4, 5) & (is.na(from_trip_id) | is.na(to_trip_id))]
  }
  
  # flip arrivals
  if(arrival) {
    setnames(transfers_dt,
             old = c("trnsfrs_to_stop_id", "trnsfrs_from_stop_id"),
             new = c("trnsfrs_from_stop_id", "trnsfrs_to_stop_id"))
    if(any(transfers_dt$transfer_type == 4)) {
      transfers_dt[transfer_type == 4, c("from_trip_id", "to_trip_id") := .(to_trip_id, from_trip_id)]
    }
  }
  
  setkey(transfers_dt, "trnsfrs_from_stop_id")
  return(transfers_dt)
}

setup_time_window = function(time_range, arrival, stop_times) {
  if(length(time_range) == 1) {
    if(!is.numeric(time_range)) {
      stop("time_range is not numeric", call. = FALSE)
    }
    if(time_range < 1) {
      stop("time_range is less than 1", call. = FALSE)
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
    stop("Cannot handle time_range with length != 1 or 2", call. = FALSE)
  }

  return(time_window)
}

setup_stop_ids = function(stop_ids, stop_times_dt, transfers_dt) {
  if(!is.character(stop_ids)) {
    stop("stop_ids must be a character vector")
  }
  stop_ids <- unique(stop_ids)
  nonexistent_stop_ids = setdiff(
    stop_ids, c(stop_times_dt$to_stop_id, transfers_dt$trnsfrs_from_stop_id, transfers_dt$trnsfrs_to_stop_id))
  
  if(length(nonexistent_stop_ids) > 0) {
    stop_ids <- setdiff(stop_ids, nonexistent_stop_ids)
    if(length(stop_ids) == 0) {
      warning("Stop not found in stop_times or transfers: ", paste(nonexistent_stop_ids, collapse = ", "), call. = FALSE)
      empty_dt = data.table(from_stop_id = character(0), to_stop_id = character(0), travel_time = numeric(0),
                            journey_departure_time = numeric(0), journey_arrival_time = character(0), transfers = integer(0))
      return(empty_dt)
    }
  }
  return(stop_ids)
}

setup_max_transfers = function(max_transfers) {
  if(is.null(max_transfers)) {
    max_transfers <- 999999L
  } else if(!is.numeric(max_transfers) || max_transfers < 0) {
    stop("max_transfers must be a number >= 0", call. = FALSE)
  }
  return(as.integer(max_transfers))
}

setup_keep = function(keep) {
  if(is.null(keep) || !(keep %in% c("shortest", "earliest", "all", "latest"))) {
    stop("`", keep, "` is not a supported optimization type, use one of: all, shortest, earliest, latest", call. = FALSE)
  }
  return(keep)
}

setup_separate_starts = function(separate_starts) {
  stopifnot(is.logical(separate_starts) && length(separate_starts) == 1 && !is.na(separate_starts))
  return(separate_starts)
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
  return(invisible(stop_times_dt))
}
