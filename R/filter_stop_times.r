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
    stop("`gtfs_obj` has no `stop_times`")
  }
  # check feasibility for routing
  assert_routable_feed(gtfs_obj)
  gtfs_obj$stop_times <- replace_NA_times(gtfs_obj$stop_times)

  # check transfers
  if(feed_contains(gtfs_obj, "transfers")) {
    transfers <- gtfs_obj[["transfers"]]
  } else {
    warning("No transfers found in feed, travel_times() or raptor() might produce unexpected results")
    transfers <- data.frame()
  }

  # trim min/max times
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
    stop("`max_arrival_time` is before `min_departure_time`", call. = FALSE)
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
    stop("No stop times between `min_departure_time` and `max_arrival_time`", call. = FALSE)
  }

  # store stops and transfers in attributes
  attributes(stop_times_dt)$stops <- stops_as_dt(gtfs_obj$stops)
  attributes(stop_times_dt)$transfers <- transfers
  attributes(stop_times_dt)$extract_date <- extract_date
  attributes(stop_times_dt)$min_departure_time <- min_departure_time
  attributes(stop_times_dt)$max_arrival_time <- max_arrival_time

  return(stop_times_dt)
}