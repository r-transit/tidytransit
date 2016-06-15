#' Function that creates gtfs meta data
#' @return Environment with gtfs data.
#' @export

get_gtfs_meta <- function() {

  # agency
  assign("agency", list())
  agency$field <- c('agency_id', 'agency_name', 'agency_url', 'agency_timezone', 'agency_lang', 'agency_phone', 'agency_fare_url', 'agency_email')
  agency$spec <- c('opt', 'req', 'req', 'req', 'opt', 'opt', 'opt', 'opt')
  agency$coltype <- rep('c', 8)

  # stops
  assign("stops", list())
  stops$field <- c('stop_id', 'stop_code', 'stop_name', 'stop_desc', 'stop_lat', 'stop_lon', 'zone_id', 'stop_url', 'location_type', 'parent_station', 'stop_timezone', 'wheelchair_boarding')
  stops$spec <- c('req', 'opt', 'req', 'opt', 'req', 'req', 'opt', 'opt', 'opt', 'opt', 'opt', 'opt')
  stops$coltype <- rep('c', length(stops$field))
  stops$coltype[which(stops$field %in% c('stop_lat', 'stop_lon'))] <- 'd' # double
  stops$coltype[which(stops$field %in% c('location_type', 'wheelchair_boarding'))] <- 'i' #integers

  # routes
  assign("routes", list())
  routes$field <- c('route_id', 'agency_id', 'route_short_name', 'route_long_name', 'route_desc', 'route_type', 'route_url', 'route_color', 'route_text_color')
  routes$spec <- c('req', 'opt', 'req', 'req', 'opt', 'req', 'opt', 'opt', 'opt')
  routes$coltype <- rep('c', length(routes$field))
  routes$coltype[routes$field %in% c('route_type')] <- 'i'

  # trips
  assign("trips", list())
  trips$field <- c('route_id', 'service_id', 'trip_id', 'trip_headsign', 'trip_short_name', 'direction_id', 'block_id', 'shape_id', 'wheelchair_accessible', 'bikes_allowed')
  trips$spec <- c('req', 'req', 'req', 'opt', 'opt', 'opt', 'opt', 'opt', 'opt', 'opt')
  trips$coltype <- rep('c', length(trips$field))
  trips$coltype[trips$field %in% c('direction_id', 'wheelchair_accessible', 'bikes_allowed')] <- 'i'

  # stop_times
  assign("stop_times", list())
  stop_times$field <- c('trip_id', 'arrival_time', 'departure_time', 'stop_id', 'stop_sequence', 'stop_headsign', 'pickup_type', 'drop_off_type', 'shape_dist_traveled', 'timepoint')
  stop_times$spec <- c('req', 'req', 'req', 'req', 'req', 'opt', 'opt', 'opt', 'opt', 'opt')
  stop_times$coltype <- rep('c', length(stop_times$field))
  stop_times$coltype[stop_times$field %in% c('stop_sequence', 'pickup_type', 'drop_off_type', 'timepoint')] <- 'i'
  stop_times$coltype[stop_times$field %in% c('shape_dist_traveled')] <- 'd'


  # calendar
  assign("calendar", list())
  calendar$field <- c('service_id', 'monday', 'tuesday', 'wednesday', 'thursday', 'friday', 'saturday', 'sunday', 'start_date', 'end_date')
  calendar$spec <- rep('req', times = 10)
  calendar$coltype <- rep('i', length(calendar$field))
  calendar$coltype[calendar$field %in% c('service_id', 'start_date', 'end_date')] <- 'c'


  # calendar_dates
  assign("calendar_dates", list())
  calendar_dates$field <- c('service_id', 'date', 'exception_type')
  calendar_dates$spec <- c('req', 'req', 'req')
  calendar_dates$coltype <- rep('c', length(calendar_dates$field))
  calendar_dates$coltype[calendar_dates$field %in% c('exception_type')] <- 'i'


  # fare_attributes
  assign("fare_attributes", list())
  fare_attributes$field <- c('fare_id', 'price', 'currency_type', 'payment_method', 'transfers', 'transfer_duration')
  fare_attributes$spec <- c('req', 'req', 'req', 'req', 'req', 'opt')
  fare_attributes$coltype <- rep('c', length(fare_attributes$field))
  fare_attributes$coltype[fare_attributes$field %in% c('payment_method', 'transfers')] <- 'i'
  fare_attributes$coltype[fare_attributes$field %in% c('transfer_duration')] <- 'd'

  # fare_rules
  assign("fare_rules", list())
  fare_rules$field <- c('fare_id', 'route_id', 'origin_id', 'destination_id', 'contains_id')
  fare_rules$spec <- c('req', 'opt', 'opt', 'opt', 'opt')
  fare_rules$coltype <- rep('c', length(fare_rules$field))
  fare_rules$coltype[fare_rules$field %in% c('direction_id', 'wheelchair_accessible', 'bikes_allowed')] <- 'i'

  # shapes
  assign("shapes", list())
  shapes$field <- c('shape_id', 'shape_pt_lat', 'shape_pt_lon', 'shape_pt_sequence', 'shape_dist_traveled')
  shapes$spec <- c('req', 'req', 'req', 'req', 'opt')
  shapes$coltype <- rep('d', length(shapes$field))
  shapes$coltype[shapes$field %in% c('shape_id')] <- 'c'
  shapes$coltype[shapes$field %in% c('shape_pt_sequence')] <- 'i'

  # frequencies
  assign("frequencies", list())
  frequencies$field <- c('trip_id', 'start_time', 'end_time', 'headway_secs', 'exact_times')
  frequencies$spec <- c('req', 'req', 'req', 'req', 'opt')
  frequencies$coltype <- rep('c', length(frequencies$field))
  frequencies$coltype[frequencies$field %in% c('headway_secs')] <- 'd'
  frequencies$coltype[frequencies$field %in% c('exact_times')] <- 'i'

  # transfers
  assign("transfers", list())
  transfers$field <- c('from_stop_id', 'to_stop_id', 'transfer_type', 'min_transfer_time')
  transfers$spec <- c('req', 'req', 'req', 'opt')
  transfers$coltype <- rep('c', length(transfers$field))
  transfers$coltype[transfers$field %in% c('exception_type')] <- 'i'

  # feed_info
  assign("feed_info", list())
  feed_info$field <- c('feed_publisher_name', 'feed_publisher_url', 'feed_lang', 'feed_start_date', 'feed_end_date', 'feed_version')
  feed_info$spec <- c('req', 'req', 'req', 'opt', 'opt', 'opt')
  feed_info$coltype <- rep('c', length(feed_info$field))
  feed_info$coltype[feed_info$field %in% c('exception_type')] <- 'i'

  # timetables
  assign("timetables", list())
  timetables$field <- c("timetable_id", "route_id", "direction_id", "start_date", "end_date", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday", "route_label", "service_notes", "direction_label", "orientation")
  timetables$spec <- rep('req', length(timetables$field))
  timetables$spec[timetables$field %in% c("route_label", "service_notes", "direction_label", "orientation")] <- 'opt'
  timetables$coltype <- rep('c', length(timetables$field))
  timetables$coltype[timetables$field %in% c("direction_id", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday")] <- 'i'

  # timetable_stop_order
  assign("timetable_stop_order", list())
  timetable_stop_order$field <- c("timetable_id", "stop_id", "stop_sequence", "stop_name", "connected_routes")
  timetable_stop_order$spec <- rep('req', length(timetable_stop_order$field))
  timetable_stop_order$spec[timetable_stop_order$field %in% c("stop_name", "connected_routes")] <- 'opt'
  timetable_stop_order$coltype <- rep('c', length(timetable_stop_order$field))
  timetable_stop_order$coltype[timetable_stop_order$field %in% c("stop_sequence")] <- 'i'

  # route_directions
  assign("route_directions", list())
  route_directions$field <- c("route_id", "direction_id", "direction_name")
  route_directions$spec <- rep('req', length(route_directions$field))
  route_directions$coltype <- rep('c', length(route_directions$field))
  route_directions$coltype[route_directions$field %in% c("direction_id")] <- 'i'

  environment()

}

