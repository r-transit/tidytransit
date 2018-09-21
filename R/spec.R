#' Function that creates gtfs meta data
#' @return Environment with gtfs data.
#' @noRd

get_gtfs_meta <- function() {

  # required files ----------------------------------------------------------

  # agency
  assign("agency", list())
  agency$field <- c('agency_id', 'agency_name', 'agency_url', 'agency_timezone', 'agency_lang', 'agency_phone', 'agency_fare_url', 'agency_email')
  agency$field_spec <- c('opt', 'req', 'req', 'req', 'opt', 'opt', 'opt', 'opt')
  names(agency$field_spec) <- agency$field
  agency$coltype <- rep('c', 8)
  agency$file_spec <- 'req'
  
  # stops
  assign("stops", list())
  stops$field <- c('stop_id', 'stop_code', 'platform_code', 'stop_name', 'stop_desc', 'stop_lat', 'stop_lon', 'zone_id', 'stop_url', 'location_type', 'parent_station', 'stop_timezone', 'wheelchair_boarding')
  stops$field_spec <- c('req', 'opt', 'opt', 'req', 'opt', 'req', 'req', 'opt', 'opt', 'opt', 'opt', 'opt', 'opt')
  names(stops$field_spec) <- stops$field
  stops$coltype <- rep('c', length(stops$field))
  stops$coltype[which(stops$field %in% c('stop_lat', 'stop_lon'))] <- 'd' # double
  stops$coltype[which(stops$field %in% c('location_type', 'wheelchair_boarding'))] <- 'i' #integers
  stops$file_spec <- 'req'
  
  # routes
  assign("routes", list())
  routes$field <- c('route_id', 'agency_id', 'route_short_name', 'route_long_name', 'route_desc', 'route_type', 'route_url', 'route_color', 'route_text_color', 'route_sort_order')
  routes$field_spec <- c('req', 'opt', 'req', 'req', 'opt', 'req', 'opt', 'opt', 'opt', 'opt')
  names(routes$field_spec) <- routes$field
  routes$coltype <- rep('c', length(routes$field))
  routes$coltype[routes$field %in% c('route_type', 'route_sort_order')] <- 'i'
  routes$file_spec <- 'req'
  
  # trips
  assign("trips", list())
  trips$field <- c('route_id', 'service_id', 'trip_id', 'trip_headsign', 'trip_short_name', 'direction_id', 'block_id', 'shape_id', 'wheelchair_accessible', 'bikes_allowed')
  trips$field_spec <- c('req', 'req', 'req', 'opt', 'opt', 'opt', 'opt', 'opt', 'opt', 'opt')
  names(trips$field_spec) <- trips$field
  trips$coltype <- rep('c', length(trips$field))
  trips$coltype[trips$field %in% c('direction_id', 'wheelchair_accessible', 'bikes_allowed')] <- 'i'
  trips$file_spec <- 'req'
  
  # stop_times
  assign("stop_times", list())
  stop_times$field <- c('trip_id', 'arrival_time', 'departure_time', 'stop_id', 'stop_sequence', 'stop_headsign', 'pickup_type', 'drop_off_type', 'shape_dist_traveled', 'timepoint')
  stop_times$field_spec <- c('req', 'req', 'req', 'req', 'req', 'opt', 'opt', 'opt', 'opt', 'opt')
  names(stop_times$field_spec) <- stop_times$field
  stop_times$coltype <- rep('c', length(stop_times$field))
  stop_times$coltype[stop_times$field %in% c('stop_sequence', 'pickup_type', 'drop_off_type', 'timepoint')] <- 'i'
  stop_times$coltype[stop_times$field %in% c('shape_dist_traveled')] <- 'd'
  stop_times$file_spec <- 'req'
  
  # calendar
  assign("calendar", list())
  calendar$field <- c('service_id', 'service_name', 'monday', 'tuesday', 'wednesday', 'thursday', 'friday', 'saturday', 'sunday', 'start_date', 'end_date')
  calendar$field_spec <- rep('req', times = length(calendar$field))
  calendar$field_spec[2] <- 'opt'
  names(calendar$field_spec) <- calendar$field
  calendar$coltype <- rep('i', length(calendar$field))
  calendar$coltype[calendar$field %in% c('service_id', 'service_name')] <- 'c'
  calendar$coltype[calendar$field %in% c('start_date', 'end_date')] <- 'D'
  calendar$file_spec <- 'req'

  # optional files ----------------------------------------------------------

  # calendar_dates
  assign("calendar_dates", list())
  calendar_dates$field <- c('service_id', 'date', 'exception_type')
  calendar_dates$field_spec <- c('req', 'req', 'req')
  names(calendar_dates$field_spec) <- calendar_dates$field
  calendar_dates$coltype <- rep('c', length(calendar_dates$field))
  calendar_dates$coltype[calendar_dates$field %in% c('exception_type')] <- 'i'
  calendar_dates$coltype[calendar_dates$field %in% c('date')] <- 'D'
  calendar_dates$file_spec <- 'opt'
  
  # fare_attributes
  assign("fare_attributes", list())
  fare_attributes$field <- c('agency_id', 'fare_id', 'price', 'currency_type', 'payment_method', 'transfers', 'transfer_duration')
  fare_attributes$field_spec <- c('opt', 'req', 'req', 'req', 'req', 'req', 'opt')
  names(fare_attributes$field_spec) <- fare_attributes$field
  fare_attributes$coltype <- rep('c', length(fare_attributes$field))
  fare_attributes$coltype[fare_attributes$field %in% c('payment_method', 'transfers')] <- 'i'
  fare_attributes$coltype[fare_attributes$field %in% c('transfer_duration')] <- 'd'
  fare_attributes$file_spec <- 'opt'
  
  # fare_rules
  assign("fare_rules", list())
  fare_rules$field <- c('fare_id', 'route_id', 'origin_id', 'destination_id', 'contains_id')
  fare_rules$field_spec <- c('req', 'opt', 'opt', 'opt', 'opt')
  names(fare_rules$field_spec) <- fare_rules$field
  fare_rules$coltype <- rep('c', length(fare_rules$field))
  fare_rules$coltype[fare_rules$field %in% c('direction_id', 'wheelchair_accessible', 'bikes_allowed')] <- 'i'
  fare_rules$file_spec <- 'opt'
    
  # shapes
  assign("shapes", list())
  shapes$field <- c('shape_id', 'shape_pt_lat', 'shape_pt_lon', 'shape_pt_sequence', 'shape_dist_traveled')
  shapes$field_spec <- c('req', 'req', 'req', 'req', 'opt')
  names(shapes$field_spec) <- shapes$field
  shapes$coltype <- rep('d', length(shapes$field))
  shapes$coltype[shapes$field %in% c('shape_id')] <- 'c'
  shapes$coltype[shapes$field %in% c('shape_pt_sequence')] <- 'i'
  shapes$file_spec <- 'opt'
  
  # frequencies
  assign("frequencies", list())
  frequencies$field <- c('trip_id', 'start_time', 'end_time', 'headway_secs', 'exact_times')
  frequencies$field_spec <- c('req', 'req', 'req', 'req', 'opt')
  names(frequencies$field_spec) <- frequencies$field
  frequencies$coltype <- rep('c', length(frequencies$field))
  frequencies$coltype[frequencies$field %in% c('headway_secs')] <- 'd'
  frequencies$coltype[frequencies$field %in% c('exact_times')] <- 'i'
  frequencies$file_spec <- 'opt'
  
  # transfers
  assign("transfers", list())
  transfers$field <- c('from_stop_id', 'to_stop_id', 'transfer_type', 'min_transfer_time')
  transfers$field_spec <- c('req', 'req', 'req', 'opt')
  names(transfers$field_spec) <- transfers$field
  transfers$coltype <- rep('c', length(transfers$field))
  transfers$coltype[transfers$field %in% c('exception_type')] <- 'i'
  transfers$file_spec <- 'opt'
  
  # feed_info
  assign("feed_info", list())
  feed_info$field <- c('feed_id', 'feed_publisher_name', 'feed_publisher_url', 'feed_lang', 'feed_version', 'feed_license', 'feed_contact_email', 'feed_contact_url', 'feed_start_date', 'feed_end_date')
  feed_info$field_spec <- rep('opt', times = length(feed_info$field))
  feed_info$field_spec[c(2:4)] <- 'req'
  names(feed_info$field_spec) <- feed_info$field
  feed_info$coltype <- rep('c', length(feed_info$field))
  feed_info$coltype[transfers$field %in% c('feed_start_date', 'feed_end_date')] <- 'D'
  feed_info$file_spec <- 'opt'
  
  meta <- list(agency, stops, routes, trips, stop_times, calendar, calendar_dates, fare_attributes, fare_rules, shapes, frequencies, transfers, feed_info)
  attributes(meta) <- list(
    names = c("agency", "stops", "routes", "trips", "stop_times", "calendar", "calendar_dates", "fare_attributes", "fare_rules", "shapes", "frequencies", "transfers", "feed_info"),
    file_spec = c('req', 'req', 'req', 'req','req', 'req', 'opt', 'opt', 'opt', 'opt', 'opt', 'opt', 'opt'))
  
  return(meta)
}

get_gtfs_plus_meta <- function() {
  
  # calendar_attributes
  assign("calendar_attributes", list())
  calendar_attributes$field <- c('service_id', 'service_description')
  calendar_attributes$field_spec <- rep('req', times = 2)
  calendar_attributes$coltype <- c('c', 'c')
  
  # directions
  assign("directions", list())
  directions$field <- c('route_id', 'direction_id', 'direction')
  directions$field_spec <- rep('req', times = 3)
  directions$coltype <- c('c', 'i', 'c')
  
  # fare_rider_categories
  assign("fare_rider_categories", list())
  fare_rider_categories$field <- c('fare_id', 'rider_category_id', 'price')
  fare_rider_categories$field_spec <- c('req', 'req', 'req')
  fare_rider_categories$coltype <- c('c', 'c', 'd')
  
  # farezone_attributes
  assign("farezone_attributes", list())
  farezone_attributes$field <- c('zone_id', 'zone_name')
  farezone_attributes$field_spec <- c('req', 'req')
  farezone_attributes$coltype <- rep('c', length(farezone_attributes$field))
  
  # rider_categories
  assign("rider_categories", list())
  rider_categories$field <- c('rider_category_id', 'rider_category_description')
  rider_categories$field_spec <- c('req', 'req')
  rider_categories$coltype <- rep('c', length(rider_categories$field))
  
  # route_directions
  assign("route_directions", list())
  route_directions$field <- c("route_id", "direction_id", "direction_name")
  route_directions$field_spec <- rep('req', length(route_directions$field))
  route_directions$coltype <- rep('c', length(route_directions$field))
  route_directions$coltype[route_directions$field %in% c("direction_id")] <- 'i'
  
  # stop_attributes
  assign("stop_attributes", list())
  stop_attributes$field <- c('stop_id', 'stop_city')
  stop_attributes$field_spec <- c('req', 'req')
  stop_attributes$coltype <- c('c', 'c')
  
  # timetable_stop_order
  assign("timetable_stop_order", list())
  timetable_stop_order$field <- c("timetable_id", "stop_id", "stop_sequence", "stop_name", "connected_routes")
  timetable_stop_order$field_spec <- rep('req', length(timetable_stop_order$field))
  timetable_stop_order$field_spec[timetable_stop_order$field %in% c("stop_name", "connected_routes")] <- 'opt'
  timetable_stop_order$coltype <- rep('c', length(timetable_stop_order$field))
  timetable_stop_order$coltype[timetable_stop_order$field %in% c("stop_sequence")] <- 'i'
  
  # timetables
  assign("timetables", list())
  timetables$field <- c("timetable_id", "route_id", "direction_id", "start_date", "end_date", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday", "route_label", "service_notes", "direction_label", "orientation")
  timetables$field_spec <- rep('req', length(timetables$field))
  timetables$field_spec[timetables$field %in% c("route_label", "service_notes", "direction_label", "orientation")] <- 'opt'
  timetables$coltype <- rep('c', length(timetables$field))
  timetables$coltype[timetables$field %in% c("direction_id", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday")] <- 'i'
  
  environment()
}

