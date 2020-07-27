#' Function that creates gtfs meta data
#' @return Environment with gtfs data.
#' @noRd
get_gtfs_meta <- function() {

  # required files ----------------------------------------------------------

  # agency
  assign("agency", list())
  agency$field <- c("agency_id", "agency_name", "agency_url", "agency_timezone", 
                    "agency_lang", "agency_phone", "agency_fare_url", "agency_email")
  agency$field_spec <- c("opt", "req", "req", "req", "opt", "opt", "opt", "opt")
  names(agency$field_spec) <- agency$field
  agency$coltype <- rep("c", 8)
  agency$file_spec <- "req"
  
  # stops
  assign("stops", list())
  stops$field <- c("stop_id", "stop_code", "stop_name", 
                   "stop_desc", "stop_lat", "stop_lon", 
                   "zone_id", "stop_url", 
                   "location_type", "parent_station", 
                   "stop_timezone", "wheelchair_boarding",
                   "level_id", "platform_code")
  stops$field_spec <- c("req", "opt", "req", "opt",
                        "req", "req", 
                        rep("opt", 8))
  names(stops$field_spec) <- stops$field
  stops$coltype <- rep("c", length(stops$field))
  stops$coltype[which(stops$field %in% 
                        c("stop_lat", "stop_lon"))] <- "d" # double
  stops$coltype[which(stops$field %in% 
                        c("location_type",
                          "wheelchair_boarding"))] <- "i" # integers
  stops$file_spec <- "req"
  
  # routes
  assign("routes", list())
  routes$field <- c("route_id", "agency_id", "route_short_name", 
                    "route_long_name", "route_desc", "route_type", 
                    "route_url", "route_color", 
                    "route_text_color", "route_sort_order",
                    "continuous_pickup", "continuous_drop_off")
  routes$field_spec <- c("req", "opt", "req", 
                         "req", "opt", "req", 
                         "opt", "opt", "opt", 
                         "opt", "opt", "opt")
  names(routes$field_spec) <- routes$field
  routes$coltype <- rep("c", length(routes$field))
  routes$coltype[routes$field %in% c("route_type", "route_sort_order",
                                     "continuous_pickup", 
                                     "continuous_drop_off")] <- "i"
  routes$file_spec <- "req"
  
  # trips
  assign("trips", list())
  trips$field <- c("route_id", "service_id", "trip_id", 
                   "trip_headsign", "trip_short_name", 
                   "direction_id", "block_id", "shape_id", 
                   "wheelchair_accessible", "bikes_allowed")
  trips$field_spec <- c("req", "req", "req", "opt", 
                        "opt", "opt", "opt", "opt", 
                        "opt", "opt")
  names(trips$field_spec) <- trips$field
  trips$coltype <- rep("c", length(trips$field))
  trips$coltype[trips$field %in% c("direction_id",
                                   "wheelchair_accessible", 
                                   "bikes_allowed")] <- "i"
  trips$file_spec <- "req"
  
  # stop_times
  assign("stop_times", list())
  stop_times$field <- c("trip_id", "arrival_time", 
                        "departure_time", 
                        "stop_id", "stop_sequence", 
                        "stop_headsign", "pickup_type", 
                        "drop_off_type", "continuous_pickup", 
                        "continuous_drop_off", "shape_dist_traveled", 
                        "timepoint")
  stop_times$field_spec <- c("req", "req", "req", 
                             "req", "req", "opt", 
                             "opt", "opt", "opt", 
                             "opt", "opt", "opt")
  names(stop_times$field_spec) <- stop_times$field
  stop_times$coltype <- rep("c", length(stop_times$field))
  stop_times$coltype[stop_times$field 
                     %in% c("stop_sequence", "pickup_type", 
                            "drop_off_type", "continuous_pickup", 
                            "continuous_drop_off","timepoint")] <- "i"
  stop_times$coltype[stop_times$field %in% 
                       c("shape_dist_traveled")] <- "d"
  stop_times$file_spec <- "req"
  
  # calendar
  assign("calendar", list())
  calendar$field <- c("service_id", "monday", "tuesday", 
                      "wednesday", "thursday", "friday", 
                      "saturday", "sunday", "start_date", 
                      "end_date")
  calendar$field_spec <- rep("req", times = length(calendar$field))
  names(calendar$field_spec) <- calendar$field
  calendar$coltype <- rep("i", length(calendar$field))
  calendar$coltype[calendar$field %in% c("service_id")] <- "c"
  calendar$coltype[calendar$field %in% c("start_date", "end_date")] <- "D"
  calendar$file_spec <- "req"

  # optional files ----------------------------------------------------------

  # calendar_dates
  assign("calendar_dates", list())
  calendar_dates$field <- c("service_id", "date", "exception_type")
  calendar_dates$field_spec <- c("req", "req", "req")
  names(calendar_dates$field_spec) <- calendar_dates$field
  calendar_dates$coltype <- rep("c", length(calendar_dates$field))
  calendar_dates$coltype[calendar_dates$field %in% c("exception_type")] <- "i"
  calendar_dates$coltype[calendar_dates$field %in% c("date")] <- "D"
  calendar_dates$file_spec <- "opt"
  
  # fare_attributes
  assign("fare_attributes", list())
  fare_attributes$field <- c("agency_id", "fare_id", "price", 
                             "currency_type", "payment_method", 
                             "transfers", "transfer_duration")
  fare_attributes$field_spec <- c("opt", "req", "req", 
                                  "req", "req", "req", "opt")
  names(fare_attributes$field_spec) <- fare_attributes$field
  fare_attributes$coltype <- rep("c", length(fare_attributes$field))
  fare_attributes$coltype[fare_attributes$field %in% 
                            c("payment_method", "transfers")] <- "i"
  fare_attributes$coltype[fare_attributes$field %in% 
                            c("transfer_duration")] <- "d"
  fare_attributes$file_spec <- "opt"
  
  # fare_rules
  assign("fare_rules", list())
  fare_rules$field <- c("fare_id", "route_id", "origin_id", 
                        "destination_id", "contains_id")
  fare_rules$field_spec <- c("req", "opt", "opt", "opt", "opt")
  names(fare_rules$field_spec) <- fare_rules$field
  fare_rules$coltype <- rep("c", length(fare_rules$field))
  fare_rules$coltype[fare_rules$field
                     %in% c("direction_id", 
                            "wheelchair_accessible", 
                            "bikes_allowed")] <- "i"
  fare_rules$file_spec <- "opt"
    
  # shapes
  assign("shapes", list())
  shapes$field <- c("shape_id", "shape_pt_lat", 
                    "shape_pt_lon", "shape_pt_sequence", 
                    "shape_dist_traveled")
  shapes$field_spec <- c("req", "req", "req", "req", "opt")
  names(shapes$field_spec) <- shapes$field
  shapes$coltype <- rep("d", length(shapes$field))
  shapes$coltype[shapes$field %in% c("shape_id")] <- "c"
  shapes$coltype[shapes$field %in% c("shape_pt_sequence")] <- "i"
  shapes$file_spec <- "opt"
  
  # frequencies
  assign("frequencies", list())
  frequencies$field <- c("trip_id", "start_time", 
                         "end_time", "headway_secs", 
                         "exact_times")
  frequencies$field_spec <- c("req", "req", "req", "req", "opt")
  names(frequencies$field_spec) <- frequencies$field
  frequencies$coltype <- rep("c", length(frequencies$field))
  frequencies$coltype[frequencies$field %in% c("headway_secs")] <- "d"
  frequencies$coltype[frequencies$field %in% c("exact_times")] <- "i"
  frequencies$file_spec <- "opt"
  
  # transfers
  assign("transfers", list())
  transfers$field <- c("from_stop_id", "to_stop_id", 
                       "transfer_type", "min_transfer_time")
  transfers$field_spec <- c("req", "req", "req", "opt")
  names(transfers$field_spec) <- transfers$field
  transfers$coltype <- rep("c", length(transfers$field))
  transfers$coltype[transfers$field %in% c("min_transfer_time")] <- "i"
  transfers$file_spec <- "opt"
  
  # pathways
  assign("pathways", list())
  pathways$field <- c("pathway_id", "from_stop_id", "to_stop_id",
    "pathway_mode", "is_bidirectional", "length", "traversal_time",
    "stair_count", "max_slope", "min_width", "signposted_as", "reversed_signposted_as")
  pathways$field_spec <- c(rep("req", 5), rep("opt", 7))
  names(pathways$field_spec) <- pathways$field
  pathways$coltype <- rep("c", length(pathways$field))
  pathways$coltype[pathways$field %in% c("traversal_time", "stair_count")] <- "i"
  pathways$coltype[pathways$field %in% c("length", "max_slope", "min_width")] <- "d"
  pathways$file_spec <- "opt"
  
  # levels
  assign("levels", list())
  levels$field <- c("level_id", "level_index", "level_name")
  levels$field_spec <- c("req", "req", "opt")
  names(levels$field_spec) <- levels$field
  levels$coltype <- c("c", "d", "c")
  levels$file_spec <- "opt"
  
  # feed_info 
  assign("feed_info", list())
  feed_info$field <- c("feed_publisher_name", "feed_publisher_url", 
                       "feed_lang", "feed_start_date", "feed_end_date", 
                       "feed_version", "feed_contact_email", "feed_contact_url")
  feed_info$field_spec <- c("req", "req", "req", 
                            "opt", "opt", "opt", 
                            "opt", "opt")
  names(feed_info$field_spec) <- feed_info$field
  feed_info$coltype <- rep("c", length(feed_info$field))
  feed_info$coltype[transfers$field %in% 
                      c("feed_start_date", "feed_end_date")] <- "D"
  feed_info$file_spec <- "opt"
  
  # translations
  assign("translations", list())
  translations$field <- c("table_name", "field_name", "language", "translation", 
                       "record_id", "record_sub_id", "field_value")
  translations$field_spec <- c("req", "req", "req", "req",
                            "opt", "opt", "opt")
  names(translations$field_spec) <- translations$field
  translations$coltype <- rep("c", length(translations$field))
  translations$file_spec <- "opt"
  
  # attributions
  assign("attributions", list())
  attributions$field <- c("attribution_id", "agency_id", "route_id", 
                          "trip_id", "organization_name", "is_producer", 
                          "is_operator", "is_authority", "attribution_url", 
                          "attribution_email", "attribution_phone")
  attributions$field_spec <- c(rep("opt", 4), "req", rep("opt", 6))
  names(attributions$field_spec) <- attributions$field
  attributions$coltype <- rep("c", length(attributions$field))
  attributions$file_spec <- "opt"
  
  
  # create meta object ####
  meta <- list(agency, stops, routes, trips, 
               stop_times, calendar, calendar_dates,
               fare_attributes, fare_rules,
               shapes, frequencies, transfers,
               pathways, levels, feed_info,
               translations, attributions)
  attributes(meta) <- list(
    names = c("agency", "stops", "routes",
              "trips", "stop_times", "calendar",
              "calendar_dates", "fare_attributes",
              "fare_rules", "shapes", "frequencies",
              "transfers", "pathways", "levels", 
              "feed_info", "translations", "attributions"),
    file_spec = c("req", "req", "req", 
                  "req", "req", "req", 
                  "opt", "opt",
                  "opt", "opt", "opt", 
                  "opt", "opt", "opt", 
                  "opt", "opt",  "opt"))
  return(meta)
}
