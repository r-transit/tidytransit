#' Function that creates gtfs meta data
#' @return Environment with gtfs data.
#' @noRd
get_gtfs_meta <- function() {
  
  spec_setup_fields = function(field_names, presence, coltypes, file_presence, primary_key = NA) {
    stopifnot(length(field_names) == length(presence))
    stopifnot(length(field_names) == length(coltypes))
    stopifnot(file_presence %in% c("req", "opt"))
    stopifnot(all(presence %in% c("req", "opt")))
    stopifnot(length(file_presence) == 1)
    
    file.txt = list()
    file.txt$field <- field_names
    file.txt$field_spec <- presence
    names(file.txt$field_spec) <- field_names
    file.txt$coltype <- coltypes
    file.txt$file_spec <- file_presence
    file.txt$primary_key <- primary_key
    
    return(file.txt)
  }
  
  m = list()
  
  # required files ----------------------------------------------------------
  
  # agency
  m$agency <- spec_setup_fields(
    c("agency_id", "agency_name", "agency_url",
      "agency_timezone", "agency_lang", "agency_phone",
      "agency_fare_url", "agency_email"),
    c("opt", "req", "req",
      "req", "opt", "opt",
      "opt", "opt"),
    rep("character", 8),
    "req",
    "agency_id")
  
  # stops
  m$stops <- spec_setup_fields(
    c("stop_id", "stop_code", "stop_name", 
      "stop_desc", "stop_lat", "stop_lon", 
      "zone_id", "stop_url", "location_type",
      "parent_station", "stop_timezone", "wheelchair_boarding",
      "level_id", "platform_code"),
    c("req", "opt", "req",
      "opt", "req", "req",
      rep("opt", 8)),
    c("character", "character", "character",
      "character", "numeric", "numeric",
      "character", "character", "integer",
      "character", "character", "integer",
      "character", "character"),
    "req",
    "stop_id"
  )
  
  # routes
  m$routes <- spec_setup_fields(
    c("route_id", "agency_id", "route_short_name", 
      "route_long_name", "route_desc", "route_type", 
      "route_url", "route_color", "route_text_color",
      "route_sort_order", "continuous_pickup", "continuous_drop_off"),
    c("req", "opt", "req", 
      "req", "opt", "req", 
      "opt", "opt", "opt", 
      "opt", "opt", "opt"),
    c("character", "character", "character",
      "character", "character", "integer",
      "character", "character", "character",
      "integer", "integer", "integer"),
    "req",
    "route_id"
  )
  
  # trips
  m$trips <- spec_setup_fields(
    c("route_id", "service_id", "trip_id", 
      "trip_headsign", "trip_short_name", "direction_id",
      "block_id", "shape_id", "wheelchair_accessible",
      "bikes_allowed"),
    c("req", "req", "req",
      "opt", "opt", "opt",
      "opt", "opt", "opt",
      "opt"),
    c("character", "character", "character",
      "character", "character", "integer",
      "character", "character", "integer",
      "integer"),
    "req",
    "trip_id")
  
  # stop_times
  m$stop_times <- spec_setup_fields(
    c("trip_id", "arrival_time", "departure_time", 
      "stop_id", "stop_sequence", "stop_headsign",
      "pickup_type", "drop_off_type", "continuous_pickup", 
      "continuous_drop_off", "shape_dist_traveled", "timepoint"),
    c("req", "req", "req", 
      "req", "req", "opt", 
      "opt", "opt", "opt", 
      "opt", "opt", "opt"),
    c("character", "character", "character",
      "character", "integer", "character",
      "integer", "integer", "integer",
      "integer", "numeric", "integer"),
    "req",
    c("trip_id", "stop_sequence"))
  
  # conditionally required --------------------------------------------------
  
  # calendar
  m$calendar <- spec_setup_fields(
    c("service_id", "monday", "tuesday", 
      "wednesday", "thursday", "friday", 
      "saturday", "sunday", "start_date", 
      "end_date"),
    rep("req", times = 10),
    c("character", "integer", "integer",
      "integer", "integer", "integer", 
      "integer", "integer", "character",
      "character"),
    "req",
    "service_id")
  
  
  # calendar_dates
  m$calendar_dates <- spec_setup_fields(
    c("service_id", "date", "exception_type"),
    c("req", "req", "req"),
    c("character", "character", "integer"),
    "opt",
    c("service_id", "date"))
  
  # optional files ----------------------------------------------------------
  
  # fare_attributes
  m$fare_attributes <- spec_setup_fields(
    c("agency_id", "fare_id", "price", 
      "currency_type", "payment_method", "transfers",
      "transfer_duration"),
    c("opt", "req", "req", 
      "req", "req", "req",
      "opt"),
    c("character", "character", "character",
      "character", "integer", "integer",
      "numeric"),
    "opt",
    "fare_id")
  
  # fare_rules
  m$fare_rules <- spec_setup_fields(
    c("fare_id", "route_id", "origin_id", 
      "destination_id", "contains_id"),
    c("req", "opt", "opt",
      "opt", "opt"),
    c("character", "character", "character",
      "character", "character"),
    "opt",
    "*")
  
  # fare_products
  m$fare_products <- spec_setup_fields(
    c("fare_product_id", "fare_product_name", "fare_media_id",
     "amount", "currency"),
    c("req", "opt", "opt", "req", "req"),
    c("character", "character", "character", "numeric", "numeric"), # TODO currency should be handled with integers
    "opt",
    c("fare_product_id", "fare_media_id"))
  
  # fare_leg_rules
  m$fare_leg_rules <- spec_setup_fields(
    c("leg_group_id", "network_id", "from_area_id",
      "to_area_id", "fare_product_id"),
    c("opt", "opt", "opt", "opt", "req"),
    c(rep("character", 5)),
    "opt",
    c("network_id", "from_area_id", "to_area_id", "fare_product_id"))
  
  # fare_transfer_rules
  m$fare_transfer_rules <- spec_setup_fields(
    c("from_leg_group_id", "to_leg_group_id", "transfer_count",
      "duration_limit", "duration_limit_type", "fare_transfer_type",
      "fare_product_id"),
    c("opt", "opt", "opt",
      "opt", "opt", "req",
      "opt"),
    c(
      "character", "character", "integer",
      "integer", "integer", "integer",
      "character"),
    "opt",
    c("from_leg_group_id", "to_leg_group_id", "fare_product_id", "transfer_count", "duration_limit"))
  
  # areas 
  m$areas <- spec_setup_fields(
    c("area_id", "area_name"),
    c("req", "opt"),
    c("character", "character"),
    "opt",
    "area_id"
  )
  
  # stop_areas
  m$stop_areas <- spec_setup_fields(
    c("area_id", "stop_id"),
    c("req", "req"),
    c("character", "character"),
    "opt",
    "*"
  )
  
  # shapes
  m$shapes <- spec_setup_fields(
    c("shape_id", "shape_pt_lat", "shape_pt_lon",
      "shape_pt_sequence", "shape_dist_traveled"),
    c("req", "req", "req", 
      "req", "opt"),
    c("character", "numeric", "numeric",
      "integer", "numeric"),
    "opt",
    c("shape_id", "shape_pt_sequence"))
  
  # frequencies
  m$frequencies <- spec_setup_fields(
    c("trip_id", "start_time", "end_time",
      "headway_secs", "exact_times"),
    c("req", "req", "req",
      "req", "opt"),
    c("character", "character", "character",
      "numeric", "integer"),
    "opt",
    c("trip_id", "start_time"))
  
  # transfers
  m$transfers <- spec_setup_fields(
    c("from_stop_id", "to_stop_id", "transfer_type",
      "min_transfer_time"),
    c("req", "req", "req",
      "opt"),
    c("character", "character", "character",
      "integer"),
    "opt",
    c("from_stop_id", "to_stop_id", "from_trip_id", "to_trip_id", "from_route_id", "to_route_id"))
  
  # pathways
  m$pathways <- spec_setup_fields(
    c("pathway_id", "from_stop_id", "to_stop_id",
      "pathway_mode", "is_bidirectional", "length",
      "traversal_time", "stair_count", "max_slope",
      "min_width", "signposted_as", "reversed_signposted_as"),
    c(rep("req", 5), rep("opt", 7)),
    c("character", "character", "character",
      "character", "character", "numeric",
      "integer", "integer", "numeric",
      "numeric", "character", "character"),
    "opt",
    "pathway_id")
  
  # levels
  m$levels <- spec_setup_fields(
    c("level_id", "level_index", "level_name"),
    c("req", "req", "opt"),
    c("character", "numeric", "character"),
    "opt",
    "level_id"
  )
  
  # translations
  m$translations <- spec_setup_fields(
    c("table_name", "field_name", "language",
      "translation", "record_id", "record_sub_id",
      "field_value"),
    c("req", "req", "req",
      "req", "opt", "opt",
      "opt"),
    rep("character", 7),
    "opt",
    c("table_name", "field_name", "language", "record_id", "record_sub_id", "field_value"))
  
  # feed_info 
  m$feed_info <- spec_setup_fields(
    c("feed_publisher_name", "feed_publisher_url", "feed_lang",
      "feed_start_date", "feed_end_date", "feed_version",
      "feed_contact_email", "feed_contact_url"),
    c("req", "req", "req", 
      "opt", "opt", "opt", 
      "opt", "opt"),
    rep("character", 8),
    "opt",
    NA
  )
  
  # attributions
  m$attributions <- spec_setup_fields(
    c("attribution_id", "agency_id", "route_id", 
      "trip_id", "organization_name", "is_producer", 
      "is_operator", "is_authority", "attribution_url", 
      "attribution_email", "attribution_phone"),
    c(rep("opt", 4), "req", rep("opt", 6)),
    rep("character", 11),
    "opt",
    "attribution_id")
  
  return(m)
}

gtfs_meta = get_gtfs_meta()
