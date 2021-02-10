# files specification -----------------------------------------------------

# metadata built based on https://developers.google.com/transit/gtfs/reference

specified_files <- c(
  "agency", "stops", "routes", "trips", "stop_times", "calendar",
  "calendar_dates", "fare_attributes", "fare_rules", "shapes", "frequencies",
  "transfers", "pathways", "levels", "feed_info", "translations", "attributions"
)

required_files <- c(
  "agency", "stops", "routes", "trips", "stop_times", "calendar"
)

# fields specification ----------------------------------------------------

# agency
agency_field <- c(
  "agency_id", "agency_name", "agency_url", "agency_timezone", "agency_lang",
  "agency_phone", "agency_fare_url", "agency_email"
)
agency_field_spec <- c("opt", "req", "req", "req", "opt", "opt", "opt", "opt")

# stops
stops_field <- c(
  "stop_id", "stop_code", "stop_name", "stop_desc", "stop_lat", "stop_lon",
  "zone_id", "stop_url", "location_type", "parent_station", "stop_timezone",
  "wheelchair_boarding", "level_id", "platform_code"
)
stops_field_spec <- c("req", "opt", "req", "opt", "req", "req", rep("opt", 8))

# routes
routes_field <- c(
  "route_id", "agency_id", "route_short_name", "route_long_name", "route_desc",
  "route_type", "route_url", "route_color", "route_text_color",
  "route_sort_order", "continuous_pickup", "continuous_drop_off"
)
routes_field_spec <- c("req", "opt", "req", "req", "opt", "req", rep("opt", 6))

# trips
trips_field <- c(
  "route_id", "service_id", "trip_id", "trip_headsign", "trip_short_name",
  "direction_id", "block_id", "shape_id", "wheelchair_accessible",
  "bikes_allowed"
)
trips_field_spec <- c("req", "req", "req", rep("opt", 7))

# stop_times
stop_times_field <- c(
  "trip_id", "arrival_time", "departure_time", "stop_id", "stop_sequence",
  "stop_headsign", "pickup_type", "drop_off_type", "continuous_pickup",
  "continuous_drop_off", "shape_dist_traveled", "timepoint"
)
stop_times_field_spec <- c("req", "req", "req", "req", "req", rep("opt", 7))

# calendar
calendar_field <- c(
  "service_id", "monday", "tuesday", "wednesday", "thursday", "friday",
  "saturday", "sunday", "start_date", "end_date"
)
calendar_field_spec <- rep("req", length(calendar_field))

# calendar_dates
calendar_dates_field <- c("service_id", "date", "exception_type")
calendar_dates_field_spec <- c("req", "req", "req")

# fare_attributes
fare_attributes_field <- c(
  "agency_id", "fare_id", "price", "currency_type", "payment_method",
  "transfers", "transfer_duration"
)
fare_attributes_field_spec <- c("opt", "req", "req", "req", "req", "req", "opt")

# fare_rules
fare_rules_field <- c(
  "fare_id", "route_id", "origin_id", "destination_id", "contains_id"
)
fare_rules_field_spec <- c("req", "opt", "opt", "opt", "opt")

# shapes
shapes_field <- c(
  "shape_id", "shape_pt_lat", "shape_pt_lon", "shape_pt_sequence",
  "shape_dist_traveled"
)
shapes_field_spec <- c("req", "req", "req", "req", "opt")

# frequencies
frequencies_field <- c(
  "trip_id", "start_time", "end_time", "headway_secs", "exact_times"
)
frequencies_field_spec <- c("req", "req", "req", "req", "opt")

# transfers
transfers_field <- c(
  "from_stop_id", "to_stop_id", "transfer_type", "min_transfer_time"
)
transfers_field_spec <- c("req", "req", "req", "opt")

# pathways
pathways_field <- c(
  "pathway_id", "from_stop_id", "to_stop_id", "pathway_mode", "is_bidirectional",
  "length", "traversal_time", "stair_count", "max_slope", "min_width",
  "signposted_as", "reversed_signposted_as"
)
pathways_field_spec <- c(rep("req", 5), rep("opt", 7))

# levels
levels_field <- c("level_id", "level_index", "level_name")
levels_field_spec <- c("req", "req", "opt")

# feed_info
feed_info_field <- c(
  "feed_publisher_name", "feed_publisher_url", "feed_lang", "feed_start_date",
  "feed_end_date", "feed_version", "feed_contact_email", "feed_contact_url"
)
feed_info_field_spec <- c("req", "req", "req", rep("opt", 5))

# translations
translations_field <- c(
  "table_name", "field_name", "language", "translation", "record_id",
  "record_sub_id", "field_value"
)
translations_field_spec <- c("req", "req", "req", "req", "opt", "opt", "opt")

# attributions
attributions_field <- c(
  "attribution_id", "agency_id", "route_id", "trip_id", "organization_name",
  "is_producer", "is_operator", "is_authority", "attribution_url",
  "attribution_email", "attribution_phone"
)
attributions_field_spec <- c(rep("opt", 4), "req", rep("opt", 6))

# fields type specification -----------------------------------------------

integer_fields <- c(
  "location_type", "wheelchair_boarding", "route_type", "route_sort_order",
  "continuous_pickup", "continuous_drop_off", "direction_id",
  "wheelchair_accessible", "bikes_allowed", "stop_sequence", "pickup_type",
  "drop_off_type", "timepoint", "monday", "tuesday", "wednesday", "thursday",
  "friday", "saturday", "sunday", "exception_type", "payment_method",
  "transfers", "transfer_duration", "shape_pt_sequence", "headway_secs",
  "exact_times", "transfer_type", "min_transfer_time", "pathway_mode",
  "is_bidirectional", "traversal_time", "stair_count", "is_producer",
  "is_operator", "is_authority"
)

double_fields <- c(
  "stop_lat", "stop_lon", "shape_dist_traveled", "price", "shape_pt_lat",
  "shape_pt_lon", "length", "max_slope", "min_width", "level_index"
)

time_fields <- c(
  "arrival_time", "departure_time", "start_time", "end_time"
)

date_fields <- c(
  "start_date", "end_date", "date", "feed_start_date", "feed_end_date"
)

# object creation ---------------------------------------------------------

gtfs_metadata <- lapply(
  specified_files,
  function(file) {

    field <- get(paste0(file, "_field"))

    field_spec <- stats::setNames(
      get(paste0(file, "_field_spec")), nm = field
    )

    coltype <- rep("character", length(field))
    coltype <- ifelse(field %in% integer_fields, "integer", coltype)
    coltype <- ifelse(field %in% double_fields, "double", coltype)
    coltype <- ifelse(field %in% date_fields, "gtfs_date", coltype)
    coltype <- stats::setNames(coltype, nm = field)

    file_spec <- ifelse(file %in% required_files, "req", "opt")

    file_list <- list(
      field = field,
      field_spec = field_spec,
      coltype = coltype,
      file_spec = file_spec
    )

  }
)

attributes(gtfs_metadata) <- list(
  names = paste0(specified_files, ".txt"),
  class = "gtfs_metadata"
)

usethis::use_data(gtfs_metadata, internal = TRUE, overwrite = TRUE)
