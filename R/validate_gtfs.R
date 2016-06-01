# Purpose -----------------------------------------------------------------

# Functions to validate/assess data quality of a gtfs class object
# These should collectively allow a user to
# - assess whether raw data from a feed has quality problems such that a gtfs class object can/should not be created
# - get a summary of data quality for a gtfs class object with various categories/levels of importance
# - save data quality metadata in a structured way

#' For a single feed list object, check provided files
#'
#' @param feed A GTFS feed list object with components agency_df, etc.
#'
#' @return Dataframe will one row for all required and optional files per spec, plus one row for any other files provided (file),
#'         with an indication of these categories (spec), and a yes/no/empty status (provided_status)

validate_files_provided <- function(feed) {

  # Per spec, these are the required and optional files
  all_req_files <- c('agency', 'stops', 'routes', 'trips', 'stop_times', 'calendar')
  all_opt_files <- c('calendar_dates', 'fare_attributes', 'fare_rules', 'shapes', 'frequencies', 'transfers', 'feed_info')
  all_spec_files <- c(all_req_files, all_opt_files)

  # Get the names of all the dfs in the list for a feed
  feed_names <- names(feed[[1]])

  # Strip the _df from the names to get to file components
  feed_names_file <- gsub('_df', '', feed_names)

  # Determine whether any of the files provided are neither required nor optional
  extra_files <- feed_names_file[!(feed_names_file %in% all_spec_files)]

  all_files <- c(all_spec_files, extra_files)

  prov_df <- data_frame(file = all_files, spec = c(rep('req', times = length(all_req_files)),
                                                   rep('opt', times = length(all_opt_files)),
                                                   rep('ext', times = length(extra_files))))

  prov_df <- prov_df %>%
    mutate(provided_status = ifelse(!(file %in% feed_names_file), 'no',
                                    ifelse(sapply(feed[[1]], dim)[2,] == 0, 'empty',
                                           'yes')))
  return(prov_df)

}

#' Create dataframe of GTFS variable spec info
#'
#' @export

make_var_val <- function() {

  # agency
  agency_vars <- c('agency_id', 'agency_name', 'agency_url', 'agency_timezone', 'agency_lang', 'agency_phone', 'agency_fare_url', 'agency_email')
  agency_vars_spec <- c('opt', 'req', 'req', 'req', 'opt', 'opt', 'opt', 'opt')
  agency_coltype <- rep('c', 8)
  agency_df <- data_frame(field = agency_vars, spec = agency_vars_spec)
  agency_df$file <- 'agency'

  # stops
  stops_vars <- c('stop_id', 'stop_code', 'stop_name', 'stop_desc', 'stop_lat', 'stop_lon', 'zone_id', 'stop_url', 'location_type', 'parent_station', 'stop_timezone', 'wheelchair_boarding')
  stops_vars_spec <- c('req', 'opt', 'req', 'opt', 'req', 'req', 'opt', 'opt', 'opt', 'opt', 'opt', 'opt')
  stops_coltype <- rep('c', length(stops_vars))
  stops_coltype[which(stops_vars %in% c('stop_lat', 'stop_lon'))] <- 'd' # double
  stops_coltype[which(stops_vars %in% c('location_type', 'parent_station', 'wheelchair_boarding'))] <- 'i' #integers
  
  stops_df <- data_frame(field = stops_vars, spec = stops_vars_spec)
  stops_df$file <- 'stops'

  # routes
  routes_vars <- c('route_id', 'agency_id', 'route_short_name', 'route_long_name', 'route_desc', 'route_type', 'route_url', 'route_color', 'route_text_color')
  routes_vars_spec <- c('req', 'opt', 'req', 'req', 'opt', 'req', 'opt', 'opt', 'opt')
  routes_df <- data_frame(field = routes_vars, spec = routes_vars_spec)
  routes_df$file <- 'routes'

  # trips
  trips_vars <- c('route_id', 'service_id', 'trip_id', 'trip_headsign', 'trip_short_name', 'direction_id', 'block_id', 'shape_id', 'wheelchair_accessible', 'bikes_allowed')
  trips_vars_spec <- c('req', 'req', 'req', 'opt', 'opt', 'opt', 'opt', 'opt', 'opt', 'opt')
  trips_df <- data_frame(field = trips_vars, spec = trips_vars_spec)
  trips_df$file <- 'trips'

  # stop_times
  stop_times_vars <- c('trip_id', 'arrival_time', 'departure_time', 'stop_id', 'stop_sequence', 'stop_headsign', 'pickup_type', 'drop_off_type', 'shape_dist_traveled', 'timepoint')
  stop_times_vars_spec <- c('req', 'req', 'req', 'req', 'req', 'opt', 'opt', 'opt', 'opt', 'opt')
  stop_times_df <- data_frame(field = stop_times_vars, spec = stop_times_vars_spec)
  stop_times_df$file <- 'stop_times'

  # calendar
  calendar_vars <- c('service_id', 'monday', 'tuesday', 'wednesday', 'thursday', 'friday', 'saturday', 'sunday', 'start_date', 'end_date')
  calendar_vars_spec <- rep('req', times = 10)
  calendar_df <- data_frame(field = calendar_vars, spec = calendar_vars_spec)
  calendar_df$file <- 'calendar'

  # calendar_dates
  calendar_dates_vars <- c('service_id', 'date', 'exception_type')
  calendar_dates_vars_spec <- c('req', 'req', 'req')
  calendar_dates_df <- data_frame(field = calendar_dates_vars, spec = calendar_dates_vars_spec)
  calendar_dates_df$file <- 'calendar_dates'

  # fare_attributes
  fare_attributes_vars <- c('fare_id', 'price', 'currency_type', 'payment_method', 'transfers', 'transfer_duration')
  fare_attributes_vars_spec <- c('req', 'req', 'req', 'req', 'req', 'opt')
  fare_attributes_df <- data_frame(field = fare_attributes_vars, spec = fare_attributes_vars_spec)
  fare_attributes_df$file <- 'fare_attributes'

  # fare_rules
  fare_rules_vars <- c('fare_id', 'route_id', 'origin_id', 'destination_id', 'contains_id')
  fare_rules_vars_spec <- c('req', 'opt', 'opt', 'opt', 'opt')
  fare_rules_df <- data_frame(field = fare_rules_vars, spec = fare_rules_vars_spec)
  fare_rules_df$file <- 'fare_rules'

  # shapes
  shapes_vars <- c('shape_id', 'shape_pt_lat', 'shape_pt_lon', 'shape_pt_sequence', 'shape_dist_traveled')
  shapes_vars_spec <- c('req', 'req', 'req', 'req', 'opt')
  shapes_df <- data_frame(field = shapes_vars, spec = shapes_vars_spec)
  shapes_df$file <- 'shapes'

  # frequencies
  frequencies_vars <- c('trip_id', 'start_time', 'end_time', 'headway_sec', 'exact_times')
  frequencies_vars_spec <- c('req', 'req', 'req', 'req', 'opt')
  frequencies_df <- data_frame(field = frequencies_vars, spec = frequencies_vars_spec)
  frequencies_df$file <- 'frequencies'

  # transfers
  transfers_vars <- c('from_stop_id', 'to_stop_id', 'transfer_type', 'min_transfer_time')
  transfers_vars_spec <- c('req', 'req', 'req', 'opt')
  transfers_df <- data_frame(field = transfers_vars, spec = transfers_vars_spec)
  transfers_df$file <- 'transfers'

  # feed_info
  feed_info_vars <- c('feed_publisher_name', 'feed_publisher_url', 'feed_lang', 'feed_start_date', 'feed_end_date', 'feed_version')
  feed_info_vars_spec <- c('req', 'req', 'req', 'opt', 'opt', 'opt')
  feed_info_df <- data_frame(field = feed_info_vars, spec = feed_info_vars_spec)
  feed_info_df$file <- 'feed_info'

  all_df <- dplyr::bind_rows(agency_df, stops_df, routes_df, trips_df, stop_times_df, calendar_df, calendar_dates_df,
                      fare_attributes_df, fare_rules_df, shapes_df, frequencies_df, transfers_df, feed_info_df)

  return(all_df[, c('file', 'field', 'spec')])

}

#' Validate variables provided vs spec
#'
#' @param val_files The dataframe output of validate_files_provided for a single feed
#' @param feed A GTFS feed list object with components agency_df, etc.
#'
#' @return Dataframe with one record per file x column (columns in spec + any extra provided),
#'         with file and field specs and file and field provided statuses
#'
#' @export

validate_vars_provided <- function(val_files, feed) {

  # Generate the df of files and fields per the GTFS spec
  spec_vars_df <- make_var_val() %>%
    rename(field_spec = spec)

  # Keep just the files that are provided for this feed, or that are required
  val_files_df <- val_files %>%
    rename(file_spec = spec, file_provided_status = provided_status) %>%
    filter(file_provided_status == 'yes' | file_spec == 'req')


  # Separately store any extra files and get their variables (since these aren't in spec_vars_df)
  extra_files_df <- val_files_df %>%
    filter(file_spec == 'ext')

  # For extra files provided, get all file names and add them into spec_vars_df
  if (nrow(extra_files_df) > 0) {
    extra_files <- extra_files_df$file
    extra_files_df <- paste0(extra_files, '_df')

    extra_vars_df <- dplyr::data_frame()

    for (i in extra_files_df) {

      temp_df <- feed[[i]]
      temp_names <- names(temp_df)

      temp_vars_df <- data_frame(file = rep(gsub('_df', '', i), length(temp_names)), field = temp_names, field_spec = rep('ext', length(temp_names)))

      extra_vars_df <- dplyr::bind_rows(extra_vars_df, temp_vars_df)

    }

    spec_vars_df <- dplyr::bind_rows(spec_vars_df, extra_vars_df)

  }

  # Join file level data with variable level data - for files that were provided
  vars_df <- left_join(val_files_df, spec_vars_df)

  val_vars_df <- dplyr::data_frame()

  val_files <- val_files_df$file
  val_files_df <- paste0(val_files, '_df')

  # List variables provided for each file and join to determine field_provided
  for (j in val_files_df) {

    temp_df <- feed[[j]]
    temp_names <- names(temp_df)

    # Handle the case where a required file is missing
    if (is.null(temp_df) || is.null(temp_names) || all(is.na(temp_names))) {
      temp_vars_df <- data_frame(file = gsub('_df', '', j), field = NA, field_provided_status = 'none')
    } else {
      provided_status <- temp_df %>%
        summarize_each(funs(is_empty = all(is.na(.))))

      temp_vars_df <- data_frame(file = rep(gsub('_df', '', j), length(temp_names)), field = temp_names, field_provided_status = ifelse(provided_status, 'empty', 'yes'))
    }


    val_vars_df <- dplyr::bind_rows(val_vars_df, temp_vars_df)

  }

  # Join observed field provided status with spec info
  all_df <- full_join(vars_df, val_vars_df)

  orig_rows <- nrow(all_df)

  # Now fill in special cases from join results

  # 1) Extra fields provided in spec files
  sub_all_df <- all_df %>%
    filter(!is.na(field_spec))

  case1_df <- all_df %>%
    filter(is.na(field_spec))

  case1_df <- case1_df %>%
    mutate(field_spec = 'ext') %>%
    group_by(file) %>%
    mutate(file_spec = unique(sub_all_df$file_spec[sub_all_df$file == unique(file)]),
           file_provided_status = unique(sub_all_df$file_provided_status[sub_all_df$file == unique(file)]))


  # 2) field_provided_status is NA for spec fields not provided in files provided
  sub_all_df$field_provided_status[is.na(sub_all_df$field_provided_status)] <- 'no'

  # Put filled in parts back together
  all_df <- dplyr::bind_rows(sub_all_df, case1_df)
  if (nrow(all_df) != orig_rows) stop('Handling of special cases failed.')

  # Check overall status for required files/fields
  all_df <- all_df %>%
    mutate(validation_status = ifelse(file_spec == 'req' & file_provided_status != 'yes',
                                      'file_missing', ifelse(field_spec == 'req' & field_provided_status != 'yes',
                                                             'field_missing', 'ok')))

  return(all_df)

}

#' Create validation list for a feed
#'
#' @param feed A GTFS feed list object with components agency_df, etc.
#' @param return_feed If TRUE, returns feed list with validate appended (true by default),
#'                    if FALSE, returns validate list only
#'
#' @return A feed list object with $validate added to the end of the list, or just validate list
#'
#' @export

validate_feed <- function(feed, return_feed = TRUE) {

  if (length(feed) == 0) {
    warning('Empty feed.')
    return()
  }

  print(feed$agency_df$agency_name)

  prov_df <- validate_files_provided(feed = feed)

  all_df <- validate_vars_provided(prov_df, feed = feed)

  validate_list <- list(all_req_files = !('file_missing' %in% all_df$validation_status),
                        all_req_fields = !('field_missing' %in% all_df$validation_status),
                        validate_df = all_df)

  feed <- c(feed, validate = list(validate_list))

  if (return_feed) {
    return(feed)
  } else {
    return(validate_list)
  }


}

