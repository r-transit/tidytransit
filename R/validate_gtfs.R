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
  feed_names <- names(feed)

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
                                    ifelse(sapply(feed, dim)[2,] == 0, 'empty',
                                           'yes')))
  return(prov_df)

}

#' Create dataframe of GTFS variable spec info
#'
#' @export

make_var_val <- function() {

  for(n in ls(get_gtfs_meta())) {
    x <- paste0(n, '_df')
    df <- as.data.frame(get(n, envir = get_gtfs_meta()), stringsAsFactors=FALSE) %>% tbl_df
    df$file <- n
    assign(x, df)
  }

  all_df <- dplyr::bind_rows(agency_df, stops_df, routes_df, trips_df, stop_times_df, calendar_df, calendar_dates_df,
                      fare_attributes_df, fare_rules_df, shapes_df, frequencies_df, transfers_df, feed_info_df)

  return(all_df)

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
  vars_df <- suppressMessages(left_join(val_files_df, spec_vars_df))

  val_vars_df <- dplyr::data_frame()

  val_cols <- val_files_df$file
  val_cols_df <- paste0(val_cols, '_df')

  # List variables provided for each file and join to determine field_provided
  for (j in val_cols_df) {

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
  all_df <- suppressMessages(full_join(vars_df, val_vars_df))

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
    return(NULL)
  }

  print(feed$agency_df$agency_name)

  prov_df <- validate_files_provided(feed = feed)

  all_df <- validate_vars_provided(prov_df, feed = feed)

  validate_list <- list(all_req_files = !('file_missing' %in% all_df$validation_status),
                        all_req_fields = !('field_missing' %in% all_df$validation_status),
                        validate_df = all_df)

  # update feed attributes with validation data
  attributes(feed) <- append(attributes(feed), list(validate = validate_list))

  if (return_feed) {
    return(feed)
  } else {
    return(validate_list)
  }


}

