# Purpose -----------------------------------------------------------------

# Functions to validate/assess data quality of a gtfs class object
# These should collectively allow a user to
# - assess whether raw data from a feed has quality problems such that a gtfs class object can/should not be created
# - get a summary of data quality for a gtfs class object with various categories/levels of importance
# - save data quality metadata in a structured way

#' For a single 'gtfs' class object, check provided files
#'
#' @param gtfs_obj A 'gtfs' class object with components agency_df, etc.
#'
#' @return Dataframe will one row for all required and optional files per spec, plus one row for any other files provided (file), with an indication of these categories (spec), and a yes/no/empty status (provided_status)
#' @export

validate_files_provided <- function(gtfs_obj) {

  stopifnot(class(gtfs_obj) == 'gtfs')

  # Per spec, these are the required and optional files
  all_req_files <- c('agency', 'stops', 'routes', 'trips', 'stop_times', 'calendar')
  all_opt_files <- c('calendar_dates', 'fare_attributes', 'fare_rules', 'shapes', 'frequencies', 'transfers', 'feed_info', 'timetables', 'timetable_stop_order', 'route_directions')
  all_spec_files <- c(all_req_files, all_opt_files)

  # Get the names of all the dfs in the list for a gtfs_obj
  feed_names <- names(gtfs_obj)

  # Strip the _df from the names to get to file components
  feed_names_file <- gsub('_df', '', feed_names)

  # Determine whether any of the files provided are neither required nor optional
  extra_files <- feed_names_file[!(feed_names_file %in% all_spec_files)]

  all_files <- c(all_spec_files, extra_files)

  prov_df <- dplyr::data_frame(file = all_files, spec = c(rep('req', times = length(all_req_files)), rep('opt', times = length(all_opt_files)), rep('ext', times = length(extra_files))))

  prov_df <- prov_df %>%
    dplyr::mutate(provided_status = ifelse(!(file %in% feed_names_file), 'no',
                                    ifelse(sapply(gtfs_obj, dim)[2,] == 0, 'empty',
                                           'yes')))
  return(prov_df)

}

#' Create dataframe of GTFS variable spec info
#' @noRd

make_var_val <- function() {

  nms <- get_gtfs_meta() %>% names # get names of each envir element

  all_df <- get_gtfs_meta() %>%
    lapply(. %>% as.data.frame(stringsAsFactors=FALSE) %>% dplyr::tbl_df(.)) # convert list data to tbl_df

  all_df <- mapply(function(x,y) dplyr::mutate(.data=x, file = y), x = all_df, y = nms, SIMPLIFY=FALSE) %>%
    dplyr::bind_rows() # assign new variable 'file' to each tbl_df based on names

  return(all_df)

}

#' Validate variables provided vs spec
#'
#' @param val_files The dataframe output of validate_files_provided for a single feed
#' @param gtfs_obj A 'gtfs' class object with components agency_df, etc.
#'
#' @return Dataframe with one record per file x column (columns in spec + any extra provided),
#'         with file and field specs and file and field provided statuses
#'
#' @export

validate_vars_provided <- function(val_files, gtfs_obj) {

  stopifnot(class(gtfs_obj) == 'gtfs', any(class(val_files) == 'tbl_df'))

  # Generate the df of files and fields per the GTFS spec
  spec_vars_df <- make_var_val() %>%
    dplyr::rename(field_spec = spec)

  # the files that are provided for this gtfs_obj
  val_files_df <- val_files %>%
    dplyr::rename(file_spec = spec, file_provided_status = provided_status)


  # Separately store any extra files and get their variables (since these aren't in spec_vars_df)
  extra_files_df <- val_files_df %>% dplyr::filter(file_spec == 'ext')

  # For extra files provided, get all file names and add them into spec_vars_df
  if (nrow(extra_files_df) > 0) {
    extra_files <- extra_files_df$file
    extra_files_df <- paste0(extra_files, '_df')

    extra_vars_df <- dplyr::data_frame()

    for (i in extra_files_df) {

      temp_df <- gtfs_obj[[i]]
      temp_names <- names(temp_df)

      # create dataframe of names
      temp_vars_df <- dplyr::data_frame(file = rep(gsub('_df', '', i), length(temp_names)), field = temp_names, field_spec = 'ext')

      extra_vars_df <- dplyr::bind_rows(extra_vars_df, temp_vars_df)

    }

    spec_vars_df <- dplyr::bind_rows(spec_vars_df, extra_vars_df)

  }

  spec_vars_df <- spec_vars_df %>% dplyr::select(field, field_spec, file)

  # Join file level data with variable level data - for files that were provided
  vars_df <- suppressMessages(dplyr::left_join(val_files_df, spec_vars_df))

  val_vars_df <- dplyr::data_frame()

  val_cols <- val_files_df$file
  val_cols_df <- paste0(val_cols, '_df')

  # List variables provided for each file and join to determine field_provided
  for (j in val_cols_df) {

    temp_df <- gtfs_obj[[j]]
    temp_names <- names(temp_df)

    # Handle the case where a required file is missing
    if (is.null(temp_df) || is.null(temp_names) || all(is.na(temp_names))) {
      temp_vars_df <- dplyr::data_frame(file = gsub('_df', '', j), field = NA, field_provided_status = 'none')
    } else {
      provided_status <- temp_df %>%
        dplyr::summarize_each(dplyr::funs(is_empty = all(is.na(.))))

      temp_vars_df <- dplyr::data_frame(file = rep(gsub('_df', '', j), length(temp_names)), field = temp_names, field_provided_status = ifelse(provided_status, 'empty', 'yes'))
    }

    val_vars_df <- dplyr::bind_rows(val_vars_df, temp_vars_df)

  }

  # Join observed field provided status with spec info
  all_df <- suppressMessages(dplyr::full_join(vars_df, val_vars_df))

  orig_rows <- nrow(all_df)

  # Now fill in special cases from join results

  # 1) Extra fields provided in spec files
  sub_all_df <- all_df %>%
    dplyr::filter(!is.na(field_spec))

  case1_df <- all_df %>%
    dplyr::filter(is.na(field_spec))

  case1_df <- case1_df %>%
    dplyr::mutate(field_spec = 'ext') %>%
    dplyr::group_by(file) %>%
    dplyr::mutate(file_spec = unique(sub_all_df$file_spec[sub_all_df$file == unique(file)]),
           file_provided_status = unique(sub_all_df$file_provided_status[sub_all_df$file == unique(file)]))


  # 2) field_provided_status is NA for spec fields not provided in files provided
  sub_all_df$field_provided_status[is.na(sub_all_df$field_provided_status)] <- 'no'

  # Put filled in parts back together
  all_df <- dplyr::bind_rows(sub_all_df, case1_df)
  if (nrow(all_df) != orig_rows) stop('Handling of special cases failed.')

  # Check overall status for required files/fields
  all_df <- all_df %>%
    dplyr::mutate(validation_status = 'ok') # default ok

  all_df <- all_df %>%
    dplyr::mutate(validation_details = NA) %>% # default to NA
    dplyr::mutate(validation_details = replace(validation_details, file_provided_status != 'yes' & file_spec == 'opt', 'missing_opt_file')) %>% # optional file missing
    dplyr::mutate(validation_details = replace(validation_details, file_provided_status != 'yes' & file_spec == 'req', 'missing_req_file')) %>% # req file missing
    dplyr::mutate(validation_details = replace(validation_details, file_provided_status == 'yes' & field_spec == 'req' & field_provided_status != 'yes', 'missing_req_field')) %>%
    dplyr::mutate(validation_details = replace(validation_details, file_provided_status == 'yes' & field_spec == 'opt' & field_provided_status != 'yes', 'missing_opt_field'))


  all_df <- all_df %>%
    dplyr::mutate(validation_status = replace(validation_status, grepl('req_files', validation_details), 'problem'))


  return(all_df)

}

#' Create validation list for a gtfs_obj. It provides an overview of the structure of all files that were imported.
#'
#' @param gtfs_obj A GTFS list object with components agency_df, etc.
#' @param return_gtfs_obj Boolean. If TRUE, returns gtfs_obj list with a 'validate' attribute appended (TRUE by default),
#'                    if FALSE, returns validate list only
#' @param quiet Boolean. Option to suppress any messages, prints, etc
#'
#' @return A gtfs_obj list object with attribute 'validate' or just a list containing validation data
#'
#' @export

validate_gtfs_structure <- function(gtfs_obj, return_gtfs_obj = TRUE, quiet = FALSE) {

  if (length(gtfs_obj) == 0) {
    warning('Empty gtfs_obj.')
    return(NULL)
  }

  if(!quiet) message(gtfs_obj$agency_df$agency_name)

  prov_df <- validate_files_provided(gtfs_obj = gtfs_obj)

  all_df <- validate_vars_provided(prov_df, gtfs_obj = gtfs_obj)

  probs_subset <- all_df %>% dplyr::filter(validation_status == 'problem') # subset of only problems

  ok_subset <- all_df %>% dplyr::filter(validation_status != 'problem') # subset of ok values

  validate_list <- list(all_req_files = !('missing_req_file' %in% probs_subset$validation_details),
                        all_req_fields_in_req_files = !('missing_req_field' %in% probs_subset$validation_details),
                        all_req_fields_in_opt_files = !('missing_req_field' %in% ok_subset$validation_details),
                        validate_df = all_df)

  # get subset of problem req files
  if(!validate_list$all_req_files) {
      validate_list$problem_req_files <- prob_subset %>%
        dplyr::filter(grepl('missing_req_*', validation_details))%>%
        dplyr::select(file, file_spec, file_provided_status, field, field_spec, field_provided_status)
  }

  # get subset of problem req files
  if(any(!validate_list$all_req_fields_in_req_files, !validate_list$all_req_fields_in_opt_files)) {
      validate_list$problem_opt_files <- ok_subset %>%
        dplyr::filter(grepl('missing_req_*', validation_details)) %>%
        dplyr::select(file, file_spec, file_provided_status, field, field_spec, field_provided_status)
  }

  # get subset of extra files
  if('ext' %in% all_df$file_spec) {
      validate_list$extra_files <- all_df %>%
        dplyr::filter(grepl('ext', file_spec)) %>%
        dplyr::select(file, file_spec, file_provided_status, field, field_spec, field_provided_status)
  }

  # update gtfs_obj attributes with validation data
  attributes(gtfs_obj) <- append(attributes(gtfs_obj), list(validate = validate_list))

  if (return_gtfs_obj) {
    return(gtfs_obj)
  } else {
    return(validate_list)
  }


}

