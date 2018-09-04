validate_gtfs <- function(gtfs_obj) {
  validate_gtfs_structure(gtfs_obj$files_validation_result, gtfs_obj)
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
#' @keywords internal

validate_gtfs_structure <- function(files_validation_result, gtfs_obj, return_gtfs_obj = TRUE, quiet = FALSE) {
  if (length(gtfs_obj) == 0) {
    warning('Empty gtfs_obj.')
    return(NULL)
  }

  if(!quiet) message(gtfs_obj$agency_df$agency_name)

  all_val_df <- validate_vars(files_validation_result, gtfs_obj = gtfs_obj) %>%
    calendar_exception_fix

  probs_subset <- all_val_df %>% dplyr::filter(validation_status == 'problem') # subset of only problems

  ok_subset <- all_val_df %>% dplyr::filter(validation_status != 'problem') # subset of ok values

  validate_list <- list(all_req_files = !('missing_req_file' %in% probs_subset$validation_details),
                        all_req_fields_in_req_files = !('missing_req_field' %in% probs_subset$validation_details),
                        all_req_fields_in_opt_files = !('missing_req_field' %in% ok_subset$validation_details),
                        full_column_and_file_validation_df = all_val_df)

  # get subset of problem req files
  if(!validate_list$all_req_files) {
      validate_list$problem_req_files <- probs_subset %>%
        dplyr::filter(grepl('missing_req_*', validation_details))%>%
        dplyr::select(file, file_spec, file_provided_status, field, field_spec, field_provided_status)
  }

  # get subset of problem opt files
  if(any(!validate_list$all_req_fields_in_req_files, !validate_list$all_req_fields_in_opt_files)) {
      validate_list$problem_opt_files <- ok_subset %>%
        dplyr::filter(grepl('missing_opt_*', validation_details)) %>%
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

is_files_validation_result <- function(files_validation_result) {
  cnames <- colnames(files_validation_result)
  return(
    length(cnames) == 3 &
      "file" %in% cnames &
      "spec" %in% cnames &
      "provided_status" %in% cnames &
      is.logical(files_validation_result$provided_status)
    ) 
}

#' For a list of files, return information about whether they are part of the GTFS spec.
#'
#' @param file_list A list of files to be checked
#'
#' @return Dataframe will one row for all required and optional files per spec, 
#'         plus one row for any other files provided (file), 
#'         with an indication of these categories (spec), 
#'         and a wheter the file is provided (provided)
#' @noRd

validate_file_list <- function(file_list) {
  
  file_list <- sapply(file_list,get_file_shortname)
  
  # Per spec, these are the required and optional files
  all_req_files <- c('agency', 'stops', 'routes', 'trips', 'stop_times', 'calendar')
  all_opt_files <- c(
    'calendar_attributes',
    'calendar_dates',
    'directions',
    'fare_attributes',
    'fare_rider_categories',
    'fare_rules',
    'farezone_attributes',
    'feed_info',
    'frequencies',
    'rider_categories',
    'route_directions',
    'shapes',
    'stop_attributes',
    'timetable_stop_order',
    'timetables',
    'transfers'
  )
  all_spec_files <- c(all_req_files, all_opt_files)

  # Get the names of all the dfs in the list for a gtfs_obj
  feed_names_file <- unname(file_list)
  
  # Determine whether any of the files provided are neither required nor optional
  extra_files <- feed_names_file[!(feed_names_file %in% all_spec_files)]

  all_files <- c(all_spec_files, extra_files)

  files_validation_result <- dplyr::data_frame(file = all_files, 
                                 spec = c(rep('req', times = length(all_req_files)), 
                                 rep('opt', times = length(all_opt_files)), 
                                 rep('ext', times = length(extra_files))))

  files_validation_result <- files_validation_result %>%
    dplyr::mutate(provided_status = (file %in% feed_names_file))
  
  return(files_validation_result)
}

valid_file_paths <- function(files_list) {
  files_list_shortnames <- sapply(files_list,get_file_shortname)
  files_validation_result <- validate_file_list(files_list)
  valid_files_meta <- files_validation_result %>% 
    dplyr::filter(spec != 'ext' & provided_status)
  
  valid_filenames <- names(files_list[files_list %in% valid_files_meta$file])

  return(valid_filenames)
}

#' Create dataframe of GTFS variable spec info
#' @noRd

make_var_val <- function() {

  nms <- get_gtfs_meta() %>% names() # get names of each envir element

  all_val_df <- get_gtfs_meta() %>%
    lapply(. %>% as.data.frame(stringsAsFactors=FALSE) %>% dplyr::tbl_df(.)) # convert list data to tbl_df

  all_val_df <- mapply(function(x,y) dplyr::mutate(.data=x, file = y), x = all_val_df, y = nms, SIMPLIFY=FALSE) %>%
    dplyr::bind_rows() # assign new variable 'file' to each tbl_df based on names

  return(all_val_df)

}

#' Validate variables provided vs spec
#'
#' @param files_validation_result The dataframe output of validate_file_list for a single feed
#' @param gtfs_obj A 'gtfs' class object with components agency_df, etc.
#'
#' @return Dataframe with one record per file x column (columns in spec + any extra provided),
#'         with file and field specs and file and field provided statuses
#'
#' @noRd

validate_vars <- function(files_validation_result, gtfs_obj) {

  stopifnot(any(class(files_validation_result) == 'tbl_df'),
    is_files_validation_result(files_validation_result))

  # files_validation_result <- gtfs_obj$files_validation_result
  gtfs_obj <- gtfs_obj[which(names(gtfs_obj) != "files_validation_result")]
  
  t# Generate the df of files and fields per the GTFS spec
  spec_vars_df <- make_var_val() %>%
    dplyr::rename(field_spec = spec)

  # the files that are provided for this gtfs_obj
  val_files_df <- files_validation_result %>%
    dplyr::rename(file_spec = spec, file_provided_status = provided_status)

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
      temp_vars_df <- dplyr::data_frame(file = gsub('_df', '', j), 
                                        field = NA, 
                                        field_provided_status = 'none')
    } else {
      provided_status <- temp_df %>%
        dplyr::summarize_all(dplyr::funs(is_empty = all(is.na(.))))

      temp_vars_df <- dplyr::data_frame(file = rep(gsub('_df', '', j), 
                                                   length(temp_names)), 
                                        field = temp_names, 
                                        field_provided_status = !provided_status)
    }

    val_vars_df <- dplyr::bind_rows(val_vars_df, temp_vars_df)

  }

  # Join observed field provided status with spec info
  all_val_df <- suppressMessages(dplyr::full_join(vars_df, val_vars_df))

  orig_rows <- nrow(all_val_df)

  # Now fill in special cases from join results

  # 1) Extra fields provided in spec files
  sub_all_val_df <- all_val_df %>%
    dplyr::filter(!is.na(field_spec))

  case_df <- all_val_df %>%
    dplyr::filter(is.na(field_spec))

  case_df <- case_df %>%
    dplyr::mutate(field_spec = 'ext') %>%
    dplyr::group_by(file) %>%
    dplyr::mutate(file_spec = unique(sub_all_val_df$file_spec[sub_all_val_df$file == unique(file)]),
           file_provided_status = unique(sub_all_val_df$file_provided_status[sub_all_val_df$file == unique(file)]))


  # 2) field_provided_status is NA for spec fields not provided in files provided
  sub_all_val_df$field_provided_status[is.na(sub_all_val_df$field_provided_status)] <- 'no'

  # Put filled in parts back together
  all_val_df <- dplyr::bind_rows(sub_all_val_df, case_df)
  if (nrow(all_val_df) != orig_rows) stop('Handling of special cases failed.')

  # Check overall status for required files/fields
  all_val_df <- all_val_df %>%
    dplyr::mutate(validation_status = 'ok') # default ok

  all_val_df <- all_val_df %>%
    dplyr::mutate(validation_details = NA) %>% # default to NA
    dplyr::mutate(validation_details = replace(validation_details, !file_provided_status & file_spec == 'opt', 'missing_opt_file')) %>% # optional file missing
    dplyr::mutate(validation_details = replace(validation_details, !file_provided_status & file_spec == 'req', 'missing_req_file')) %>% # req file missing
    dplyr::mutate(validation_details = replace(validation_details, file_provided_status & field_spec == 'req' & field_provided_status != 'yes', 'missing_req_field')) %>%
    dplyr::mutate(validation_details = replace(validation_details, file_provided_status & field_spec == 'opt' & field_provided_status != 'yes', 'missing_opt_field'))


  all_val_df <- all_val_df %>%
    dplyr::mutate(validation_status = replace(validation_status, grepl('req_file', validation_details), 'problem'))


  return(all_val_df)
}

#' checks for the missing calendar.txt 
#' exception (see https://developers.google.com/transit/gtfs/reference/calendar_dates-file). 
#' if the exception is TRUE, then calendar has its file spec set to 'extra'.
#' @param all_val_df dataframe of all files and fields. returned from validate_vars_provided()
#' @return corrected dataframe.
#' @noRd

calendar_exception_fix <- function(all_val_df) {

  # see if missing calendar
  missing_calendar <- all_val_df %>%
    dplyr::filter(file == 'calendar') %>%
    dplyr::pull('file_provided_status') %>%
    unique()
  
  missing_calendar <- c('no') %in% missing_calendar

  # see if calendar_dates file includes field `date`
  has_date_field <- all_val_df %>%
    dplyr::filter(file == 'calendar_dates') %>%
    dplyr::filter(field == 'date') %>%
    dplyr::pull('field_provided_status')

  has_date_field <- c('yes') %in% missing_calendar

  # if missing calendar.txt but calendar_dates.txt has 'date' field, then update validation status of calendar to 'ok'

  if(all(missing_calendar, has_date_field)) {

    all_val_df <- all_val_df %>%
      dplyr::mutate(validation_status = dplyr::if_else(file == "calendar", "ok", validation_status)) %>%
      dplyr::mutate(file_spec = dplyr::if_else(file == "calendar", "ext", file_spec))
  }

  return(all_val_df)

}

validate_feed <- function(files_validation_result, gtfs_list, quiet = FALSE) {
  # check if valid 'gtfs'
  check <- validate_gtfs_structure(files_validation_result, gtfs_list, return_gtfs_obj = FALSE, quiet = TRUE)
  valid <- all(check$all_req_files, check$all_req_fields_in_req_files)
  
  if(!quiet) message("Testing data structure...")
  if(valid) {
    class(gtfs_list) <- 'gtfs'
    if(!quiet) message("...passed. Valid GTFS object.\n")
  } else {
    if(!quiet) message("...failed. Invalid data structure.\n")
  }
  gtfs_list$validation <- check 
  return(gtfs_list)
}
