#' Validation table that defines for every data frame within gtfs_obj
#' whether the file is required ('req'), optional ('opt') or an extra 
#' file ('ext'). The same is applied for each field. The validation_status
#' column points problems with files/fiels. 
validate_gtfs <- function(gtfs_obj) {

  validation_result <- validate_gtfs_structure(gtfs_obj)
  
  # check existing files and fields
  validation_result$validation_status <- 'ok'
  validation_result$validation_details <- NA
  
  validation_result <- validation_result %>% 
    dplyr::mutate(validation_details = NA) %>% # default to NA
    dplyr::mutate(validation_details = replace(validation_details, !file_provided_status & file_spec == 'req', 'missing_req_file')) %>% # req file missing
    dplyr::mutate(validation_details = replace(validation_details, !file_provided_status & file_spec == 'opt', 'missing_opt_file')) %>% # optional file missing
    dplyr::mutate(validation_details = replace(validation_details, file_provided_status & field_spec == 'req' & !field_provided_status, 'missing_req_field')) %>%
    dplyr::mutate(validation_details = replace(validation_details, file_provided_status & field_spec == 'opt' & !field_provided_status, 'missing_opt_field'))

  validation_result <- validation_result %>% 
    dplyr::mutate(validation_status = replace(validation_status, validation_details == 'missing_req_file', 'problem')) %>% 
    dplyr::mutate(validation_status = replace(validation_status, validation_details == 'missing_req_field', 'problem')) %>% 
    dplyr::mutate(validation_status = replace(validation_status, !is.na(validation_details), 'info'))
  
  # assign validation result to gtfs_obj
  attributes(gtfs_obj) <- append(attributes(gtfs_obj), list(validation_result = validation_result))
  
  return(gtfs_obj)
}

#' Create validation table for a gtfs_obj. It provides an overview of the structure of all files that were imported.
#'
#' @param gtfs_obj A GTFS list object with components agency_df, etc.
validate_gtfs_structure <- function(gtfs_obj) {

  meta <- get_gtfs_meta()
  structure <- tibble::tibble()
  
  # check files => data frames
  for(dfname in names(gtfs_obj)) {
    # substr instead of replacing _df in case of weird filenames
    df <- gtfs_obj[[dfname]]
    file <- substr(dfname, 1, nchar(dfname)-3)
    fmeta <- meta[[file]]
    
    # validate file
    file_provided_status <- F
    file_spec <- 'ext'
    if(!is.null(fmeta)) {
      file_provided_status <- T
      file_spec <- fmeta$file_spec
      
      # validate fields
      df_validation <- tibble::tibble(file, file_spec, file_provided_status, field = fmeta$field, field_spec = fmeta$field_spec, field_provided_status = F)
      for(colname in colnames(df)) {
        if(colname %in% fmeta$field) {
          df_validation <- df_validation %>% dplyr::mutate(field_provided_status = replace(field_provided_status, field == colname, T))
        } else {
          df_validation <- df_validation %>% add_row(file, file_spec, file_provided_status, field = colname, field_spec = 'ext', field_provided_status = T)
        }
      }
    } else {
      # extra file
      cnames <- colnames(df)
      if(is.null(cnames) | length(cnames) == 1) {
        cnames <- NA
      }
      df_validation <- tibble::tibble(file, file_spec, file_provided_status, field = cnames, field_spec = 'ext', field_provided_status = T)
    }
    structure <- rbind(structure, df_validation)
  }
  
  # checks for the missing calendar.txt exception, see https://developers.google.com/transit/gtfs/reference/#calendar_datestxt
  # if the exception is TRUE, then calendar has its file spec set to 'extra' and calendar_dates is required
  calendar_exists <- structure %>% filter(file == "calendar") %>% pull(file_provided_status) %>% all()
  if(!calendar_exists) {
    structure <- structure %>% 
      dplyr::mutate(file_spec = replace(file_spec, file == 'calendar', 'ext')) %>% 
      dplyr::mutate(file_spec = replace(file_spec, file == 'calendar_dates', 'req'))
  }
  
  return(structure)
}

#' Basic check if a given list is a gtfs object 
is_gtfs_obj <- function(gtfs_obj) {
  obj_attributes <- attributes(gtfs_obj)
  return(
    class(gtfs_obj) == "gtfs" &
    !is.null(obj_attributes$names) &
    !is.null(obj_attributes$validation_result)
  )
}
