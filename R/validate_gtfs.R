#' Validate GTFS file
#'
#' Validates the GTFS object against GTFS specifications and raises warnings if
#' required files/fields are not found. This function is called in \code{\link{read_gtfs}}.
#' 
#' Note that this function just checks if required files or fields are missing. There's no
#' validation for internal consistency (e.g. no departure times before arrival times or 
#' calendar covering a reasonable period).
#'
#' @param gtfs_obj gtfs object (i.e. a list of tables, not necessary a tidygtfs object)
#' @param files A character vector containing the text files to be validated
#'   against the GTFS specification (without the \code{.txt} extension). If
#'   \code{NULL} (the default) the provided GTFS is validated against all
#'   possible GTFS text files.
#' @param warnings Whether to display warning messages (defaults to TRUE).
#'
#' @return A \code{validation_result} tibble containing the validation summary of all
#'   possible fields from the specified files.
#'
#' @section Details:
#' GTFS object's files and fields are validated against the GTFS specifications
#' as documented in \href{https://gtfs.org/documentation/schedule/reference/}{
#' GTFS Schedule Reference}:
#' \itemize{
#'   \item GTFS feeds are considered valid if they include all required files
#'     and fields. If a required file/field is missing the function (optionally)
#'     raises a warning.
#'   \item Optional files/fields are listed in the reference above but are not
#'     required, thus no warning is raised if they are missing.
#'   \item Extra files/fields are those who are not listed in the reference
#'     above (either because they refer to a specific GTFS extension or due to
#'     any other reason).
#' }
#' Note that some files (\code{calendar.txt}, \code{calendar_dates.txt} and
#' \code{feed_info.txt}) are conditionally required. This means that:
#' \itemize{
#'   \item \code{calendar.txt} is initially set as a required file. If it's not
#'     present, however, it becomes optional and \code{calendar_dates.txt}
#'     (originally set as optional) becomes required.
#'   \item \code{feed_info.txt} is initially set as an optional file. If
#'     \code{translations.txt} is present, however, it becomes required.
#' }
#'
#' @examples
#' validate_gtfs(gtfs_duke)
#' #> # A tibble: 233 × 8
#' #>    file   file_spec file_provided_status field  field_spec field_provided_status
#' #>    <chr>  <chr>     <lgl>                <chr>  <chr>      <lgl>                
#' #>  1 agency req       TRUE                 agenc… opt        TRUE                 
#' #>  2 agency req       TRUE                 agenc… req        TRUE                 
#' #>  3 agency req       TRUE                 agenc… req        TRUE                 
#' #>  4 agency req       TRUE                 agenc… req        TRUE                 
#' #>  5 agency req       TRUE                 agenc… opt        TRUE                 
#' #>  6 agency req       TRUE                 agenc… opt        TRUE                 
#' #>  7 agency req       TRUE                 agenc… opt        TRUE                 
#' #>  8 agency req       TRUE                 agenc… opt        FALSE                
#' #>  9 stops  req       TRUE                 stop_… req        TRUE                 
#' #> 10 stops  req       TRUE                 stop_… opt        TRUE                 
#' #> # 223 more rows
#' #> # 2 more variables: validation_status <chr>, validation_details <chr>
#' 
#' \dontrun{
#' local_gtfs_path <- system.file("extdata", "google_transit_nyc_subway.zip", package = "tidytransit")
#' gtfs <- read_gtfs(local_gtfs_path)
#' attr(gtfs, "validation_result")
#'
#' gtfs$shapes <- NULL
#' validation_result <- validate_gtfs(gtfs)
#'
#' # should raise a warning
#' gtfs$stop_times <- NULL
#' validation_result <- validate_gtfs(gtfs)
#' }
#' @export
validate_gtfs <- function(gtfs_obj, files = NULL, warnings = TRUE) {
  
  # input checking
  if(!inherits(gtfs_obj, "gtfs") & !inherits(gtfs_obj, "list")) {
    stop("gtfs_obj must be a gtfs or list object")
  }
  
  # if any files have been specified in read_gtfs, only validate those

  if(is.null(files)) {
    files_to_validate <- unique(c(names(gtfs_reference), names(gtfs_obj)))
  } else {
    if(length(setdiff(files, names(gtfs_obj))) > 0) {
      stop("File names not found in gtfs_obj: ", paste(setdiff(files, names(gtfs_obj)), collapse = ", "))
    }
    files_to_validate <- files
  }
  
  # don't validate geojson files
  files_to_validate <- files_to_validate[!is_geojson(files_to_validate)]
  
  if(length(files_to_validate) == 0) return(data.table::data.table(
    file = character(), file_spec = character(), file_provided_status = logical(),
    field = character(), field_spec = character(), field_provided_status = logical()
  ))

  # build validation dt for each file
  validation_result <- lapply(files_to_validate, function(file) {
    file_metadata <- gtfs_reference[[file]]

    # if metadata is null then file is undocumented. validate it as an "extra" file
    if(is.null(file_metadata)) {
      file_provided_status  <- TRUE
      file_spec             <- "Extra"
      field_spec            <- "Extra"
      field_provided_status <- TRUE
      field                 <- names(gtfs_obj[[file]])
      if(is.null(field)) field <- NA # file is not a dataframe
    } else {
      
      # undocumented fields are labeled as "extra" fields
      provided_fields   <- names(gtfs_obj[[file]])

      documented_fields <- file_metadata$fields$Field_Name
      
      file_provided_status  <- file %in% names(gtfs_obj)
      file_spec             <- file_metadata$File_Presence
      field                 <- c(
        documented_fields,
        setdiff(provided_fields, documented_fields)
      )
      field_presence = file_metadata$fields$Presence
      names(field_presence) <- file_metadata$fields$Field_Name
      field_spec            <- ifelse(
        field %in% documented_fields,
        field_presence[field],
        "Extra"
      )
      field_provided_status <- field %in% provided_fields
      
    }
    
    data.table::data.table(
      file,
      file_spec,
      file_provided_status,
      field,
      field_spec,
      field_provided_status
    )
  })

  validation_result <- data.table::rbindlist(validation_result)
  
  # checks if calendar.txt is missing. if it is then it becomes optional and
  # calendar_dates.txt becomes required

  if(!"calendar" %in% names(gtfs_obj)) {
    validation_result[file == "calendar", file_spec := "Optional"]
    validation_result[file == "calendar_dates", file_spec := "Required"]
  }
  
  # checks if translations.txt is provided. if it is, feed_info.txt becomes required
  if("translations" %in% names(gtfs_obj)) {
    validation_result[file == "feed_info", file_spec := "Required"]
  }
  
  # checks for validation status and details
  validation_result[, `:=`(validation_status = "ok", validation_details = NA_character_)]
  
  # if file is not provided and is required, mark as a problem
  validation_result[
    !file_provided_status & file_spec == "Required",
    `:=`(validation_status = "problem", validation_details = "missing_req_file")
  ]
  
  # if file is not provided and is optional, mark as a info
  validation_result[
    !file_provided_status & file_spec == "Optional",
    `:=`(validation_status = "info", validation_details = "missing_opt_file")
  ]
  
  # if file is provided but misses a required field, mark as a problem
  validation_result[
    file_provided_status & !field_provided_status & field_spec == "Required",
    `:=`(validation_status = "problem", validation_details = "missing_req_field")
  ]
  
  # if file is provided but misses a optional field, mark as a info
  validation_result[
    file_provided_status & !field_provided_status & field_spec == "Optional",
    `:=`(validation_status = "info", validation_details = "missing_opt_field")
  ]
  
  # if file is provided but undocumented in gtfs specifications, mark as a info
  validation_result[
    file_spec == "Extra",
    `:=`(validation_status = "info", validation_details = "undocumented_file")
  ]
  
  # if field is provided but undocumented in gtfs specifications, mark as a info
  validation_result[
    file_spec != "Extra" & field_spec == "Extra",
    `:=`(validation_status = "info", validation_details = "undocumented_field")
  ]
  
  # raises warnings if problems are found
  files_problems <- validation_result[validation_details == "missing_req_file"]
  
  if(nrow(files_problems) >= 1 & warnings) {
    
    warning(
      paste0(
        "Invalid feed. Missing required file(s): ",
        paste(paste0(unique(files_problems$file), ".txt"), collapse = ", ")
        # currently only txt files required
      )
    )
    
  }

  fields_problems <- validation_result[validation_details == "missing_req_field"]
  
  if(nrow(fields_problems) >= 1 & warnings) {
    problematic_files <- unique(fields_problems$file)
    
    problematic_fields <- unlist(
      lapply(
        problematic_files,
        function(i) paste(fields_problems[file == i]$field, collapse = ", ")
      )
    )
    
    warning(
      paste0(
        "Invalid feed. ",
        paste0(
          "Missing required field(s) in ", problematic_files, ": ", problematic_fields,
          collapse = ". "
        )
      )
    )
    
  }

  # attach validation_result as an attribute of the given gtfs
  dplyr::as_tibble(validation_result)
}

#' Check if primary keys are unique within tables
#' @param gtfs_list list of tables
#' @keywords internal
duplicated_primary_keys = function(gtfs_list) {
  stopifnot(inherits(gtfs_list, "list"))

  tbl_has_dupl_keys = rep(FALSE, length(gtfs_list))
  names(tbl_has_dupl_keys) <- names(gtfs_list)
  
  for(tbl_name in intersect(names(gtfs_list), names(gtfs_reference))) {
    id_fields = gtfs_reference[[tbl_name]]$primary_key
    
    if(!anyNA(id_fields)) {
      if(length(id_fields) == 1 && id_fields == "*") {
        id_fields <- colnames(gtfs_list[[tbl_name]])
      }
      
      # required fields have already been checked in validate_gtfs
      id_fields = intersect(colnames(gtfs_list[[tbl_name]]), id_fields)
      if(length(id_fields) > 0) {
        primary_key_table = as.data.table(gtfs_list[[tbl_name]])[,id_fields, with = FALSE]
        primary_key_table_dupl.index = anyDuplicated(primary_key_table)
        tbl_has_dupl_keys[tbl_name] <- any(primary_key_table_dupl.index != 0)
      }
    }
  }
  return(tbl_has_dupl_keys)
}
