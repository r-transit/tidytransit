#' Validate GTFS file
#'
#' Validates the GTFS object against GTFS specifications and raises warnings if
#' required files/fields are not found.
#'
#' @param gtfs A GTFS object as created by \code{\link{read_gtfs}}.
#' @param files A character vector containing the text files to be validated
#'   against the GTFS specification (without the \code{.txt} extension). If
#'   \code{NULL} (the default) the provided GTFS is validated against all
#'   possible GTFS text files.
#' @param quiet Whether to hide log messages (defaults to TRUE).
#' @param warnings Whether to display warning messages (defaults to TRUE).
#'
#' @return A GTFS object with a \code{validation_result} attribute. This
#'   attribute is a \code{data.table} containing the validation summary of all
#'   possible fields from the specified files.
#'
#' @section Details:
#' GTFS object's files and fields are validated against the GTFS specifications
#' as documented in \href{https://developers.google.com/transit/gtfs/reference}{
#' Google's Static GTFS Reference}:
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
#' data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
#'
#' gtfs <- read_gtfs(data_path)
#' attr(gtfs, "validation_result")
#'
#' gtfs$shapes <- NULL
#' validation_result <- validate_gtfs(gtfs)
#'
#' # should raise a warning
#' gtfs$stop_times <- NULL
#' validation_result <- validate_gtfs(gtfs)
#'
#' @export
validate_gtfs <- function(gtfs, files = NULL, quiet = TRUE, warnings = TRUE) {

  # input checking

  checkmate::assert_class(gtfs, "dt_gtfs")
  checkmate::assert_logical(quiet)
  checkmate::assert_logical(warnings)
  checkmate::assert_character(files, null.ok = TRUE)

  # if any files have been specified in read_gtfs, only validate those
  # uses internal data gtfs_metadata - check data-raw/gtfs_metadata.R

  if (is.null(files)) {

    specified_files   <- names(gtfs_metadata)
    extra_files       <- setdiff(paste0(names(gtfs), ".txt"), names(gtfs_metadata))
    files_to_validate <- c(specified_files, extra_files)

  } else {

    checkmate::assert_names(files, subset.of = names(gtfs))

    files_to_validate <- paste0(files, ".txt")

  }

  # build validation dt for each file

  validation_result <- lapply(files_to_validate, function(filename) {

    file_metadata <- gtfs_metadata[[filename]]
    file          <- sub(".txt", "", filename)

    # if metadata is null then file is undocumented. validate it as an "extra" file

    if (is.null(file_metadata)) {

      file_provided_status  <- TRUE
      file_spec             <- "ext"
      field                 <- names(gtfs[[file]])
      field_spec            <- "ext"
      field_provided_status <- TRUE

    } else {

      # undocumented fields are labeled as "extra" fields

      provided_fields   <- names(gtfs[[file]])
      documented_fields <- file_metadata$field

      file_provided_status  <- file %in% names(gtfs)
      file_spec             <- file_metadata$file_spec
      field                 <- c(
        documented_fields,
        setdiff(provided_fields, documented_fields)
      )
      field_spec            <- ifelse(
        field %in% documented_fields,
        file_metadata$field_spec[field],
        "ext"
      )
      field_provided_status <- field %in% names(gtfs[[file]])

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

  if (! "calendar" %in% names(gtfs)) {

    validation_result[file == "calendar", file_spec := "opt"]
    validation_result[file == "calendar_dates", file_spec := "req"]

  }

  # checks if translations.txt is provided. if it is, feed_info.txt becomes required

  if ("translations" %in% names(gtfs)) {
    validation_result[file == "feed_info", file_spec := "req"]
  }

  # checks for validation status and details

  validation_result[, `:=`(validation_status = "ok", validation_details = NA_character_)]

  # if file is not provided and is required, mark as a problem

  validation_result[
    !file_provided_status & file_spec == "req",
    `:=`(validation_status = "problem", validation_details = "missing_req_file")
  ]

  # if file is not provided and is optional, mark as a info

  validation_result[
    !file_provided_status & file_spec == "opt",
    `:=`(validation_status = "info", validation_details = "missing_opt_file")
  ]

  # if file is provided but misses a required field, mark as a problem

  validation_result[
    file_provided_status & !field_provided_status & field_spec == "req",
    `:=`(validation_status = "problem", validation_details = "missing_req_field")
  ]

  # if file is provided but misses a optional field, mark as a info

  validation_result[
    file_provided_status & !field_provided_status & field_spec == "opt",
    `:=`(validation_status = "info", validation_details = "missing_opt_field")
  ]

  # if file is provided but undocumented in gtfs specifications, mark as a info

  validation_result[
    file_spec == "ext",
    `:=`(validation_status = "info", validation_details = "undocumented_file")
  ]

  # if field is provided but undocumented in gtfs specifications, mark as a info

  validation_result[
    file_spec != "ext" & field_spec == "ext",
    `:=`(validation_status = "info", validation_details = "undocumented_field")
  ]

  # raises warnings if problems are found

  files_problems <- validation_result[validation_details == "missing_req_file"]

  if (nrow(files_problems) >= 1 & warnings) {

    warning(
      paste0(
        "Invalid feed. Missing required file(s): ",
        paste(paste0(unique(files_problems$file), ".txt"), collapse = ", ")
      )
    )

  }

  fields_problems <- validation_result[validation_details == "missing_req_field"]

  if (nrow(fields_problems) >= 1 & warnings) {

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

  if (nrow(files_problems) == 0 & nrow(fields_problems) == 0 & !quiet) {
    message("Valid gtfs data structure.")
  }

  # attach validation_result as an attribute of the given gtfs

  attr(gtfs, "validation_result") <- validation_result

  return(gtfs)

}
