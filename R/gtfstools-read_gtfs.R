#' Read and validate GTFS files
#'
#' Reads GTFS text files from either a local \code{.zip} file or an URL and
#' validates them against GTFS specifications.
#'
#' @param path The path to a GTFS \code{.zip} file.
#' @param files A character vector containing the text files to be read from the
#'   GTFS (without the \code{.txt} extension). If \code{NULL} (the default) all
#'   existing files are read.
#' @param quiet Whether to hide log messages and progress bars (defaults to TRUE).
#' @param warnings Whether to display warning messages (defaults to TRUE).
#'
#' @return A GTFS object: a list of data.tables in which each index represents a
#'   GTFS text file.
#'
#' @seealso \code{\link{validate_gtfs}}
#'
#' @examples
#' data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
#'
#' gtfs <- read_gtfs(data_path)
#' names(gtfs)
#'
#' gtfs <- read_gtfs(data_path, files = c("trips", "stop_times"))
#' names(gtfs)
#'
#' @export
read_gtfs <- function(path, files = NULL, quiet = TRUE, warnings = TRUE) {

  checkmate::assert_string(path)
  checkmate::assert_logical(quiet)
  checkmate::assert_logical(warnings)

  # check if path is an url. if so, download the gtfs from it

  if (grepl('http[s]?://.*', path)) {

    temp_file <- tempfile(pattern = "gtfs", fileext = ".zip")

    utils::download.file(path, temp_file, method = "auto", quiet = quiet)

    if (!quiet) message(paste0("File downloaded to ", normalizePath(temp_file)))

    path <- temp_file

  }

  # unzip files to temporary folder

  checkmate::assert_file_exists(path, extension = "zip")

  filenames_in_gtfs <- zip::zip_list(path)$filename

  if (is.null(files)) {

    filenames_to_read <- filenames_in_gtfs

  } else {

    checkmate::assert_names(files, subset.of = sub(".txt", "", filenames_in_gtfs))

    filenames_to_read <- paste0(files, ".txt")

  }

  temp_dir <- file.path(tempdir(), "gt_gtfsdir")
  unlink(temp_dir, recursive = TRUE)
  zip::unzip(path, files = filenames_to_read, exdir = temp_dir, overwrite = TRUE)

  if (!quiet) {
    message(
      paste0(
        "Unzipped the following files to directory ",
        normalizePath(temp_dir),
        ":\n",
        paste0("> ", filenames_to_read, collapse = "\n")
      )
    )
  }

  # read files into list and assign GTFS class

  gtfs <- lapply(filenames_to_read, read_files, temp_dir, quiet)
  gtfs <- stats::setNames(gtfs, sub(".txt", "", filenames_to_read))
  class(gtfs) <- c("gtfs", "list")

  # check if any parsing warnings were thrown

  files_class <- lapply(gtfs, class)
  has_warning <- unlist(lapply(files_class, function(i) "warning" %in% i))

  if (sum(has_warning) >= 1) {

    gtfs_warnings <- gtfs[has_warning]
    gtfs_warnings <- lapply(gtfs_warnings, extract_warning_message)

    if (warnings) {
      warning(
        paste0(
          "Parsing failures while reading the following file(s): ",
          paste(names(gtfs_warnings), collapse = ", ")
        )
      )
    }

    if (!quiet) message("\nReturning parsing failures details.")

    return(gtfs_warnings)

  }

  # if not, validate given GTFS structure against specifications

  gtfs <- validate_gtfs(gtfs, files, quiet, warnings)

  return(gtfs)

}



#' Read GTFS text files
#'
#' Reads a text file from the main zip file.
#'
#' @param filename The name of the file (with \code{.txt} extension) to be read.
#' @param temp_dir The path to the temporary folder where the GTFS was unzipped.
#' @param quiet Whether to hide log messages and progress bars (defaults to TRUE).
#'
#' @return Either a data.table containing the desired file or a log message if
#'   any parsing warnings were thrown.
#'
#' @noRd
read_files <- function(filename, temp_dir, quiet) {

  # uses internal data gtfs_metadata - check data-raw/gtfs_metadata.R

  file_metadata <- gtfs_meta[[gsub(".txt", "", filename)]]
  
  if (!quiet) message(paste0("Reading ", filename))

  # if metadata is null then file is undocumented. read everything as character

  if (is.null(file_metadata)) {

    if (!quiet) {

      message(
        paste0(
          "  File ",
          filename,
          ".txt not recognized. Trying to read it as a csv."
        )
      )

    }
    
    file_path = file.path(temp_dir, filename)
    
    # check if a file is empty. If so, return NULL.
    flength <- suppressWarnings(length(scan(file_path, what = "", quiet = TRUE, sep = "\n")))
    if(flength < 1) {
      if(!quiet) message(sprintf("   File '%s' is empty.", basename(file_path)))
      return(NULL)
    }
    
    full_dt <- data.table::fread(
      file_path,
      colClasses = "character",
      showProgress = !quiet)
    

  } else {
    metadata_colclasses <- setNames(file_metadata$coltype, file_metadata$field)
    
    # read first row to know what columns to read

    sample_dt <- suppressWarnings(
      data.table::fread(file.path(temp_dir, filename), nrows = 1)
    )

    # if file is completely empty (without even a header) return NULL data.table

    if (ncol(sample_dt) == 0) {

      if (!quiet) message("  File is empty. Returning a NULL data.table")
      return(data.table::data.table(NULL))

    }

    # read full file. if a parsing warning has been thrown save it

    col_to_read <- names(sample_dt)
    col_classes <- metadata_colclasses[col_to_read]

    # substitute NA elements in col_classes with undocumented column names and
    # "character" as the default column type

    extra_col <- setdiff(col_to_read, names(col_classes))
    col_classes[is.na(col_classes)] <- "character"
    names(col_classes)[is.na(names(col_classes))] <- extra_col

    full_dt <- tryCatch(
      data.table::fread(file.path(temp_dir, filename), select = col_classes),
      warning = function(w) w
    )

  }

  as.data.frame(full_dt)

}



#' Extract warning message
#'
#' Extracts the warning message as a string from a warning log.
#'
#' @param warning_log Warning log generated by \code{data.table::fread}.
#'
#' @return A string containing details on the parsing failure.
#'
#' @noRd
extract_warning_message <- function(warning_log) {

  warning_log <- as.character(warning_log)

  possible_warnings <- c(
    "Detected \\d+ column names but the data has \\d+ columns \\(i\\.e\\. invalid file\\)\\.",
    "Detected \\d+ column names but the data has \\d+ columns.",
    "Stopped early on line \\d+\\. Expected \\d+ fields but found \\d+\\."
  )

  warning_message <- regmatches(
    warning_log,
    regexpr(
      paste(possible_warnings, collapse = "|"),
      warning_log
    )
  )

  return(warning_message)

}
