#' Write GTFS files
#'
#' Writes in-memory GTFS objects as GTFS \code{.zip} files. Conditionally
#' includes optional and extra \code{.txt} files (check \code{\link{validate_gtfs}}
#' documentation to check what are optional/extra files).
#'
#' @param gtfs A GTFS object as created by \code{\link{read_gtfs}}.
#' @param path The path to the \code{.zip} file in which the feed should be
#'   written to.
#' @param optional Whether to write optional \code{.txt}. Defaults to TRUE.
#' @param extra Whether to write extra \code{.txt}. Defaults to TRUE.
#' @param overwrite Whether to overwrite existing \code{.zip} file. Defaults to
#'   TRUE.
#' @param quiet Whether to hide log messages and progress bars (defaults to TRUE).
#' @param warnings Whether to display warning messages (defaults to TRUE).
#'
#' @return Invisibly returns the provided GTFS object with an updated
#'   \code{validation_result} attribute.
#'
#' @seealso \code{\link{validate_gtfs}}
#'
#' @examples
#' data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
#' gtfs <- read_gtfs(data_path)
#'
#' tmp_dir <- tempdir()
#' list.files(tmp_dir)
#'
#' tmp_file <- tempfile(pattern = "gtfs", tmpdir = tmp_dir, fileext = ".zip")
#' write_gtfs(gtfs, tmp_file)
#' list.files(tmp_dir)
#'
#' gtfs_all_files <- read_gtfs(tmp_file)
#' names(gtfs_all_files)
#'
#' write_gtfs(gtfs_all_files, tmp_file, optional = FALSE)
#' gtfs_no_opt <- read_gtfs(tmp_file)
#' names(gtfs_no_opt)
#'
# TODO handle additional columns
#' @noRd
gtfstools_write_gtfs <- function(gtfs,
                       path,
                       optional = TRUE,
                       extra = TRUE,
                       overwrite = TRUE,
                       quiet = TRUE,
                       warnings = TRUE) {

  checkmate::assert_class(gtfs, "gtfs")
  checkmate::assert_path_for_output(path, overwrite = overwrite, extension = "zip")
  checkmate::assert_logical(optional)
  checkmate::assert_logical(extra)
  checkmate::assert_logical(overwrite)

  # validate gtfs

  gtfs <- validate_gtfs(gtfs, files = NULL, quiet = quiet, warnings = warnings)

  # write files to temporary folder

  temp_dir <- file.path(tempdir(), "gt_gtfsdir")
  unlink(temp_dir, recursive = TRUE)
  dir.create(temp_dir)

  # figure out what files should be written

  files_in_gtfs <- names(gtfs)

  validation_result <- attr(gtfs, "validation_result")
  files_index <- validation_result[, .I[1], keyby = file]$V1
  files_specs <- stats::setNames(
    validation_result$file_spec[files_index],
    validation_result$file[files_index]
  )

  files_to_write <- files_in_gtfs

  # optional/ext files are those marked as "opt"/"ext" in the validation attribute

  optional_files <- files_to_write[files_specs[files_to_write] == "opt"]
  extra_files    <- files_to_write[files_specs[files_to_write] == "ext"]

  if (!optional) files_to_write <- files_to_write[! files_to_write %in% optional_files]
  if (!extra)    files_to_write <- files_to_write[! files_to_write %in% extra_files]

  if (!quiet) message(paste0("Writing .txt files to ", normalizePath(temp_dir), ":"))

  for (file in files_to_write) {

    if (!quiet) message("> ", paste0(file, ".txt"))

    dt <- gtfs[[file]]
    col_classes <- vapply(dt, function(i) class(i)[1], character(1))

    # format dates back to YYYYMMDD

    date_cols <- names(which(col_classes == "Date"))

    if (length(date_cols) > 0) {

      dt <- data.table::copy(dt)
      dt[, (date_cols) := lapply(.SD, format, "%Y%m%d"), .SDcols = date_cols]

    }

    # write files

    file_path <- file.path(temp_dir, paste0(file, ".txt"))

    data.table::fwrite(dt, file_path, showProgress = !quiet)

  }

  # zip files

  zip::zipr(path, file.path(temp_dir, paste0(files_to_write, ".txt")))

  if (!quiet) {
    message(paste0("GTFS file successfully zipped to ", normalizePath(path)))
  }

  return(invisible(gtfs))

}
