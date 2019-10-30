#' GTFS feed summary
#'
#' @param object a gtfs_obj as read by [read_gtfs()]
#' @export
#' @param ... further specifications
#' @importFrom dplyr select arrange filter
summary.gtfs <- function(object, ...) {
  dots <- list(...)
  
  # agency info
  agencies <- object$agency$agency_name
  ag_n <- length(agencies)
  ag_p <- min(ag_n, 3)
  ag_excess = ag_n - ag_p
  agencies_info <- paste(agencies[1:ag_p], collapse=", ")
  if(ag_excess > 0) {
    agencies_info <- paste0(agencies_info, " ... ", ag_excess, " more")
  }
  agency_sing_plur <- ifelse(ag_n == 1, "agency       ", "agencies     ")
                             
  # counts
  n_stop_ids <- length(unique(object$stops$stop_id))
  n_stop_names <- length(unique(object$stops$stop_name))
  n_trips <- length(unique(object$trips$trip_id))
  n_routes <- length(unique(object$routes$route_id))
  n_shapes <- length(unique(object$shapes$shape_id))
  w <- ceiling(log(max(n_stop_ids, n_stop_names, n_trips, n_routes, n_shapes), 10))
  
  # stop_times or frequency based
  n_st <- ifelse(is.null(nrow(object$stop_times)), 0 , nrow(object$stop_times))
  n_freq <- ifelse(is.null(nrow(object$frequencies)), 0 , nrow(object$frequencies))
  if(n_st > 0) {
    if(n_freq > 0) {
      st_freq_info <- "stop_times and frequencies"
    } else {
      st_freq_info <- "stop_times (no frequencies)"
    }
  } else {
      st_freq_info <- "frequencies (no stop_times)"
  }

  # files provided
  validation_result <- attributes(object)$validation_result
  files <- unique(validation_result[c("file", "file_spec", "file_provided_status")])
  files <- arrange(files, dplyr::desc(file_spec))
  files <- filter(files, file_provided_status == TRUE)

  # date info
  if(is.null(object$.$date_service_table)) {
    object <- set_date_service_table(object)
  }
  date_min <- min(object$.$date_service_table$date)
  date_max <- max(object$.$date_service_table$date)
  
  cat(paste0("GTFS object\n"))
  cat(paste0("files        ", paste(files$file, collapse=", "), "\n"))
  cat(paste0(agency_sing_plur, agencies_info, "\n"))
  cat(paste0("service      from ", date_min, " to ", date_max, "\n"))
  cat(paste0("uses         ", st_freq_info, "\n"))
  cat(paste0("# routes     ", format(n_routes, width = w), "\n"))
  cat(paste0("# trips      ", format(n_trips, width = w), "\n"))
  cat(paste0("# stop_ids   ", format(n_stop_ids, width = w), "\n"))
  cat(paste0("# stop_names ", format(n_stop_names, width = w), "\n"))
  cat(paste0("# shapes     ", format(n_shapes, width = w), "\n"))
}

