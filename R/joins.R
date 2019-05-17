#'Join the shapes, trips and routes tables together - also checks on some potential errors in the data and warns accordingly
#' @param gtfs_obj a gtfs object
#' @param route_ids the routes for which to join the tables together - required, but not sure why this can't just be any/all routes in routes_df
#' @param service_ids - an optional filter for a certain service-default NULL
#' @return shapes_routes_service - a dataframe in which routes, services, and shape_ids are all joined
#' @keywords internal
#' @importFrom dplyr %>% group_by filter
#' @importFrom rlang !! .data :=
shape_route_service <- function(gtfs_obj, 
                                route_ids = NULL, 
                                service_ids = NULL) {
  
  stopifnot(class(gtfs_obj) == 'gtfs',
            !is.null(gtfs_obj$shapes),
            !is.null(gtfs_obj$trips),
            !is.null(gtfs_obj$routes))
  
  # pull all route_ids if the user doesn't provide any
  if(length(route_ids) == 0) {
    route_ids <- unique(gtfs_obj$routes$route_id)
  }
  
  # check for bad route ids
  bad_route_ids <- route_ids[which(!route_ids %in% gtfs_obj$routes$route_id)]
  route_ids <- route_ids[which(route_ids %in% gtfs_obj$routes$route_id)]
  
  # error if all route ids are bad
  if(length(route_ids) == 0) {
    s <- "No provided Route ID(s) were found. 
          Please provide valid Route IDs." %>% 
      sprintf(paste(bad_route_ids,
                    collapse = ", "))
    stop(s)
  }
  
  # warn if some route ids are omitted
  if(length(bad_route_ids) > 0) {
    s <- "Route ID(s) '%s' not found. Omitted." %>% 
      sprintf(paste(bad_route_ids, collapse = ", "))
    warning(s)
  }
  
  if(!is.null(service_ids)) {
    # check service ids
    bad_service_ids <- service_ids[which(!service_ids %in% 
                                           gtfs_obj$trips$service_id)]
    service_ids <- service_ids[which(service_ids %in% 
                                       gtfs_obj$trips$service_id)]
    if(length(service_ids) == 0) {
      s <- "No provided Service ID(s) --- 
            '%s' --- were found. 
            Please provide valid Service IDs." %>% 
        sprintf(paste(bad_service_ids, collapse = ", "))
      stop(s)
    }
    if(length(bad_service_ids) > 0) {
      s <- "Service ID(s) '%s' 
            not found. Omitted." %>% 
        sprintf(paste(bad_service_ids, collapse = ", "))
      warning(s)
    }
    shapes_routes_df <- gtfs_obj$trips %>%
      dplyr::filter(.data$service_id %in% service_ids) %>%
      dplyr::filter(.data$route_id %in% route_ids) %>%
      dplyr::select(shape_id, route_id, service_id) %>%
      dplyr::filter(!is.na(.data$shape_id)) %>%
      dplyr::distinct(.data$service_id, 
                      .data$shape_id, 
                      .data$route_id, 
                      .keep_all = TRUE) # want only distinct routes
    
  } else {
    shapes_routes_df <- gtfs_obj$trips %>%
      dplyr::slice(which(.data$route_id %in% route_ids)) %>%
      dplyr::select(shape_id, route_id, service_id) %>%
      dplyr::filter(!is.na(.data$shape_id)) %>%
      dplyr::distinct(.data$service_id, 
                      .data$shape_id, 
                      .data$route_id, 
                      .keep_all = TRUE) # want only distinct routes
  }
  
  return(shapes_routes_df)
  
}

#' Get a set of shapes for a route
#' 
#'
#' @param a dataframe output by join_mega_and_hf_routes()
#' @param route_id the id of the route
#' @param service_id the service for which to get stops 
#' @return shapes for a route
#' @importFrom dplyr %>% group_by filter
#' @importFrom rlang !! .data :=
#' @keywords internal
shape_for_route <- function(g1, select_route_id, select_service_id) {
  some_trips <- g1$trips %>%
    filter(.data$route_id %in% select_route_id & 
             .data$service_id %in% select_service_id)
  
  some_shapes <- g1$shapes %>% 
    filter(.data$shape_id %in% some_trips$shape_id) 
  
  some_shapes$route_id <- select_route_id
  return(some_shapes)
}

#' Get a set of shapes for a set of routes
#' 
#' @param a dataframe output by join_mega_and_hf_routes()
#' @param route_ids the ids of the routes
#' @param service_ids the service for which to get stops 
#' @param directional if the routes should by related to a route direction (e.g. inbound, outbound) - currently not implemented
#' @return shapes for routes
#' @keywords internal
shapes_for_routes <- function(g1, 
                              route_ids, 
                              service_ids, 
                              directional=FALSE) {
  l1 = list()
  i <- 1
  for (route_id in route_ids) {
    l1[[i]] <- shape_for_route(g1,route_id, 
                               service_ids)
    i <- i + 1
  }
  df_routes <- do.call("rbind", l1)
  return(df_routes)
}


#' Get a set of stops for a given set of service ids and route ids
#' 
#' @param gtfs_obj as read by read_gtfs()
#' @param service_ids the service for which to get stops 
#' @param route_ids the route_ids for which to get stops 
#' @return stops for a given service
#' @export
#' @examples \donttest{
#' local_gtfs_path <- system.file("extdata", "google_transit_nyc_subway.zip", package = "tidytransit")
#' nyc <- read_gtfs(local_gtfs_path,local=TRUE)
#' select_service_id <- filter(nyc$calendar, monday==1) %>% pull(service_id)
#' select_route_id <- sample_n(nyc$routes, 1) %>% pull(route_id)
#' filtered_stops_df <- filter_stops(nyc, select_service_id, select_route_id)
#' }
filter_stops <- function(gtfs_obj, service_ids, route_ids) {
  some_trips <- gtfs_obj$trips %>%
    dplyr::filter(.data$service_id %in% service_ids &
                    .data$route_id %in% route_ids)
  
  some_stop_times <- gtfs_obj$stop_times %>% 
    dplyr::filter(.data$trip_id %in% some_trips$trip_id) 
  
  some_stops <- gtfs_obj$stops %>%
    dplyr::filter(.data$stop_id %in% some_stop_times$stop_id)
  
  return(some_stops)
}