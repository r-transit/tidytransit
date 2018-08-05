#'Join the shapes, trips and routes tables together - also checks on some potential errors in the data and warns accordingly
#' @param gtfs_obj a gtfs object
#' @param route_ids the routes for which to join the tables together - required, but not sure why this can't just be any/all routes in routes_df
#' @param service_ids - an optional filter for a certain service-default NULL
#' @return shapes_routes_service_df - a dataframe in which routes, services, and shape_ids are all joined
#' @keywords internal
#' @importFrom dplyr %>% group_by filter
#' @importFrom rlang !! .data :=
shape_route_service <- function(gtfs_obj, route_ids = NULL, service_ids = NULL) {
  
  stopifnot(class(gtfs_obj) == 'gtfs',
            !is.null(gtfs_obj$shapes_df),
            !is.null(gtfs_obj$trips_df),
            !is.null(gtfs_obj$routes_df))
  
  # pull all route_ids if the user doesn't provide any
  if(length(route_ids) == 0) {
    route_ids <- unique(gtfs_obj$routes_df$route_id)
  }
  
  # check for bad route ids
  bad_route_ids <- route_ids[which(!route_ids %in% gtfs_obj$routes_df$route_id)]
  route_ids <- route_ids[which(route_ids %in% gtfs_obj$routes_df$route_id)]
  
  # error if all route ids are bad
  if(length(route_ids) == 0) {
    s <- "No provided Route ID(s) were found. Please provide valid Route IDs." %>% sprintf(paste(bad_route_ids, collapse = ", "))
    stop(s)
  }
  
  # warn if some route ids are omitted
  if(length(bad_route_ids) > 0) {
    s <- "Route ID(s) '%s' not found. Omitted." %>% sprintf(paste(bad_route_ids, collapse = ", "))
    warning(s)
  }
  
  if(!is.null(service_ids)) {
    
    # check service ids
    bad_service_ids <- service_ids[which(!service_ids %in% gtfs_obj$trips_df$service_id)]
    service_ids <- service_ids[which(service_ids %in% gtfs_obj$trips_df$service_id)]
    
    if(length(service_ids) == 0) {
      s <- "No provided Service ID(s) --- '%s' --- were found. Please provide valid Service IDs." %>% sprintf(paste(bad_service_ids, collapse = ", "))
      stop(s)
    }
    
    if(length(bad_service_ids) > 0) {
      s <- "Service ID(s) '%s' not found. Omitted." %>% sprintf(paste(bad_service_ids, collapse = ", "))
      warning(s)
    }
    
    shapes_routes_df <- gtfs_obj$trips_df %>%
      dplyr::filter(.data$service_id %in% service_ids) %>%
      dplyr::filter(.data$route_id %in% route_ids) %>%
      dplyr::select_('shape_id', 'route_id', 'service_id') %>%
      dplyr::filter(!is.na(.data$shape_id)) %>%
      dplyr::distinct(.data$service_id, .data$shape_id, .data$route_id, .keep_all = TRUE) # want only distinct routes
    
  } else {
    
    shapes_routes_df <- gtfs_obj$trips_df %>%
      dplyr::slice(which(.data$route_id %in% route_ids)) %>%
      dplyr::select_('shape_id', 'route_id', 'service_id') %>%
      dplyr::filter(!is.na(.data$shape_id)) %>%
      dplyr::distinct(.data$service_id, .data$shape_id, .data$route_id, .keep_all = TRUE) # want only distinct routes
    
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
  some_trips <- g1$trips_df %>%
    filter(.data$route_id %in% select_route_id & 
             .data$service_id %in% select_service_id)
  
  some_shapes <- g1$shapes_df %>% 
    filter(.data$shape_id %in% some_trips$shape_id) 
  
  some_shapes$route_id <- select_route_id
  return(some_shapes)
}

#' Get a set of shapes for a set of routes
#' 
#' @param a dataframe output by join_mega_and_hf_routes()
#' @param route_ids the ids of the routes
#' @param service_id the service for which to get stops 
#' @param directional if the routes should by related to a route direction (e.g. inbound, outbound) - currently not implemented
#' @return shapes for routes
#' @keywords internal
shapes_for_routes <- function(g1, route_ids, select_service_ids, directional=FALSE) {
  l1 = list()
  i <- 1
  for (route_id in route_ids) {
    l1[[i]] <- shape_for_route(g1,route_id, select_service_ids)
    i <- i + 1
  }
  df_routes <- do.call("rbind", l1)
  return(df_routes)
}

