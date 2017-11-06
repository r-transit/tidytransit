#' Get shapes spatial data for given route ids
#' @param gtfs_obj A GTFS list object with components agency_df, etc.
#' @param route_ids Vector (Character). IDs for routes of interest.
#' @param service_ids Vector (Character). Service IDs. NULL by Default.
#' @param shape_ids Vector (Character). Shape IDs. NULL by Default.
#' @param route_opacity Numeric. Value must be between 0 and 1. Default is NULL.
#' @param route_colors Character. Names of colors (e.g. "blue") or hex values (e.g. '#000000').
#' @return Environment containing spatial data, labels, colorings used for plotting
#' @export
get_routes_sldf <- function(gtfs_obj, route_ids, service_ids, shape_ids, route_opacity, route_colors) {

  stopifnot(class(gtfs_obj) == 'gtfs',
  !is.null(gtfs_obj$shapes_df),
  !is.null(gtfs_obj$trips_df),
  !is.null(gtfs_obj$routes_df),
  length(route_ids) > 0)

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
      dplyr::slice(which(service_id %in% service_ids)) %>%
      dplyr::slice(which(route_id %in% route_ids)) %>%
      dplyr::select(shape_id, route_id, service_id) %>%
      dplyr::filter(!is.na(shape_id)) %>%
      dplyr::distinct(., service_id, shape_id, route_id, .keep_all = TRUE) # want only distinct routes

  } else {

    shapes_routes_df <- gtfs_obj$trips_df %>%
      dplyr::slice(which(route_id %in% route_ids)) %>%
      dplyr::select(shape_id, route_id, service_id) %>%
      dplyr::filter(!is.na(shape_id)) %>%
      dplyr::distinct(., service_id, shape_id, route_id, .keep_all = TRUE) # want only distinct routes

  }

  # extract or check for shape_ids
  if(is.null(shape_ids)) {
    shape_ids <- shapes_routes_df$shape_id
  } else {
    indx <- shape_ids %in% shapes_routes_df$shape_id
    if(length(shape_ids[indx]) == 0) {
      message("No user defined shape_ids found. Using all valid shape_ids.")
      shape_ids <- shapes_routes_df$shape_id
    } else {
      if(length(indx) > length(shape_ids[indx])) {
        s <- "Shape ID(s) '%s' not found. Omitted." %>% sprintf(paste(shape_ids[!indx], collapse = ", "))
        message(s)
      }

      # filter out user specified shapes and update
      shapes_routes_df %<>%
        dplyr::filter(shape_id %in% shape_ids[indx])
      shape_ids <- shapes_routes_df$shape_id

    }

  }

  # check if nothing is found
  if(length(shape_ids) == 0) {
    s <- "No shapes for Route ID '%s' were found." %>% sprintf(paste(route_ids, collapse = ", "))
    stop(s)
  }

  gtfs_trips <- gtfs_obj$trips_df %>%
    dplyr::slice(which(route_id %in% route_ids))

  # extract all shapes for given shape ids
  gtfs_shapes <- gtfs_obj$shapes_df %>%
    dplyr::slice(which(shape_id %in% shape_ids))

  # code was taken from `stplanr::gtfs2sldf` (package::function)
  sp_lines <- (gtfs_shapes %>% dplyr::rename(lat = shape_pt_lat, lon = shape_pt_lon) %>%
    dplyr::group_by(shape_id) %>%
    dplyr::arrange(shape_pt_sequence) %>% dplyr::do_(gtfsline = "sp::Lines(sp::Line(as.matrix(.[,c('lon','lat')])),unique(.$shape_id))") %>%
    dplyr::ungroup() %>% dplyr::do_(gtfsline = "sp::SpatialLines(.[[2]], proj4string = sp::CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))")) %>%
    magrittr::extract2('gtfsline') %>%
    magrittr::extract2(1)

  # get updated shape ids (order matters)
  shape_ids <- sapply(sp_lines@lines, function(x) x@ID)

  df <- shape_ids %>%
    as.data.frame %>%
    `rownames<-`(., shape_ids)

  gtfs_lines <- sp::SpatialLinesDataFrame(sp_lines, data = df)

  # OPACITY AND COLORS ------------------------------------------------
  ## route_colors
  if(!is.null(route_colors)) {
    if(length(route_colors) != length(route_ids)) {
      warning("route_colors and route_ids are not the same length. route_colors is ignored and default colors will be used.")
      route_colors <- scales::hue_pal()(length(route_ids))
    } else {
      route_colors <- scales::col2hcl(route_colors) %>%
        sapply(. %>% substr(.,1,7), USE.NAMES = FALSE)
    }
  } else {
    route_colors <- scales::hue_pal()(length(route_ids))
  }

  # extract corresponding route ids and names for shape ids
  routes_colors_df <- dplyr::data_frame(route_id = route_ids,
    color = route_colors) %>%
    dplyr::left_join(gtfs_obj$routes_df %>% dplyr::select(route_id, route_short_name), by = 'route_id')

  # merge colors to shape_routes
  shapes_routes_colors_df <- shapes_routes_df %>%
    dplyr::left_join(routes_colors_df, by = 'route_id')

  # popup maker
  gen_popups_routes <- function(a, b, c) {
    mapply(function(x, y, z) {
    text <- htmltools::tags$div(style = 'color:#000000',
      htmltools::tags$body(
        htmltools::span(htmltools::strong(paste('Route', x))), htmltools::br(),
        htmltools::span(htmltools::strong('Service ID: '), y), htmltools::br(),
        htmltools::span(htmltools::strong('Shape ID: '), z))
      )
      as.character(text)
    },
    x = a,
    y = b,
    z = c,
    SIMPLIFY = FALSE) %>%
    unlist %>%
    sapply(stringr::str_replace_all, '\n[ ]*', '') %>%
    stats::setNames(NULL)
  }

  # make color vector for shapes
  shapes_colors_df <- shapes_routes_colors_df %>%
    dplyr::slice(match(shape_ids, shape_id)) %>% # match helps resort rows so colors/labels match up with gtfs_lines (only works cause we have ONE of each shape)
    dplyr::group_by(route_id) %>%
    dplyr::mutate(n = n(), opacity = route_opacity/(n)) %>% # opacity is scaled by route numbers
    dplyr::mutate(labels = paste("Route", route_short_name)) %>%
    dplyr::mutate(popups = gen_popups_routes(route_id, service_id, shape_id)) %>%
    dplyr::select(-n) %>%
    dplyr::ungroup() %>% # important to keep order correct!
    dplyr::select(shape_id, color, opacity, labels, popups) %>%
    dplyr::mutate(opacity = dplyr::if_else(opacity < 0.05, 0.05, opacity)) # opacity threshold

  # CHECKING ROUTES ------------------------------------------------
  # update/ensure route_ids carry correctly and sort correctly for plotting
  keep <- shapes_routes_df$route_id %>% unique()
  ids <- routes_colors_df$route_id

  indx <- match(ids, keep) %>% stats::na.omit()
  not_found <- ids[!ids %in% keep] #routes not found

  #check to see if routes were dropped
  if(length(ids) > length(indx)) {
    s <- sprintf("Shapes for route_id(s) %s are/were not found. Removed.", paste(not_found, collapse = ', '))
    message(s)
  }

  # update routes and keep only routes with shapes
  routes_colors_df %<>%
    dplyr::filter(route_id %in% keep)

  # return
  env <- new.env()
  lapply(c("gtfs_lines", "shapes_colors_df", "shapes_routes_df", "routes_colors_df"), function(x) assign(x, value = get(x), envir = env))
  return(env)

}

  #' extract all possible stops across all trips for given route
#' @noRd
get_possible_stops <- function(gtfs_obj, trip_ids) {
  gtfs_obj$stop_times_df %>%
    dplyr::slice(which(trip_id %in% trip_ids)) %>%
    dplyr::select(stop_id) %>%
    unique %>%
    magrittr::extract2(1)

}


#' Generate stop popups
#' @noRd
gen_stop_popups <- function(a, b, c, d) {


  # a <- stops$stop_name
  # b <- stops$stop_id
  # c <- stops$lat
  # d <- stops$lng

  # popup maker
  mapply(function(w, x, y, z) {
  text <- htmltools::tags$div(style = 'color:#000000',
    htmltools::tags$body(
      htmltools::span(htmltools::strong(w)), htmltools::br(),
      htmltools::span(htmltools::strong('Stop ID: '), x), htmltools::br(),
      htmltools::span(htmltools::strong('Lat: '), y), htmltools::br(),
      htmltools::span(htmltools::strong('Lon: '), z))
    )
    as.character(text)
  },
  w = a,
  x = b,
  y = c,
  z = d,
  SIMPLIFY = FALSE) %>%
  unlist %>%
  sapply(stringr::str_replace_all, '\n[ ]*', '') %>%
  stats::setNames(NULL)
}

