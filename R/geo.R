#' Calculate distances between a given set of stops
#' 
#' @param gtfs_stops gtfs stops table either as data frame (with at least `stop_id`, 
#'                   `stop_lon` and `stop_lat` columns) or as `sf` object.
#' 
#' @return Returns a data.frame with each row containing a pair of stop_ids (columns
#'         `from_stop_id` and `to_stop_id`) and the `distance` between them (in meters)
#'        
#' @note The resulting data.frame has `nrow(gtfs_stops)^2` rows, distances calculations 
#'       among all stops for large feeds should be avoided.
#'       
#' @examples
#' \dontrun{
#' library(dplyr)
#' 
#' nyc_path <- system.file("extdata", "nyc_subway.zip", package = "tidytransit")
#' nyc <- read_gtfs(nyc_path)
#' 
#' nyc$stops %>%
#'   filter(stop_name == "Borough Hall") %>%
#'   stop_distances() %>%
#'   arrange(desc(distance))
#'
#' #> # A tibble: 36 × 3
#' #>    from_stop_id to_stop_id  distance
#' #>    <chr>        <chr>          <dbl>
#' #>  1 423          232             91.5
#' #>  2 423N         232             91.5
#' #>  3 423S         232             91.5
#' #>  4 423          232N            91.5
#' #>  5 423N         232N            91.5
#' #>  6 423S         232N            91.5
#' #>  7 423          232S            91.5
#' #>  8 423N         232S            91.5
#' #>  9 423S         232S            91.5
#' #> 10 232          423             91.5
#' #> # … with 26 more rows
#' }
#' @importFrom dplyr as_tibble
#' @importFrom sf st_distance
#' @importFrom geodist geodist
#' @export
stop_distances = function(gtfs_stops) {
  stopifnot(nrow(gtfs_stops) > 1)
  if(!inherits(gtfs_stops, "data.frame")) {
    stop("Please pass a stops data.frame (i.e. with gtfs_obj$stops)")
  }
  if(inherits(gtfs_stops, "sf")) {
    dist_matrix = st_distance(gtfs_stops)
  } else {
    dist_matrix = geodist(gtfs_stops[,c("stop_id", "stop_lon", "stop_lat")])
  }
  
  rownames(dist_matrix) <- gtfs_stops$stop_id
  colnames(dist_matrix) <- gtfs_stops$stop_id
  dist_matrix_df = as_tibble(dist_matrix, rownames = "from_stop_id")
  
  # replace gather (no dependency on tidyr)
  # dists_gathered = gather(as_tibble(dist_matrix_df), "to_stop_id", "dist", -from_stop_id)
  dists = reshape(as.data.frame(dist_matrix_df), direction = "long",
          idvar = "from_stop_id", timevar = "to_stop_id", v.names = "distance",
          varying = dist_matrix_df$from_stop_id)
  
  dists$to_stop_id <- rep(dist_matrix_df[["from_stop_id"]], each = length(dist_matrix_df[["from_stop_id"]]))
  dists$distance <- as.numeric(dists[["distance"]])

  as_tibble(dists)
}

geodist_list = function(lon, lat, names = NULL) {
  # second fastest measure after cheap
  dists = geodist::geodist(data.frame(lon, lat), measure = "haversine")
  if(!is.null(names)) {
    colnames(dists) <- rownames(dists) <- names
  }
  list(dists)
}

geodist_list_sf = function(pts) {
  dists = sf::st_distance(pts)
  dists <- matrix(as.numeric(dists), nrow = nrow(dists), ncol = ncol(dists))
  colnames(dists) <- rownames(dists) <- NULL
  list(dists)
}

prep_dist_mtrx = function(dist_list) {
  mtrx = dist_list[[1]]
  diag(mtrx) <- NA
  v = c(mtrx)
  v[!is.na(v)]
}

#' Calculates distances among stop within the same group column
#' 
#' By default calculates distances among stop_ids with the same stop_name.
#' 
#' @inheritParams stop_distances
#' @param by group column, default: "stop_name"
#' 
#' @returns data.frame with one row per group containing a distance matrix (distances),
#'          number of stop ids within that group (n_stop_ids) and distance summary values 
#'          (dist_mean, dist_median and dist_max).
#'          
#' @examples 
#' \dontrun{
#' library(dplyr)
#' 
#' nyc_path <- system.file("extdata", "nyc_subway.zip", package = "tidytransit")
#' nyc <- read_gtfs(nyc_path)
#' 
#' stop_group_distances(nyc$stops)
#' #> # A tibble: 380 × 6
#' #>    stop_name   distances       n_stop_ids dist_mean dist_median dist_max
#' #>    <chr>       <list>               <dbl>     <dbl>       <dbl>    <dbl>
#' #>  1 86 St       <dbl [18 × 18]>         18     5395.       5395.   21811.
#' #>  2 79 St       <dbl [6 × 6]>            6    19053.      19053.   19053.
#' #>  3 Prospect Av <dbl [6 × 6]>            6    18804.      18804.   18804.
#' #>  4 77 St       <dbl [6 × 6]>            6    16947.      16947.   16947.
#' #>  5 59 St       <dbl [6 × 6]>            6    14130.      14130.   14130.
#' #>  6 50 St       <dbl [9 × 9]>            9     7097.       7097.   14068.
#' #>  7 36 St       <dbl [6 × 6]>            6    12496.      12496.   12496.
#' #>  8 8 Av        <dbl [6 × 6]>            6    11682.      11682.   11682.
#' #>  9 7 Av        <dbl [9 × 9]>            9     5479.       5479.   10753.
#' #> 10 111 St      <dbl [9 × 9]>            9     3877.       3877.    7753.
#' #> # … with 370 more rows
#' }
#' @importFrom dplyr filter as_tibble
#' @export
stop_group_distances = function(gtfs_stops, by = "stop_name") {
  distances <- NULL
  if(!by %in% colnames(gtfs_stops)) {
    stop("column ", by, " does not exist in ", deparse(substitute(gtfs_stops)))
  }
  if(inherits(gtfs_stops, "sf")) {
    gtfs_stops <- sf_points_to_df(gtfs_stops, c("stop_lon", "stop_lat"), TRUE)
  }
  n_stops = table(gtfs_stops$stop_name)
  
  gtfs_single_stops = gtfs_stops %>% filter(stop_name %in% names(n_stops)[n_stops == 1])
  gtfs_multip_stops = gtfs_stops %>% filter(stop_name %in% names(n_stops)[n_stops != 1])

  if(nrow(gtfs_multip_stops) > 0) {
    gtfs_multip_stops <- gtfs_multip_stops %>%
      dplyr::group_by_at(by) %>%
      dplyr::summarise(distances = geodist_list(stop_lon, stop_lat, stop_id), .groups = "keep") %>%
      dplyr::mutate(n_stop_ids = nrow(distances[[1]]),
                    dist_mean = median(prep_dist_mtrx(distances)),
                    dist_median = median(prep_dist_mtrx(distances)),
                    dist_max = max(prep_dist_mtrx(distances))) %>% ungroup()
    
    # tidytable version
    # gtfs_multip_stops <- gtfs_multip_stops %>%
    #   tidytable::summarise.(distances = geodist_list(stop_lon, stop_lat, stop_id), .by = by) %>%
    #   tidytable::mutate.(n_stop_ids = nrow(distances[[1]]),
    #                      dist_mean = median(prep_dist_mtrx(distances)),
    #                      dist_median = median(prep_dist_mtrx(distances)),
    #                      dist_max = max(prep_dist_mtrx(distances)), .by = "stop_name")
  }
  
  gtfs_single_stops <- gtfs_single_stops %>% 
    select(stop_name) %>% 
    dplyr::mutate(distances = list(matrix(0)), n_stop_ids = 1, dist_mean = 0, dist_median = 0, dist_max = 0)

  dists = as_tibble(dplyr::bind_rows(gtfs_single_stops, gtfs_multip_stops))
  dists[order(dists$dist_max, dists$n_stop_ids, dists[[by]], decreasing = TRUE),]
}

#' Cluster nearby stops within a group
#' 
#' Finds clusters of stops for each unique value in `group_col` (e.g. stop_name). Can 
#' be used to find different groups of stops that share the same name but are located more
#' than `max_dist` apart. `gtfs_stops` is assigned a new column (named `cluster_colname`) 
#' which contains the `group_col` value and the cluster number.
#' 
#' [stats::kmeans()] is used for clustering.
#' 
#' @param gtfs_stops Stops table of a gtfs object. It is also possible to pass a 
#'                   tidygtfs object to enable piping.
#' @param max_dist Only stop groups that have a maximum distance among them above this
#'                 threshold (in meters) are clustered.
#' @param group_col Clusters for are calculated for each set of stops with the same value
#'                  in this column (default: stop_name) 
#' @param cluster_colname Name of the new column name. Can be the same as group_col to overwrite. 
#' 
#' @return Returns a stops table with an added cluster column. If `gtfs_stops` is a tidygtfs object, a 
#'         modified tidygtfs object is return
#' 
#' @importFrom stats kmeans
#' @examples \donttest{
#' library(dplyr)
#' nyc_path <- system.file("extdata", "nyc_subway.zip", package = "tidytransit")
#' nyc <- read_gtfs(nyc_path)
#' nyc <- cluster_stops(nyc)
#' 
#' # There are 6 stops with the name "86 St" that are far apart
#' stops_86_St = nyc$stops %>% 
#'   filter(stop_name == "86 St")
#'
#' table(stops_86_St$stop_name_cluster)
#'
#' stops_86_St %>% select(stop_id, stop_name, parent_station, stop_name_cluster) %>% head()
#' 
#' library(ggplot2)
#' ggplot(stops_86_St) +
#'   geom_point(aes(stop_lon, stop_lat, color = stop_name_cluster))
#' }
#' @export
cluster_stops = function(gtfs_stops,
                         max_dist = 300,
                         group_col = "stop_name",
                         cluster_colname = "stop_name_cluster") {
  if(inherits(gtfs_stops, "tidygtfs")) {
    gstops = gtfs_stops$stops
  } else {
    gstops = gtfs_stops 
  }
  
  is_sf = inherits(gstops, "sf")
  stops_clusters = lapply(unique(gstops[[group_col]]), function(sn) {
    stop_name_set = gstops[gstops[[group_col]] == sn,]
    stop_name_set[[cluster_colname]] <- sn
    if(nrow(stop_name_set) == 1) return(stop_name_set)
    
    dists = stop_distances(stop_name_set)
    if(max(dists$distance) > max_dist) {
      if(is_sf) {
        stop_name_lonlat = do.call(rbind, sf::st_geometry(stop_name_set))
      } else {
        stop_name_lonlat = stop_name_set[,c("stop_lon", "stop_lat")]
      }
      
      stops_unique_coords = unique(stop_name_lonlat)
      n_dists = min(length(unique(dists$distance)), nrow(stops_unique_coords))
      n_clusters = min(n_dists, nrow(stop_name_set)-1)
      
      kms = kmeans(stop_name_lonlat, n_clusters)
      
      stop_name_set[[cluster_colname]] <- paste0(sn, " [", kms$cluster, "]")
    }
    stop_name_set
  })
  stops_clusters = dplyr::bind_rows(stops_clusters)
  
  if(inherits(gtfs_stops, "tidygtfs")) {
    gtfs_stops$stops <- stops_clusters
    return(gtfs_stops)
  } else {
    return(stops_clusters)
  }
}
