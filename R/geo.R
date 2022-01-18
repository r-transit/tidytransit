#' Calculate distances between a given set of stops
#' 
#' @param gtfs_stops stops table 
#' 
#' @return Returns a data.frame with each row containing a pair of stop_ids and the 
#'         distance between them (in meters)
#'        
#' @note The resulting data.frame has nrow(gtfs_stops)^2 rows, distances calculations 
#'       among all stops for large feeds should be avoided
#'       
#' @examples
#' library(dplyr)
#' 
#' nyc_path <- system.file("extdata", "google_transit_nyc_subway.zip", package = "tidytransit")
#' nyc <- read_gtfs(nyc_path)
#' 
#' nyc$stops %>%
#'   filter(stop_name == "Borough Hall") %>%
#'   tidytransit::stop_distances() %>%
#'   arrange(desc(dist))
#'
#' #> # A tibble: 36 × 3
#' #>    from_stop_id to_stop_id  dist
#' #>    <chr>        <chr>      <dbl>
#' #>  1 423          232         91.5
#' #>  2 423N         232         91.5
#' #>  3 423S         232         91.5
#' #>  4 423          232N        91.5
#' #>  5 423N         232N        91.5
#' #>  6 423S         232N        91.5
#' #>  7 423          232S        91.5
#' #>  8 423N         232S        91.5
#' #>  9 423S         232S        91.5
#' #> 10 232          423         91.5
#' #> # … with 26 more rows
#' 
#' @export
stop_distances = function(gtfs_stops) {
  stopifnot(nrow(gtfs_stops) > 1)
  from_stop_id <- NULL
  if(!inherits(gtfs_stops, "data.frame")) {
    stop("Please pass a stops data.frame (i.e. with gtfs_obj$stops)")
  }
  if(inherits(gtfs_stops, "sf")) {
    dist_matrix = sf::st_distance(gtfs_stops)
  } else {
    dist_matrix = geodist::geodist(gtfs_stops)
  }
  
  rownames(dist_matrix) <- gtfs_stops$stop_id
  colnames(dist_matrix) <- gtfs_stops$stop_id
  dist_matrix_df = dplyr::as_tibble(dist_matrix, rownames = "from_stop_id")
  
  # replace gather (no dependency on tidyr)
  # dists_gathered = gather(dplyr::as_tibble(dist_matrix_df), "to_stop_id", "dist", -from_stop_id)
  dists = reshape(as.data.frame(dist_matrix_df), direction = "long",
          idvar = "from_stop_id", timevar = "to_stop_id", v.names = "dist",
          varying = dist_matrix_df$from_stop_id)
  
  dists$to_stop_id <- rep(dist_matrix_df$from_stop_id, each = length(dist_matrix_df$from_stop_id))
  dists$dist <- as.numeric(dists$dist)

  dplyr::as_tibble(dists)
}

geodist_list = function(lon, lat, names = NULL) {
  # second fastest measure after cheap
  dists = geodist::geodist(data.frame(lon, lat), measure = "haversine")
  if(!is.null(names)) {
    colnames(dists) <- rownames(dists) <- names
  }
  list(dists)
}

geodist_list_sf = function(pts, names = NULL) {
  dists = as.numeric(sf::st_distance(pts))
  if(!is.null(names)) {
    colnames(dists) <- rownames(dists) <- names
  }
  list(dists)
}

prep_dist_mtrx = function(dist_list) {
  mtrx = dist_list[[1]]
  if(nrow(mtrx) == 1) return(0)
  diag(mtrx) <- NA
  v = c(mtrx)
  v[!is.na(v)]
}

#' Calculates distances among stop within the same group column
#' 
#' By default calculates distances among stop_ids with the same name.
#' 
#' @param gtfs_stops gtfs stops table
#' @param by group column, default: stop_name
#' 
#' @returns data.frame with one row per group containing a distance matrix (dists),
#'          number of stop ids within that group (n_stop_ids) and distance summary values 
#'          (dist_mean, dist_median and dist_max).
#' 
#' @export
stop_group_distances = function(gtfs_stops, by = "stop_name") {
  dists <- n_stop_ids <- dist_mean <- dist_media <- dist_max <- NULL
  if(inherits(gtfs_stops, "sf")) {
    gtfs_stops <- sf_points_to_df(gtfs_stops, c("stop_lon", "stop_lat"), TRUE)
  }
  if(!by %in% colnames(gtfs_stops)) {
    stop("column ", by, " does not exist in ", deparse(substitute(gtfs_stops)))
  }
  n_stops = table(gtfs_stops$stop_name)
  
  gtfs_single_stops = gtfs_stops %>% filter(stop_name %in% names(n_stops)[n_stops == 1])
  gtfs_multip_stops = gtfs_stops %>% filter(stop_name %in% names(n_stops)[n_stops != 1])

  gtfs_multip_stops <- gtfs_multip_stops %>%
    dplyr::group_by_at(by) %>%
    dplyr::summarise(dists = geodist_list(stop_lon, stop_lat, stop_id), .groups = "keep") %>%
    dplyr::mutate(n_stop_ids = nrow(dists[[1]]),
                  dist_mean = median(prep_dist_mtrx(dists)),
                  dist_median = median(prep_dist_mtrx(dists)),
                  dist_max = max(prep_dist_mtrx(dists))) %>% ungroup()
  
  # tidytable version
  # gtfs_multip_stops <- gtfs_multip_stops %>%
  #   tidytable::summarise.(dists = geodist_list(stop_lon, stop_lat, stop_id), .by = by) %>%
  #   tidytable::mutate.(n_stop_ids = nrow(dists[[1]]),
  #                      dist_mean = median(prep_dist_mtrx(dists)),
  #                      dist_median = median(prep_dist_mtrx(dists)),
  #                      dist_max = max(prep_dist_mtrx(dists)), .by = "stop_name")

  gtfs_single_stops <- gtfs_single_stops %>% 
    select(stop_name) %>% 
    dplyr::mutate(dists = list(matrix(0)), n_stop_ids = 1, dist_mean = 0, dist_median = 0, dist_max = 0)

  dists = dplyr::as_tibble(dplyr::bind_rows(gtfs_single_stops, gtfs_multip_stops))
  dists[order(dists$dist_max, dists$n_stop_ids, dists[[by]], decreasing = T),]
}

#' Cluster nearby stops within a group
#' 
#' Finds clusters of stops for each unique value in `group_col` (e.g. stop_name). Can 
#' be used to find different groups of stops that share the same name but are located more
#' than `max_dist` apart. `gtfs_stops` is assigned a new column 
#' (named `cluster_colname`) which contains the `group_col` value and the cluster number.
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
#' nyc_path <- system.file("extdata", "google_transit_nyc_subway.zip", package = "tidytransit")
#' nyc <- read_gtfs(nyc_path)
#' nyc <- cluster_stops(nyc)
#' 
#' # There are 6 stops with the name "86 St" that are far apart
#' stops_86_St = nyc$stops %>% 
#'   filter(stop_name == "86 St")
#'
#' table(stops_86_St$stop_name_cluster)
#' #> 86 St [1] 86 St [2] 86 St [3] 86 St [4] 86 St [5] 86 St [6] 
#' #>         3         3         3         3         3         3 
#'
#' stops_86_St %>% select(stop_id, stop_name, parent_station, stop_name_cluster) %>% head()
#' #> # A tibble: 6 × 4
#' #>   stop_id stop_name parent_station stop_name_cluster
#' #>   <chr>   <chr>     <chr>          <chr>            
#' #> 1 121     86 St     ""             86 St [3]        
#' #> 2 121N    86 St     "121"          86 St [3]        
#' #> 3 121S    86 St     "121"          86 St [3]        
#' #> 4 626     86 St     ""             86 St [4]        
#' #> 5 626N    86 St     "626"          86 St [4]        
#' #> 6 626S    86 St     "626"          86 St [4]
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
    if(max(dists$dist) > max_dist) {
      if(is_sf) {
        stop_name_lonlat = do.call(rbind, sf::st_geometry(stop_name_set))
      } else {
        stop_name_lonlat = stop_name_set[,c("stop_lon", "stop_lat")]
      }
      
      stops_unique_coords = unique(stop_name_lonlat)
      n_dists = min(length(unique(dists$dist)), nrow(stops_unique_coords))
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
