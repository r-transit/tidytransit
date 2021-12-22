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
  
  dists = dplyr::bind_rows(gtfs_single_stops, gtfs_multip_stops)
  dists[order(dists$dist_max, dists$n_stop_ids, dists$stop_name, decreasing = T),]
}
