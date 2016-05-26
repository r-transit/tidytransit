#############
## This is some ad hoc analysis I was working to look at characteristics across a list of multiple feeds
## some may be based on outdated versions of the functions in the files sourced at the top ...
## It might be useful to abstract some of these things into functions
#############

# source('R/get_gtfs.R')
# source('R/read_gtfs.R')
# source('R/validate_gtfs.R')

# library(tidyr)

# # Would need to load results first
# attach('rdata/all_feeds.rda')


# Get location and feed metadata ------------------------------------------

loc_df <- get_locations()

feedlist_df <- get_feedlist()

new_feedlist_df <- filter_feedlist(feedlist_df)

# validation? ------------------------------------------------
all_req_files <- c('agency', 'stops', 'routes', 'trips', 'stop_times', 'calendar')
all_opt_files <- c('calendar_dates', 'fare_attributes', 'fare_rules', 'shapes', 'frequencies', 'transfers', 'feed_info')
all_spec_files <- c(all_req_files, all_opt_files)

has_colnames <- paste0('has_', all_spec_files)
req_colnames <- paste0('has_', all_req_files)
opt_colnames <- paste0('has_', all_opt_files)

result_df <- new_feedlist_df[, c('id', 't', 'loc_t', has_colnames)]

long_result_df <- result_df %>%
  filter(!is.na(has_agency)) %>% # these must not have downloaded
  gather(key = 'file', value = 'file_provided', has_agency:has_feed_info) %>%
  mutate(is_required = file %in% req_colnames) %>%
  mutate(file = gsub('has_', '', file))

summary_req_df <- long_result_df %>%
  filter(is_required) %>%
  group_by(id, t, loc_t) %>%
  summarize(n_req = sum(file_provided == 'yes')) %>%
  mutate(says_gtfs = grepl(pattern = 'GTFS', x = t))

summary_req_gtfs_df <- summary_req_df %>%
  filter(says_gtfs)

req_0 <- summary_req_df$id[summary_req_df$n_req == 0]

summary_file_df <- long_result_df %>%
  mutate(says_gtfs = grepl(pattern = 'GTFS', x = t)) %>%
  filter(!(id %in% req_0)) %>%
  mutate(n_agencies = n_distinct(id)) %>%
  group_by(n_agencies, is_required, file, file_provided) %>%
  summarize(n_files = n()) %>%
  mutate(pct_agencies = round(100 * n_files/n_agencies, 0))

# This answers the question: what percent of agencies provide each file type?
summary_provided_df <- summary_file_df %>%
  ungroup() %>%
  select(file, is_required, file_provided, pct_agencies) %>%
  mutate(file_provided = paste0('pct_', file_provided)) %>%
  spread(file_provided, value = pct_agencies, fill = 0) %>%
  arrange(desc(is_required))



# How many agencies per feed? ---------------------------------------------

# This is all that have feeds with contents, but some may not be GTFS
num_agencies <- unlist(sapply(all_feeds, function(x) nrow(x$agency_df)))

numa_df <- data_frame(num_agencies_per_feed = num_agencies)
summary_numag_df <- numa_df %>%
  group_by(num_agencies_per_feed) %>%
  summarize(n_feeds = n()) %>%
  mutate(pct_feeds = round(100 * n_feeds / sum(n_feeds), 1))


# How many routes and stops per feed? -------------------------------------

num_stops_df <- data_frame(num_stops = unlist(sapply(all_feeds, function(x) n_distinct(x$stops_df$stop_id))))

num_routes_df <- data_frame(num_routes = unlist(sapply(all_feeds, function(x) n_distinct(x$routes_df$route_id))))


# Validation --------------------------------------------------------------

sub_feeds <- all_feeds[1:10]

val_list <- lapply(all_feeds, function(x) validate_feed(x, return_feed = FALSE))

req_files <- unlist(sapply(val_list, function(x) x$all_req_files))
req_fields <- unlist(sapply(val_list, function(x) x$all_req_fields))

validation_missing <- lapply(val_list, function(x) {
  if (is.null(x$validate_df)) {
    return(data_frame())
  } else {
    missing_df <- x$validate_df %>% filter(validation_status != 'ok')
    return(missing_df)
  }
})

validation_all <- lapply(val_list, function(x) {
  if (is.null(x$validate_df)) {
    return(data_frame())
  } else {
    all_df <- x$validate_df
    return(all_df)
  }
})

req_both_df <- data_frame(files = req_files, fields = req_fields)

req_both_df %>%
  group_by(files, fields) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(pct = round(100 * n/sum(n)))

lapply(all_feeds, function(x) validate_feed(x, return_feed = FALSE))


# Get stop locations  --------------------------------------------------

all_stops <- lapply(all_feeds, function(x) x$stops_df)
all_feedname <- lapply(all_feeds, function(x) x$agency_df$agency_name)

length(all_feedname) == length(all_stops)

num_agencies <- sapply(all_feedname, function(x) length(x))
one_agency <- num_agencies == 1

keep_stops <- all_stops[one_agency]

names(keep_stops) <- all_feedname[one_agency]


stops_ll <- lapply(keep_stops, function(x) x[, c('stop_lat', 'stop_lon')])

stops_df <- data_frame()

for (i in 1:length(stops_ll)) {
  temp_df <- stops_ll[[i]]
  temp_df$short_name <- names(stops_ll)[i]
  temp_df <- temp_df %>% rename(latitude = stop_lat, longitude = stop_lon)
  temp_df$source <- 'GTFS'
  stops_df <- bind_rows(stops_df, temp_df)
}


