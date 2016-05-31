
# Setup -------------------------------------------------------------------

# source('R/get_gtfs.R')
# source('R/read_gtfs.R')
# source('R/validate_gtfs.R')

# Get location and feed metadata ------------------------------------------

loc_df <- get_locations()

feedlist_df <- get_feedlist()

new_feedlist_df <- filter_feedlist(feedlist_df)


# Take one URL through the process of getting all data into an obj --------

f <- get_feed(feedlist_df$url_d[1]) # need to add a check that this url is a zip and act accordingly

zip_extract_dir <- unzip_gtfs(file = f)

data_list <- read_gtfs(exdir = zip_extract_dir, FALSE)


# Put the workflow into a single function ---------------------------------

feed_flow <- function(url) {

  path <- get_feed(url)

  unzip_gtfs(path)

  read_gtfs(path = strsplit(path, '/')[[1]][1])

}


# Function to check for a component ---------------------------------------

has_component <- function(one_gtfs, component) {
# browser()
  one_list <- unlist(one_gtfs[1], recursive = FALSE)

  list_names <- names(one_list)

  df_name <- paste0(component, '_df')

  has_component <- ifelse(!(df_name %in% list_names), 'no',
                          ifelse(nrow(one_list[df_name][[1]]) == 0, 'empty',
                                 'yes'))

  has_component

}


# Take multiple URLs to a single multi-feed object ------------------------

sub_feedlist_df <- new_feedlist_df[1:5, ]


# Filter to only US agencies ----------------------------------------------

new_feedlist_df <- new_feedlist_df %>%
  mutate(is_USA = grepl('USA', loc_t))

new_feedlist_df <- new_feedlist_df %>%
  filter(is_USA)

# Punting to for loop -----------------------------------------------------

# list all the possible files
all_req_files <- c('agency', 'stops', 'routes', 'trips', 'stop_times', 'calendar')
all_opt_files <- c('calendar_dates', 'fare_attributes', 'fare_rules', 'shapes', 'frequencies', 'transfers', 'feed_info')
all_spec_files <- c(all_req_files, all_opt_files)

# add empty column for each possible file
has_colnames <- paste0('has_', all_spec_files)

for (j in has_colnames) {
  new_feedlist_df[, j] <- NA
}

all_feeds <- list()

# problems: some are FTP (code now skips these, but should handle this)
# one has a subfolder after unzipping (http://www.theride.org/google/google_transit.zip)
for (i in 1:nrow(new_feedlist_df)) {

  if (grepl('ftp', new_feedlist_df$url_d[i])) next

  gtfs_list <- feed_flow(new_feedlist_df$url_d[i])

  all_feeds <- c(all_feeds, gtfs_list)

  for (j in has_colnames) {

    new_feedlist_df[i, j] <- has_component(gtfs_list, gsub('has_', '', j))

  }

  print(paste0('Finished number ', i, ' for ', new_feedlist_df$t[i]))

}

save(all_feeds, new_feedlist_df, file = 'rdata/all_feeds.rda')
write_csv(new_feedlist_df, 'transitfeed_US_feed_metadata.csv')


# Nested dataframe approach -----------------------------------------------

feed_df <- sub_feedlist_df %>%
  rowwise() %>%
  mutate(gtfs_list = feed_flow(url_d))


# Why doesn't this give the right result??

more_feed_df <- feed_df %>%
  rowwise() %>%
  mutate(has_transfers = has_component(gtfs_list, 'transfers'))
