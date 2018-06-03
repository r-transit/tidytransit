devtools::load_all()
feedlist_df <- get_feedlist() # create a data frame of all feeds
saveRDS(feedlist_df, here::here('data-raw/feedlist_df'))
