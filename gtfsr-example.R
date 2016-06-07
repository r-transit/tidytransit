# gtfsr-example.R

## install
devtools::install_github('ropenscilabs/gtfsr')

library(gtfsr)
library(magrittr)
library(dplyr)
options(dplyr.width = Inf) # I like to see all the columns


## set api key and get full list of GTFS Feeds
set_api_key('2ec1ae29-b8c2-4a03-b96e-126d585233f9') # input your API key here

feedlist_df <- get_feedlist() # create a data frame of all feeds
feedlist_df <- feedlist_df %>% filter_feedlist # filter the feedlist
feedlist_df %>% select(url_d) %>% head(5) # show first 5 feed urls

## get australian feeds then extract data from 2 urls
aussie_df <- feedlist_df %>%
    filter(grepl('australia', loc_t, ignore.case = TRUE)) # filter out locations with "australia" in name
aussie_df %>% select(loc_t) %>% head(5) # look at location names
aussie_urls <- aussie_df %>% select(url_d) # get aussie urls
data_list <- aussie_urls %>% slice(8:9) %>% get_gtfs

## how to extract 'problems'
url <- feedlist_df %>% slice(44) %>% select(url_d)
data_list1 <- url %>% get_gtfs

df <- data_list1[[1]]$calendar_df # extract `calendar_df` from the 1st list element (there's only one)
df
attr(df, 'problems')





