#oncomment to:
#set_api_key()
library(transitfeedr)
feedlist <- get_feedlist()
devtools::use_data(feedlist)
