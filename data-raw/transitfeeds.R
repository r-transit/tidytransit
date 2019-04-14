#oncomment to:
#set_api_key()
library(transitfeedr)
feedlist <- get_feedlist()
usethis::use_data(feedlist)
