library(readr)
route_type_names <- read_tsv(here:::here("/data-raw/route_types.tsv"))
usethis::use_data(route_type_names)
