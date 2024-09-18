library(readr)
route_type_names <- read_tsv(here:::here("data-raw/route_types.tsv"), 
                             col_types = cols(
                               route_type = col_integer(),
                               route_type_name = col_character()
                             ))
usethis::use_data(route_type_names)
