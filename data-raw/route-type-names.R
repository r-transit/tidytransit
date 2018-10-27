library(readr)
route_type_names_df <- read_tsv(here:::here("/data-raw/route_types.tsv"))
devtools::use_data(route_type_names_df)
