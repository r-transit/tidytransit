## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(tidytransit)
library(dplyr)
library(ggplot2)

## -----------------------------------------------------------------------------
local_gtfs_path <- system.file("extdata", "google_transit_nyc_subway.zip", package = "tidytransit")
gtfs <- read_gtfs(local_gtfs_path)
# gtfs <- read_gtfs("http://web.mta.info/developers/data/nyct/subway/google_transit.zip")

## -----------------------------------------------------------------------------
# get the id of the first stop in the trip's stop sequence
first_stop_id <- gtfs$stop_times %>% 
  group_by(trip_id) %>% 
  summarise(stop_id = stop_id[which.min(stop_sequence)])

# join with the stops table to get the stop_name
first_stop_names <- left_join(first_stop_id, gtfs$stops, by="stop_id")

# rename the first stop_name as trip_origin
trip_origins <- first_stop_names %>% select(trip_id, trip_origin = stop_name)

# join the trip origins back onto the trips
gtfs$trips <- left_join(gtfs$trips, trip_origins, by = "trip_id")

## -----------------------------------------------------------------------------
gtfs$trips %>%
  select(route_id, trip_origin) %>%
  head()

## -----------------------------------------------------------------------------
if(!exists("trip_headsign", where = gtfs$trips)) {
  # get the last id of the trip's stop sequence
  trip_headsigns <- gtfs$stop_times %>% 
    group_by(trip_id) %>% 
    summarise(stop_id = stop_id[which.max(stop_sequence)]) %>% 
    left_join(gtfs$stops, by="stop_id") %>%
    select(trip_id, trip_headsign.computed = stop_name)

  # assign the headsign to the gtfs object 
  gtfs$trips <- left_join(gtfs$trips, trip_headsigns, by = "trip_id")
}

## -----------------------------------------------------------------------------
stop_ids <- gtfs$stops %>% 
  filter(stop_name == "Times Sq - 42 St") %>% 
  select(stop_id)

## -----------------------------------------------------------------------------
departures <- stop_ids %>% 
  inner_join(gtfs$stop_times %>% 
               select(trip_id, arrival_time, 
                      departure_time, stop_id), 
             by = "stop_id")

departures <- departures %>% 
  left_join(gtfs$trips %>% 
              select(trip_id, route_id, 
                     service_id, trip_headsign, 
                     trip_origin), 
            by = "trip_id") 

## -----------------------------------------------------------------------------
departures <- departures %>% 
  left_join(gtfs$routes %>% 
              select(route_id, 
                     route_short_name), 
            by = "route_id")

## -----------------------------------------------------------------------------
departures %>% 
  select(arrival_time,
         departure_time,
         trip_headsign,trip_origin,
         route_id) %>%
  head() %>%
  knitr::kable()

## -----------------------------------------------------------------------------
head(gtfs$.$dates_services)

## ----fig.width=8, fig.height=12-----------------------------------------------
services_on_180823 <- gtfs$.$dates_services %>% 
  filter(date == "2018-08-23") %>% select(service_id)

departures_180823 <- departures %>% 
  inner_join(services_on_180823, by = "service_id")

## -----------------------------------------------------------------------------
departures_180823 %>%
  arrange(departure_time, stop_id, route_short_name) %>% 
  select(departure_time, stop_id, route_short_name, trip_headsign) %>% 
  filter(departure_time >= hms::hms(hours = 7)) %>% 
  filter(departure_time < hms::hms(hours = 7, minutes = 10)) %>% 
  knitr::kable()

## ----fig.width=8, fig.height=6------------------------------------------------
route_colors <- gtfs$routes %>% select(route_id, route_short_name, route_color)
route_colors$route_color[which(route_colors$route_color == "")] <- "454545"
route_colors <- setNames(paste0("#", route_colors$route_color), route_colors$route_short_name)

ggplot(departures_180823) + theme_bw() +
  geom_point(aes(y=trip_headsign, x=departure_time, color = route_short_name), size = 0.2) +
  scale_x_time(breaks = seq(0, max(as.numeric(departures$departure_time)), 3600), 
               labels = scales::time_format("%H:%M")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "bottom") +
  scale_color_manual(values = route_colors) +
  labs(title = "Departures from Times Square on 08/23/18")

## ----fig.width=6, fig.height=6------------------------------------------------
departures_180823_sub_7to8 <- departures_180823 %>% 
  filter(stop_id %in% c("127N", "127S")) %>% 
  filter(departure_time >= hms::hms(hours = 7) & departure_time <= hms::hms(hours = 8))

ggplot(departures_180823_sub_7to8) + theme_bw() +
  geom_point(aes(y=trip_headsign, x=departure_time, color = route_short_name), size = 1) +
  scale_x_time(breaks = seq(7*3600, 9*3600, 300), labels = scales::time_format("%H:%M")) +
  scale_y_discrete(drop = F) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "bottom") +
  labs(title = "Departures from Times Square on 08/23/18") +
  facet_wrap(~stop_id, ncol = 1)

