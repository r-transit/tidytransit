## ----setup, include=FALSE-----------------------------------------------------
library(knitr)
library(tidytransit)
library(dplyr)
library(lubridate)
library(ggplot2)
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
local_gtfs_path <- system.file("extdata", "google_transit_nyc_subway.zip", package = "tidytransit")
gtfs <- read_gtfs(local_gtfs_path)
# gtfs <- read_gtfs("http://web.mta.info/developers/data/nyct/subway/google_transit.zip")

## -----------------------------------------------------------------------------
head(gtfs$.$dates_services)

## -----------------------------------------------------------------------------
holidays = tribble(~date, ~holiday,
  ymd("2018-07-04"), "Independence Day",
  ymd("2018-09-03"), "Labor Day")

calendar = tibble(date = unique(gtfs$.$dates_services$date)) %>% 
  mutate(
    weekday = (function(date) {
      c("Sunday", "Monday", "Tuesday", 
        "Wednesday", "Thursday", "Friday", 
        "Saturday")[as.POSIXlt(date)$wday + 1]
    })(date)
  )

calendar <- calendar %>% left_join(holidays, by = "date")
head(calendar)

## -----------------------------------------------------------------------------
gtfs <- set_servicepattern(gtfs)

## -----------------------------------------------------------------------------
head(gtfs$.$servicepatterns)

## -----------------------------------------------------------------------------
head(gtfs$.$dates_servicepatterns)

# number of service ids used
n_services <- length(unique(gtfs$trips$service_id)) # 52

# unique date patterns 
n_servicepatterns <- length(unique(gtfs$.$servicepatterns$servicepattern_id)) # 3

## ----fig.height=4, fig.width=7------------------------------------------------
date_servicepattern_table <- gtfs$.$dates_servicepatterns %>% left_join(calendar, by = "date")

ggplot(date_servicepattern_table) + theme_bw() + 
  geom_point(aes(x = date, y = servicepattern_id, color = weekday), size = 1) + 
  scale_x_date(breaks = scales::date_breaks("1 month")) + theme(legend.position = "bottom")

## -----------------------------------------------------------------------------
suggest_servicepattern_name = function(dates, calendar) {
  servicepattern_calendar = tibble(date = dates) %>% left_join(calendar, by = "date")
  
  # all normal dates without holidays
  calendar_normal = servicepattern_calendar %>% filter(is.na(holiday))
  
  # create a frequency table for all calendar dates without holidays
  weekday_freq = sort(table(calendar_normal$weekday), decreasing = T)
  n_weekdays = length(weekday_freq)
  
  # all holidays that are not covered by normal weekdays anyways
  calendar_holidays <- servicepattern_calendar %>% filter(!is.na(holiday)) %>% filter(!(weekday %in% names(weekday_freq)))

  if(n_weekdays == 7) {
    pattern_name = "Every day"
  }
  # Single day service
  else if(n_weekdays == 1) {
    wd = names(weekday_freq)[1]
    # while paste0(weekday, "s") is easier, this solution can be used for other languages
    pattern_name = c("Sunday"  = "Sundays", 
        "Monday"    = "Mondays", 
        "Tuesday"   = "Tuesdays", 
        "Wednesday" = "Wednesdays",
        "Thursday"  = "Thursdays",  
        "Friday"    = "Fridays",  
        "Saturday"  = "Saturdays")[wd]
  } 
  # Weekday Service
  else if(n_weekdays == 5 && 
      length(intersect(names(weekday_freq), 
        c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))) == 5) {
    pattern_name = "Weekdays"
  }
  # Weekend
  else if(n_weekdays == 2 && 
      length(intersect(names(weekday_freq), c("Saturday", "Sunday"))) == 2) {
    pattern_name = "Weekends"
  }
  # Multiple weekdays that appear regularly
  else if(n_weekdays >= 2 && (max(weekday_freq) - min(weekday_freq)) <= 1) {
    wd = names(weekday_freq)
    ordered_wd = wd[order(match(wd, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))]
    pattern_name = paste(ordered_wd, collapse = ", ")
  } 
  # default
  else {
    pattern_name = paste(weekday_freq, names(weekday_freq), sep = "x ", collapse = ", ")
  }
  
  # add holidays
  if(nrow(calendar_holidays) > 0) {
    pattern_name <- paste0(pattern_name, " and ", paste(calendar_holidays$holiday, collapse = ", "))
  }
  
  pattern_name <- paste0(pattern_name, " (", min(dates), " - ", max(dates), ")") 

  return(pattern_name)
}

## -----------------------------------------------------------------------------
servicepattern_names = gtfs$.$dates_servicepatterns %>% 
  group_by(servicepattern_id) %>% summarise(
    servicepattern_name = suggest_servicepattern_name(date, calendar)
  )

print(servicepattern_names)

## ----fig.height=4, fig.width=7------------------------------------------------
dates = gtfs$.$dates_servicepatterns
dates$wday <- lubridate::wday(dates$date, label = T, abbr = T, week_start = 7)
dates$week_nr <- lubridate::week(dates$date)

dates <- dates %>% group_by(week_nr) %>% summarise(week_first_date = min(date)) %>% right_join(dates, by = "week_nr")

week_labels = dates %>% select(week_nr, week_first_date) %>% unique()

ggplot(dates) + theme_bw() +
  geom_tile(aes(x = wday, y = week_nr), color = "#747474") +
  scale_x_discrete(drop = F) +
  scale_y_continuous(trans = "reverse", labels = week_labels$week_first_date, breaks = week_labels$week_nr) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = NULL, y = "Date of Sundays") +
  facet_wrap(~servicepattern_id, nrow = 1)

## ----fig.height=4, fig.width=7------------------------------------------------
trips_servicepattern = left_join(select(gtfs$trips, trip_id, service_id), gtfs$.$servicepatterns, by = "service_id")
trip_dates = left_join(gtfs$.$dates_servicepatterns, trips_servicepattern, by = "servicepattern_id", relationship = "many-to-many")

trip_dates_count = trip_dates %>% group_by(date) %>% summarise(count = dplyr::n()) 
trip_dates_count$weekday <- lubridate::wday(trip_dates_count$date, label = T, abbr = T, week_start = 7)
trip_dates_count$day_of_month <- lubridate::day(trip_dates_count$date)
trip_dates_count$first_day_of_month <- lubridate::wday(trip_dates_count$date - trip_dates_count$day_of_month,  week_start = 7)
trip_dates_count$week_of_month <- ceiling((trip_dates_count$day_of_month - as.numeric(trip_dates_count$weekday) - trip_dates_count$first_day_of_month) / 7)
trip_dates_count$month <- lubridate::month(trip_dates_count$date, label = T, abbr = F)

ggplot(trip_dates_count, aes(x = weekday, y = -week_of_month)) + theme_bw() +
  geom_tile(aes(fill = count, colour = "grey50")) +
  geom_text(aes(label = day_of_month), size = 3, colour = "grey20") +
  facet_wrap(~month, ncol = 3) +
  scale_fill_gradient(low = "cornsilk1", high = "DarkOrange", na.value="white")+
    scale_color_manual(guide = "none", values = "grey50") +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  theme(panel.grid = element_blank()) +
  labs(x = NULL, y = NULL, fill = "# trips") +
  coord_fixed()

