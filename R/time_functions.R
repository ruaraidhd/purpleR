library(tidyverse)
library(lubridate)
library(xts)
library(timetk)

# Pass data in, average it out by every five minutes (with no interpolation)
get_five_minute_averages <- function(tbl) {
  if(count(tbl) == 0) {
    return (tbl)
  }
  tbl_xts <- xts(tbl$average_pm2.5, tbl$datetime, tzone = tz(tbl$datetime))
  five_minutes <- align.time(period.apply(tbl_xts, endpoints(tbl_xts, "mins", k=5), mean, na.rm=T), 5*60)
  five_minutes <- tk_tbl(five_minutes, rename_index = "datetime") %>% rename(average_pm2.5 = value)
  return(five_minutes)
}

# assign days a logical for validity based on the number of hours with coverage
get_valid_days <- function(tbl, threshold=22) {
  days_by_threshold <- tbl %>% mutate(day=date(datetime)) %>% group_by(day) %>% summarise(count_of_periods=n())
  days_by_threshold <- days_by_threshold %>% mutate(meets_threshold=if_else(count_of_periods >= (threshold/24)*(1440/5), "above", "below"))
  return(days_by_threshold)
}

# Take a tibble with datetime and average_pm2.5 at five minute intervals,
# return a record for each value saying whether it should be included in means
# based on a threshold of hours covered
count_five_minute_intervals <- function(tbl, threshold=22) {
  days_by_threshold <- get_valid_days(tbl, threshold)
  tbl <- tbl %>% mutate(day=date(datetime))
  return(left_join(tbl, days_by_threshold))
}




# Cut off first and last days
cut_first_and_last_days <- function(tbl, tzone="Europe/London") {
  this_data <- tbl
  first_day <- yday(as_datetime(as.numeric(this_data[1, "datetime"]), tz=tzone))
  last_day <- yday(as_datetime(as.numeric(this_data[nrow(this_data), "datetime"]), tz=tzone))
  cut_data <- this_data %>% filter(yday(datetime) != first_day) %>% filter(yday(datetime) != last_day)
  return(cut_data)
}

# Return data from the day with the highest average
choose_worst_day <- function(tbl, tzone="Europe/London", cut_selection="Yes") {
  if (cut_selection == "Yes"){
    tbl$daily_averages <- tbl$daily_averages[-c(nrow(tbl$daily_averages)),]
    tbl$daily_averages <- tbl$daily_averages[-c(1),]
  }
  dt <- tbl$daily_averages[which.max(tbl$daily_averages$pm2.5), "date"]
  worst_day <- tbl$mean_pm2.5 %>% filter(as_date(datetime, tz=tzone) == dt)
  print("worst_day calculated")
  return(worst_day)
}
