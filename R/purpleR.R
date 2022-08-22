library(ggplot2)
library(tidyverse)
library(lubridate)
library(readxl)
library(tools)


# This is a convenience function that calls a number of others below. I use it
# when prepping this sort of data for general consumption. It's messy I'm afraid.
standard_file_process <- function(tbl) {
  print(tbl$mean_pm2.5$filename[1])
  print(quantile(tbl$sensor_agreements$sensor_agreement))
  tbl <- tibble(tbl$mean_pm2.5) %>% filter(!is.na(datetime), !is.na(average_pm2.5), !is.nan(average_pm2.5))
  tbl_five <- get_five_minute_averages(tbl)
  tbl_five <- count_five_minute_intervals(tbl_five, threshold=22)
  print(get_valid_days(tbl_five) %>% count(meets_threshold))
  # tbl_five <- tbl_five %>% filter(meets_threshold == "above") # uncomment to get only data above threshold
  print(mean(tbl_five$average_pm2.5, na.rm=TRUE))
  print(sum(is.na(tbl_five$average_pm2.5)))
  return(tbl_five)
}







#
# # Read in a PA csv file. There are a lot of hacks in this to get them to play
# # nicely, I'm sorry about that! I've tried to document them but I'm sure I
# # haven't fully succeeded...
# read_PA_file <- function(file_location,
#                          output_location=NA,
#                          averaging="none",
#                          humidity_correction=FALSE,
#                          atm_or_cf="atm",
#                          timezone="UTC") {
#
#   print(file_location)
#
#   # read all lines in file
#   rl <- read_lines(file_location)
#
#   # dump lines that contain UTCDateTime or current_dewpoint (so we get rid of headers) - believe it or not the current_dewpoint thing corrects a bug...
#   lines_with_utcdatetime <- str_detect(rl, "UTCDateTime") | str_detect(rl, "current_dewpoint") | str_detect(rl, "csv")
#   rl_no_header <- rl[which(!lines_with_utcdatetime)]
#
#   # is the vector now empty? if so, dump it
#   if (length(rl_no_header) == 0) {
#     print("no lines in file!")
#     return(NULL)
#   }
#
#   # remove empty lines
#   rl_no_header <- rl_no_header[rl_no_header != ""]
#   rl_no_header <- map_chr(rl_no_header, iconv, from="UTF-8", to="UTF-8", sub="")
#
#   # read the first line of the vector in as a CSV file
#   first_line <- read.csv(textConnection(rl_no_header[1]), header = FALSE)
#
#   # get version number
#   version_number <- as.numeric(first_line["V3"])
#
#   if (is.na(version_number)) {
#     print("version number is NA, error in reading of some kind")
#     print(file_location)
#     return(NULL)
#   }
#
#   if (version_number < 4.11) {
#     column_names <- c('UTCDateTime', 'mac_address', 'firmware_ver', 'hardware', 'current_temp_f', 'current_humidity', 'current_dewpoint_f', 'pressure', 'adc', 'mem', 'rssi', 'uptime', 'pm1_0_atm', 'pm2_5_atm', 'pm10_0_atm', 'pm1_0_cf_1', 'pm2_5_cf_1', 'pm10_0_cf_1', 'p_0_3_um', 'p_0_5_um', 'p_1_0_um', 'p_2_5_um', 'p_5_0_um', 'p_10_0_um', 'pm1_0_atm_b', 'pm2_5_atm_b', 'pm10_0_atm_b', 'pm1_0_cf_1_b', 'pm2_5_cf_1_b', 'pm10_0_cf_1_b', 'p_0_3_um_b', 'p_0_5_um_b', 'p_1_0_um_b', 'p_2_5_um_b', 'p_5_0_um_b', 'p_10_0_um_b')
#   } else {
#     column_names <- c('UTCDateTime', 'mac_address', 'firmware_ver', 'hardware', 'current_temp_f', 'current_humidity', 'current_dewpoint_f', 'pressure', 'adc', 'mem', 'rssi', 'uptime', 'pm1_0_atm', 'pm2_5_atm', 'pm10_0_atm', 'pm1_0_cf_1', 'pm2_5_cf_1', 'pm10_0_cf_1', 'pm2.5_aqi_atm', 'pm2.5_aqi_cf_1', 'p_0_3_um', 'p_0_5_um', 'p_1_0_um', 'p_2_5_um', 'p_5_0_um', 'p_10_0_um', 'pm1_0_atm_b', 'pm2_5_atm_b', 'pm10_0_atm_b', 'pm1_0_cf_1_b', 'pm2_5_cf_1_b', 'pm10_0_cf_1_b', 'pm2.5_aqi_atm_b', 'pm2.5_aqi_cf_1_b', 'p_0_3_um_b', 'p_0_5_um_b', 'p_1_0_um_b', 'p_2_5_um_b', 'p_5_0_um_b', 'p_10_0_um_b', 'gas')
#   }
#
#   # read_csv doesn't handle single rows very well - it needs a newline character to detect it's data rather than a filename
#   num_rows <- length(rl_no_header)
#   if (num_rows == 1) {
#     rl_no_header[1] <- paste(rl_no_header[1], "\n", sep="")
#   }
#
#   tbl <- read_csv(rl_no_header,
#                   col_names = column_names,
#                   guess_max = 5, #this shouldn't work - we need to get lucky so the erroneous rows aren't in the first five
#                   col_types = c(
#                     "UTCDateTime" = col_character(),
#                     "mac_address" = col_character(),
#                     "hardware" = col_character(),
#                     "adc" = col_character(),
#                     "pm2_5_atm" = col_number(),
#                     "pm10_0_atm" = col_number(),
#                     "pm2_5_cf_1" = col_number()
#                   )
#   )
#
#
#   # Get rid of any non-UTF-8 characters in the datetime column
#   tbl$UTCDateTime <- iconv(tbl$UTCDateTime, "UTF-8", "UTF-8", sub='')
#   tbl$pm2_5_atm <- iconv(tbl$pm2_5_atm, "UTF-8", "UTF-8", sub='')
#   tbl$pm2_5_atm_b <- iconv(tbl$pm2_5_atm_b, "UTF-8", "UTF-8", sub='')
#
#   tbl$datetime <- as_datetime(tbl$UTCDateTime, tz="UTC")
#
#   #get internal sensor agreement (an R^2 value)
#   sensor_agreement <- NaN
#   if (atm_or_cf == "atm") {
#     sensor_agreement <- get_sensor_agreement(tbl)
#   }
#   if (atm_or_cf == "cf") {
#     sensor_agreement <- get_sensor_agreement(tbl, agreement_cols = c("pm2_5_cf_1", "pm2_5_cf_1_b"))
#   }
#
#   #Choose which columns to use for averaging - we can make this more complex in future
#   average_cols <- c("pm2_5_atm", "pm2_5_atm_b")
#   if (atm_or_cf=="cf") {
#     average_cols <- c("pm2_5_cf_1", "pm2_5_cf_1_b")
#   }
#
#   # Take the average of both columns
#   # print(typeof(tbl[,average_cols[1]]))
#   # return(tbl[,average_cols[2]])
#   sensor_a <- unlist(tbl[,average_cols[1]])
#   sensor_b <- unlist(tbl[,average_cols[2]])
#   sensors <- tibble(as.numeric(sensor_a))
#   # sensors$sensor_a <- rbind(sensors,sensor_a)
#   sensors$sensor_b <- as.numeric(sensor_b)
#   # print(sensors)
#   tbl$average_pm2.5 <- rowMeans(sensors, na.rm = TRUE)
#   # print(tbl[,average_cols[2]])
#   hourly_averages <- tbl %>% mutate(date=date(datetime), hour=hour(datetime)) %>% group_by(date, hour) %>% summarise(pm2.5=mean(average_pm2.5, na.rm=TRUE))
#   hourly_averages <- hourly_averages %>% mutate(datetime_rightbounded=make_datetime(year=year(date), month=month(date), day=day(date), hour=hour+1))
#   hourly_averages <- hourly_averages %>% mutate(datetime_leftbounded=make_datetime(year=year(date), month=month(date), day=day(date), hour=hour))
#   daily_averages <-  tbl %>% mutate(date=date(datetime)) %>% group_by(date) %>% summarise(pm2.5=mean(average_pm2.5, na.rm = TRUE))
#   daily_averages <- tibble(daily_averages)
#   hourly_averages <- tibble(hourly_averages)
#   return(list("sensor_agreement"=sensor_agreement, "mean_pm2.5"=tbl %>% select(datetime, average_pm2.5), "hourly_averages"=hourly_averages, "daily_averages"=daily_averages))
# }

