library(tidyverse)
library(readxl)



#' Read all PA files in a directory
#'
#' Reads all PA files (ending .csv) in a directory
#'
#' @param directory path to directory
#' @param timezone timezone as an Olson name (default: "Europe/London)
#' @param atm_or_cf which PA pm2.5 estimate to use ("atm" or "cf") (default: "atm")
#' @param slope slope for correction factor (default: 1)
#' @param intercept intercept for correction factor (default: 0)
#'
#' @return List of tibbles, including: mean_pm2.5 (averaged between sensors every
#' two minutes), hourly_averages, daily_averages, daily_count (count of records
#' per day), pa_agreement (information on sensor agreement) and original_file_data
#' (data as contained the original files, including headers).
#'
#' @export
read_PA_directory <- function(directory, timezone="Europe/London", atm_or_cf="atm", slope=1, intercept=0) {
  #Get relevant files
  filenames <- list.files(directory, pattern = ".csv$",  full.names = TRUE)
  return(read_PA_files(filenames,
                       timezone = timezone,
                       atm_or_cf = atm_or_cf,
                       slope = slope,
                       intercept = intercept))
}

# Input: a file path or a list of files
read_PA_files <- function(file_locations,
                          output_location=NA,
                          averaging="none",
                          humidity_correction=FALSE,
                          atm_or_cf="atm",
                          timezone="Europe/London",
                          slope=1,
                          intercept=0) {

  all_tbl <- tibble()

  original_file_data <- tibble()

  for (file_location in file_locations) {
    # read all lines in file
    rl <- read_lines(file_location)

    # dump lines that contain UTCDateTime or current_dewpoint (so we get rid of headers) - believe it or not the current_dewpoint thing corrects a bug...
    lines_with_utcdatetime <- str_detect(rl, "UTCDateTime") | str_detect(rl, "current_dewpoint") | str_detect(rl, "csv")
    rl_no_header <- rl[which(!lines_with_utcdatetime)]

    # is the vector now empty? if so, dump it
    if (length(rl_no_header) == 0) {
      print("no lines in file!")
      return(NULL)
    }

    # remove empty lines
    rl_no_header <- rl_no_header[rl_no_header != ""]
    rl_no_header <- map_chr(rl_no_header, iconv, from="UTF-8", to="UTF-8", sub="")
    first_line <- read.csv(textConnection(rl_no_header[1]), header = FALSE)

    # get version number
    version_number <- as.numeric(first_line["V3"])

    if (is.na(version_number)) {
      print("version number is NA, error in reading of some kind")
      print(file_location)
      return(NULL)
    }

    if (version_number < 4) {
      column_names <- c('UTCDateTime', 'mac_address', 'firmware_ver', 'hardware', 'current_temp_f', 'current_humidity', 'current_dewpoint_f', 'pressure', 'adc', 'mem', 'rssi', 'uptime', 'pm1_0_atm', 'pm2_5_atm', 'pm10_0_atm', 'pm1_0_cf_1', 'pm2_5_cf_1', 'pm10_0_cf_1', 'p_0_3_um', 'p_0_5_um', 'p_1_0_um', 'p_2_5_um', 'p_5_0_um', 'p_10_0_um', 'pm1_0_atm_b', 'pm2_5_atm_b', 'pm10_0_atm_b', 'pm1_0_cf_1_b', 'pm2_5_cf_1_b', 'pm10_0_cf_1_b', 'p_0_3_um_b', 'p_0_5_um_b', 'p_1_0_um_b', 'p_2_5_um_b', 'p_5_0_um_b', 'p_10_0_um_b')
    } else {
      column_names <- c('UTCDateTime', 'mac_address', 'firmware_ver', 'hardware', 'current_temp_f', 'current_humidity', 'current_dewpoint_f', 'pressure', 'adc', 'mem', 'rssi', 'uptime', 'pm1_0_atm', 'pm2_5_atm', 'pm10_0_atm', 'pm1_0_cf_1', 'pm2_5_cf_1', 'pm10_0_cf_1', 'pm2.5_aqi_atm', 'pm2.5_aqi_cf_1', 'p_0_3_um', 'p_0_5_um', 'p_1_0_um', 'p_2_5_um', 'p_5_0_um', 'p_10_0_um', 'pm1_0_atm_b', 'pm2_5_atm_b', 'pm10_0_atm_b', 'pm1_0_cf_1_b', 'pm2_5_cf_1_b', 'pm10_0_cf_1_b', 'pm2.5_aqi_atm_b', 'pm2.5_aqi_cf_1_b', 'p_0_3_um_b', 'p_0_5_um_b', 'p_1_0_um_b', 'p_2_5_um_b', 'p_5_0_um_b', 'p_10_0_um_b', 'gas')
    }



    tbl <- read.csv(textConnection(rl_no_header), header = FALSE)
    colnames(tbl) <- column_names[1:ncol(tbl)]
    tbl <- tibble(tbl)
    tbl$datetime <- as_datetime(tbl$UTCDateTime, tz="UTC")
    tbl$datetime <- with_tz(tbl$datetime, tzone = timezone)

    original_file_data <- rbind(original_file_data, tbl)
    average_cols <- c("pm2_5_atm", "pm2_5_atm_b")
    if (atm_or_cf=="cf") {
      average_cols <- c("pm2_5_cf_1", "pm2_5_cf_1_b")
    }

    # Take the average of both columns to provide a mean mass concentration
    sensor_a <- unlist(tbl[,average_cols[1]])
    sensor_b <- unlist(tbl[,average_cols[2]])
    sensors <- tibble(sensor_a)
    sensors$sensor_b <- sensor_b
    tbl$average_pm2.5 <- rowMeans(sensors, na.rm = TRUE)
    all_tbl <- rbind(all_tbl, tbl)
  }

  tbl <- all_tbl

  # correction factors - multiply and add
  tbl$average_pm2.5 <- (tbl$average_pm2.5 * slope) + intercept

  pa_agreement <- internal_agreement(original_file_data)
  hourly_averages <- tbl %>% mutate(date=date(datetime), hour=hour(datetime)) %>% group_by(date, hour) %>% summarise(pm2.5=mean(average_pm2.5, na.rm=TRUE))
  hourly_averages <- hourly_averages %>% mutate(datetime_rightbounded=make_datetime(year=year(date), month=month(date), day=day(date), hour=hour+1))
  hourly_averages <- hourly_averages %>% mutate(datetime_leftbounded=make_datetime(year=year(date), month=month(date), day=day(date), hour=hour))
  daily_averages <-  tbl %>% mutate(date=date(datetime)) %>% group_by(date) %>% summarise(pm2.5=mean(average_pm2.5, na.rm = TRUE))
  daily_averages <- tibble(daily_averages)
  hourly_averages <- tibble(hourly_averages)
  daily_count <- tbl %>% mutate(date=date(datetime)) %>% group_by(date) %>% summarise(daily_count=n())
  daily_count <- tibble(daily_count)
  return(list("mean_pm2.5"=tbl %>% select(datetime, average_pm2.5),
              "hourly_averages"=hourly_averages,
              "daily_averages"=daily_averages,
              "daily_count"=daily_count,
              "pa_agreement"=pa_agreement,
              "original_file_data" = original_file_data))
}
