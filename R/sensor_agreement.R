library(tidyverse)


# Pass in a tibble containing all PA data from all files read in. Returns a list
# with named variables.
internal_agreement <- function(df) {

  # results <- c()
  # filenames <- c()
  # mac_addresses <- c()
  # ids <- c()
  # first_datetimes <- c()
  # file_lengths <- c()
  # a_means <- c()
  # b_means <- c()
  # a_to_b_means_ratio <- c()
  # large_means_diff <- c()

  #Add 1 to each value before log transform
  df$pm2_5_atm <- df$pm2_5_atm + 1
  df$pm2_5_atm_b <- df$pm2_5_atm_b + 1

  #Take logs of the data (as PM isn't normally distributed so one bad data point
  # can spoil a whole day)
  log_df <- data.frame(log(df$pm2_5_atm))
  log_df$log_b <- log(df$pm2_5_atm_b)

  #Linear model of the logged data
  linMod <- lm(log.df.pm2_5_atm.~log_b, data=log_df)

  #Get R^2 values
  r_squared <- summary(linMod)$r.squared

  #Get first datetime
  first_datetime <- as.character(df$UTCDateTime[1])

  #Get length of file
  file_length <- nrow(df)

  #Get sensor means
  a_mean <- mean(df$pm2_5_atm)
  b_mean <- mean(df$pm2_5_atm_b)
  a_to_b_means_ratio <- mean(df$pm2_5_atm)/mean(df$pm2_5_atm_b)
  diff_more_than_five <- FALSE
  diff_greater_than_twenty_pc <- FALSE
  large_difference_in_means <- FALSE

  if ((mean(df$pm2_5_atm) - mean(df$pm2_5_atm_b)) > 5 || (mean(df$pm2_5_atm) - mean(df$pm2_5_atm_b)) < -5){
    diff_more_than_five <- TRUE
  }

  if (a_to_b_means_ratio > 1.2 || a_to_b_means_ratio < 0.8) {
    diff_greater_than_twenty_pc <- TRUE
  }

  if (diff_more_than_five == TRUE && diff_greater_than_twenty_pc == TRUE) {
    large_difference_in_means <- TRUE
  }

  results <- list(data_length=file_length,
                  r_squared=r_squared,
                  a_mean=a_mean,
                  b_mean=b_mean,
                  a_to_b_means_ratio=a_to_b_means_ratio,
                  diff_more_than_five=diff_more_than_five,
                  diff_greater_than_twenty_pc=diff_greater_than_twenty_pc,
                  large_difference_in_means = large_difference_in_means)

  return(results)
}


# Sensor agreement by logged linear model
get_sensor_agreement <- function(tbl, agreement_cols=c("pm2_5_atm", "pm2_5_atm_b")) {

  #Add 1 to each value before log transform
  # print(as.numeric(unlist(tbl[,agreement_cols[1]])))
  col1 <- as.numeric(unlist(tbl[,agreement_cols[1]])) + 1
  col2 <- as.numeric(unlist(tbl[,agreement_cols[2]])) + 1
  # print(col1)

  #Log transform correcting for right skew
  logged_values <- tbl
  # print("yo")
  logged_values$log_col1 <- as.numeric(unlist(log(col1)))
  logged_values$log_col2 <- as.numeric(unlist(log(col2)))
  # print("bo")

  #Linear model
  # print(logged_values$log_col1)
  linMod <- lm(log_col1~log_col2, data=logged_values)

  #Get R^2 values
  results <- summary(linMod)$r.squared
  return(results)
}
