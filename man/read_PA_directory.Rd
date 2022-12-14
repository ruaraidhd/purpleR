% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_data.R
\name{read_PA_directory}
\alias{read_PA_directory}
\title{Read all PA files in a directory}
\usage{
read_PA_directory(
  directory,
  timezone = "Europe/London",
  atm_or_cf = "atm",
  slope = 1,
  intercept = 0
)
}
\arguments{
\item{directory}{path to directory}

\item{timezone}{timezone as an Olson name (default: "Europe/London)}

\item{atm_or_cf}{which PA pm2.5 estimate to use ("atm" or "cf") (default: "atm")}

\item{slope}{slope for correction factor (default: 1)}

\item{intercept}{intercept for correction factor (default: 0)}
}
\value{
List of tibbles, including: mean_pm2.5 (averaged between sensors every
two minutes), hourly_averages, daily_averages, daily_count (count of records
per day), pa_agreement (information on sensor agreement) and original_file_data
(data as contained the original files, including headers).
}
\description{
Reads all PA files (ending .csv) in a directory
}
