% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/USGS_Discharge_Downloader.R
\name{discharge_download}
\alias{discharge_download}
\title{Download the average daily discharge for a vector of gages from the USGS site.}
\usage{
discharge_download(
  gages,
  discharge_directory,
  start_date = "1951-10-01",
  end_date = "2011-09-30",
  missing_days = 14
)
}
\arguments{
\item{gages}{a vector of gages to download discharge data for.}

\item{discharge_directory}{The directory where the raw discharge files are located.}

\item{start_date}{The first date to download data for.}

\item{end_date}{The last date to download data for.}

\item{missing_days}{The number of missing days in the discharge record that is acceptable.}
}
\description{
Download the average daily discharge for a vector of gages from the USGS site.
}
