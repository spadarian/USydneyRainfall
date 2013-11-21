\name{get_refdata}
\alias{get_refdata}
\title{Better Rainfall Forecast for Grain Growers API connection}
\usage{
  get_refdata(ref_station_id, time_frame = "All",
    relative = F)
}
\arguments{
  \item{ref_station_id}{ID of the BoM station.}

  \item{time_frame}{number of days of to extract. When set
  to \code{'All'} the function returns all the records of
  the station.}

  \item{relative}{used when \code{time_frame != 'All'}. If
  \code{TRUE} the numbers of days (\code{time_frame}) are
  considered from the last record date of the station. If
  \code{FALSE} (default) the numbers of days are considered
  from the current date.}
}
\value{
  \code{data.frame}
}
\description{
  This function gets rainfall records from a BoM station
}

