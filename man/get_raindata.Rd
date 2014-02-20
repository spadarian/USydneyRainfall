\name{get_raindata}
\alias{get_raindata}
\title{Better Rainfall Forecast for Grain Growers API connection}
\usage{
  get_raindata(station_id, time_frame = "All",
    relative = F, type = "ref", year = NULL)
}
\arguments{
  \item{station_id}{ID of the station (or BoM station).}

  \item{time_frame}{number of days of to extract. When set
  to \code{'All'} the function returns all the records of
  the station.}

  \item{relative}{used when \code{time_frame != 'All'}. If
  \code{TRUE} the numbers of days (\code{time_frame}) are
  considered from the last record date of the station. If
  \code{FALSE} (default) the numbers of days are considered
  from the current date.}

  \item{type}{Tipe of station. If \code{'ref'} (default)
  the function returns data from the BoM station with id
  \code{station_id}. If \code{'st'} it returns data from
  the user station.}

  \item{year}{The function returns data from that specific
  year.}
}
\value{
  \code{data.frame}
}
\description{
  This function gets rainfall records from a station
}

