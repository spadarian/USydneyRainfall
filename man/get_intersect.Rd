\name{get_intersect}
\alias{get_intersect}
\title{Better Rainfall Forecast for Grain Growers API connection}
\usage{
  get_intersect(station_id, ref_station_id)
}
\arguments{
  \item{station_id}{ID of the station.}

  \item{ref_station_id}{ID of the BoM station.}

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
  This function gets rainfall records (year sum) from user
  station and BoM station, of intersecting years.
}

