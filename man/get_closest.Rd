\name{get_closest}
\alias{get_closest}
\title{Better Rainfall Forecast for Grain Growers API connection}
\usage{
  get_closest(station_id, single = F, dist = 0.5,
    overlap = 365L, max_st = 10)
}
\arguments{
  \item{station_id}{ID of the farm station.}

  \item{single}{when \code{TRUE} the function returns the
  closest single BoM station closest to the farm station
  with id \code{station_id}. When \code{FALSE} (default)
  the function returns a group of BoM stations.}

  \item{dist}{numeric representing the search radius in
  degrees.}

  \item{overlap}{integer representing record date overlap
  between the farm station and the BoM station (days).}

  \item{max_st}{maximum number of stations.}
}
\value{
  \code{data.frame}
}
\description{
  This function gets the closest BoM stations from the
  specified farm station ID
}

