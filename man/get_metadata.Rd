\name{get_metadata}
\alias{get_metadata}
\title{Better Rainfall Forecast for Grain Growers API connection}
\usage{
  get_metadata(station_id, type = "ref")
}
\arguments{
  \item{station_id}{ID of the station.}

  \item{type}{when set to \code{'ref'} (default) the
  function returns information from a BoM station. When set
  to \code{'st'} it returns information from a farm
  station.}
}
\value{
  \code{data.frame}
}
\description{
  This function gets metadata related with a station.
}

