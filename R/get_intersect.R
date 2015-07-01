#' Better Rainfall Forecast for Grain Growers API connection
#'
#' This function gets rainfall records (year sum) from user station and BoM station, of intersecting years.
#'
#' @param station_id ID of the station.
#' @param ref_station_id ID of the BoM station.
#' @param relative used when \code{time_frame != 'All'}. If \code{TRUE} the numbers of days (\code{time_frame}) are considered from the last record date of the station. If \code{FALSE} (default) the numbers of days are considered from the current date.
#' @return \code{data.frame}
#' @importFrom RCurl getURL
#' @importFrom RJSONIO fromJSON
#' @export

get_intersect <- function(station_id,ref_station_id) {
  json <- getURL(paste0('https://rainfall-aerdm.sydney.edu.au/graph-station/',station_id,'?ref_station_id=',ref_station_id))
  ans <- fromJSON(json,simplify=T)
  rbindList(ans$graph_data)
}
