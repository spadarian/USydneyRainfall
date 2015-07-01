#' Better Rainfall Forecast for Grain Growers API connection
#'
#' This function gets rainfall records from a station
#'
#' @param station_id ID of the station (or BoM station).
#' @param time_frame number of days of to extract. When set to \code{'All'} the function returns all the records of the station.
#' @param relative used when \code{time_frame != 'All'}. If \code{TRUE} the numbers of days (\code{time_frame}) are considered from the last record date of the station. If \code{FALSE} (default) the numbers of days are considered from the current date.
#' @param type Tipe of station. If \code{'ref'} (default) the function returns data from the BoM station with id \code{station_id}. If \code{'st'} it returns data from the user station.
#' @param year The function returns data from that specific year.
#' @return \code{data.frame}
#' @importFrom RCurl getURL
#' @importFrom RJSONIO fromJSON
#' @export

get_raindata <- function(station_id,time_frame='All',relative=F,type='ref',year=NULL) {
  if (length(station_id) > 0) {
    response <- lapply(station_id, function(x) {
      json <- getURL(paste0('https://rainfall-aerdm.sydney.edu.au/get-raindata/?station_id=',x,'&type=',type,'&tf=',time_frame,'&rel=',substr(relative,1,1),'&year=',paste0(year,collapse=',')))
      ans <- fromJSON(json,simplify=T)
      if (length(ans$data$station$records) == 0) return(NULL)
      data <- rbindList(ans$data$station$records)
      data$rain_mm <- as.numeric(data$rain_mm)
      data
    })
    if (length(station_id) == 1) response[[1]] else response
  }
}
