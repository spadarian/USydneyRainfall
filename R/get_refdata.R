#' Better Rainfall Forecast for Grain Growers API connection
#' 
#' This function gets rainfall records from a BoM station
#' 
#' @param ref_station_id ID of the BoM station.
#' @param time_frame number of days of to extract. When set to \code{'All'} the function returns all the records of the station.
#' @param relative used when \code{time_frame != 'All'}. If \code{TRUE} the numbers of days (\code{time_frame}) are considered from the last record date of the station. If \code{FALSE} (default) the numbers of days are considered from the current date.
#' @return \code{data.frame}
#' @importFrom RCurl getURL
#' @importFrom RJSONIO fromJSON
#' @export

get_refdata <- function(ref_station_id,time_frame='All',relative=F) {
  if (length(ref_station_id) > 0) {
    response <- lapply(ref_station_id, function(x) {
      json <- getURL(paste0('http://10.65.26.5:8000/get-refdata/',x,'&tf=',time_frame,'&rel=',substr(relative,1,1)))
      test <- fromJSON(json,simplify=T)
      rbindList(test$data$ref_station$records)
    })
    if (length(ref_station_id) == 1) response[[1]] else response
  }
}