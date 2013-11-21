#' Better Rainfall Forecast for Grain Growers API connection
#' 
#' This function gets metadata related with a station.
#' 
#' @param station_id ID of the station.
#' @param type when set to \code{'ref'} (default) the function returns information from a BoM station. When set to \code{'st'} it returns information from a farm station.
#' @return \code{data.frame}
#' @importFrom RCurl getURL
#' @importFrom RJSONIO fromJSON
#' @export

get_metadata <- function(station_id, type='ref') {
  if (length(station_id) > 0) {
    response <- lapply(station_id, function(x){
      json <- getURL(paste0('http://10.65.26.5:8000/get-metadata/',x,'&type=',type))
      ans <- fromJSON(json,simplify=T)$data
      loc <- t(sapply(ans,`[[`,'location'))
      rest <- lapply(ans, function(x) {x$location <- NULL; x})
      cbind(rbindList(rest),loc)[,c('id','code','name','latitude','longitude','hq')]
    })
    rbindList(response)
  }
}