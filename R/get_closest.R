#' Better Rainfall Forecast for Grain Growers API connection
#' 
#' This function gets the closest BoM stations from the specified farm station ID
#' 
#' @param station_id ID of the farm station.
#' @param single when \code{TRUE} the function returns the closest single BoM station closest to the farm station with id \code{station_id}. When \code{FALSE} (default) the function returns a group of BoM stations.
#' @param dist numeric representing the search radius in degrees.
#' @param overlap integer representing record date overlap between the farm station and the BoM station (days).
#' @return \code{data.frame}
#' @importFrom RCurl getURL
#' @importFrom RJSONIO fromJSON
#' @export

get_closest <- function(station_id,single=F,dist=0.5,overlap=365L) {
  if (!(is.logical(single)&is.numeric(dist)&is.integer(overlap))) stop('Check the optional parameters')
  json <- getURL(paste0('http://10.65.26.5:8000/get-closest/',station_id,'&single=',substr(single,1,1),'&dist=',dist,'&overlap=',overlap))
  ans <- fromJSON(json,simplify=T)$data$ref_station
  loc <- t(sapply(ans,`[[`,'location'))
  rest <- lapply(ans, function(x) {x$location <- NULL; x})
  cbind(rbindList(rest),loc)[,c('id','code','name','latitude','longitude','hq')]
}