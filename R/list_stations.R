#' Better Rainfall Forecast for Grain Growers API connection
#'
#' This function gets list of user stations.
#'
#' @return \code{data.frame}
#' @importFrom RCurl getURL
#' @importFrom RJSONIO fromJSON
#' @export

list_stations <- function() {
  json <- getURL('https://rainfall-aerdm.sydney.edu.au/stations/?format=json',.opts=list(cookiefile=get('.cookie_file',envir=.GlobalEnv)))
  ans <- fromJSON(json,simplify=T)$data[[1]]
  loc <- t(sapply(ans,`[[`,'location'))
  rest <- lapply(ans, function(x) {x$location <- NULL; x})
  cbind(rbindList(rest),loc)[,c('id','name','latitude','longitude')]
}
