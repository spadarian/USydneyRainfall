#' Better Rainfall Forecast for Grain Growers API connection
#' 
#' This function is used to use the API
#' 
#' @param username username for the API
#' @param password password for the API
#' @importFrom RCurl getURL
#' @importFrom RJSONIO fromJSON
#' @export

rainfall_login <- function(username,password){
  assign('.cookie_file',tempfile(),envir=.GlobalEnv)
  step <- system(paste0('curl -s -c ',get('.cookie_file',envir=.GlobalEnv),' 10.65.26.5:8000/login/'),intern=T)
  step2 <- regmatches(step,regexpr("name[=][\'|\"]csrfmiddlewaretoken[\'|\"] value[=][\'|\"].*[\'|\"]",step))
  step3 <- gsub("'",'',unlist(regmatches(step2,gregexpr("'.*?'",step2))))
  step4 <- paste0(step3[1],'=',step3[2],'&username=',username,'&password=',password)
  ans <- system(paste0("curl -s -b ",get('.cookie_file',envir=.GlobalEnv)," -c ",get('.cookie_file',envir=.GlobalEnv)," -d '",step4,"' -X POST -H 'Content-Type: application/x-www-form-urlencoded' 10.65.26.5:8000/login/"),intern=T)
  any(grepl('Logged in as',ans))
}