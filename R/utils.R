#' Better Rainfall Forecast for Grain Growers API connection
#' 
#' These functions are internally used
#'

rbindList <- function(df_list,add_to_rownames=0){
  result.df <- lapply(seq.int(df_list[[1]]), function(i) do.call(c, lapply(df_list, function(x) x[[i]])))
  names(result.df) <- names(df_list[[1]])
  attr(result.df, "row.names") <- c(NA, -length(result.df[[1]]))
  attributes(result.df)$row.names <- seq(1+add_to_rownames,length(attributes(result.df)$row.names)+add_to_rownames)
  class(result.df) <- "data.frame"
  result.df
}