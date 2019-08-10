##'A function for the description of experience sampling data with lags
#'
#' This function gives discriptions of experience sampling data with lags
#' @param data the cleared dataframe
#' @param lag the number of lags, default = 2
#' @keywords sampling experience descriptives
#' @export
#' @examples
#' desc_occurance_rolling(dat1, lag = 3)
#'

desc_occurance_rolling <- function(data, lag = 2){
  library(zoo)
  d1<- data %>% group_by(day, id) %>%
    summarize_all(.funs = "mean") %>%
    mutate("occurances" = Y * 3) %>%
    select(id, occurances, time) %>%
    group_by(day) %>%
    summarise("occurances" = sum(occurances))
  d1$rollmean<- rollmean(zoo(d1$occurances, d1$day),lag, fill = NA)
  d1$rollmedian<- rollmedian(zoo(d1$occurances, d1$day),lag, fill = NA)
  return(d1)
}
