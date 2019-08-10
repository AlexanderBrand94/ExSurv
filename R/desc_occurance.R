#'A function for the description of experience sampling data
#'
#' This function gives discriptions of experience sampling data
#' @param data the cleared dataframe
#' @keywords sampling experience descriptives 
#' @export
#' @examples
#' desc_occurance(dat1)
#' 

desc_occurance <- function(data){
  obs_day <- data %>% dplyr::distinct(beep) %>% nrow()
  data %>% dplyr::group_by(day) %>%
    dplyr::summarize_all(.funs = "mean")
}








