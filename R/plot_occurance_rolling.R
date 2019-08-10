#'A function for the plotting of occurances in experience sampling data with lagged data
#'
#' This function produces a plot of occurances of experience sampling data
#' @param data an object, produced with desc_occurance_rolling
#' @param title the title (optional)
#' @keywords sampling experience descriptives
#' @export
#' @examples
#' plot_occurance(dat1)
#'

plot_occurance_rolling <- function(data, title = ""){
  data2 <- data %>% gather(variable, value, -day)
  ggplot(data2, aes(day, value, group = variable, color = variable))+
    geom_line(size = 1)+ theme_minimal()+ scale_color_viridis_d()+
    labs(x = "Day", y="Occurances", color = "Category", title = title)
}

