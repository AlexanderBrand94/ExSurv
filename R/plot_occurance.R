#'A function for the plotting of occurances in experience sampling data
#'
#' This function produces a plot of occurances of experience sampling data, facetted by ID
#' @param data the cleared dataframe
#' @param title the title (optional)
#' @keywords sampling experience descriptives
#' @export
#' @examples
#' plot_occurance(dat1)
#'

plot_occurance <- function(data, title = ""){
  obs_day <- data %>% dplyr::distinct(beep) %>% nrow()
  data %>% dplyr::group_by(id, day) %>%
    dplyr::summarize_all(.funs = "mean") %>%
    ggplot(aes(time,
               Y*obs_day,
               group = id,
               color = factor(id)))+
    geom_line(size = 1) +
    facet_wrap(~id)+
    scale_color_viridis_d()+
    labs(
      color = "Person",
      title = title,
      y = "Occurrance per Days",
      x = "Time"
    )+
    theme_minimal()+ geom_smooth(alpha = 0.1)
}
