#'A generell sample function for the creation of Experience Sampling Data
#'
#' This function allows you to test sample experience data.
#' @param n_participants Number of Participants. Default = 100
#' @param n_days Number of Days. Default = 7
#' @param art_breaks Specification at which value and event occurs. Between 0 ans 1. Default = 0.20
#' @keywords sampling experience
#' @export
#' @examples
#' data_sample <-  sample_ex_data(50, 10)


sample_ex_data <- function(n_participants = 100, n_days = 7, art_breaks = 0.20){
  library(emaph)
  sample_plan <- sample_plan(n_participants, n_days)
  dat <- mm_par_a <- list(
    fixed  = c(intercept = 0,
               time      = 0),
    random = c(intercept = 0.1,
               time      = 0.005),
    error  = 0.2,
    phi    = 0.5
  )
  dat <- sim_ema(plan = sample_plan,
                 mm_par = mm_par_a) %>% tibble::as.tibble()
  group <- c(rep(1, (nrow(dat)/2)), rep(2, (nrow(dat)/2)))
  Y <- ifelse(dat$Y < art_breaks, 0, 1)
  dat$group <- group
  dat$Y <- Y
  return(dat)
}
