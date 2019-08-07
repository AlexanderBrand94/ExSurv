#' compare_fits_multiple
#'
#' This function fits multiple survival models for experience sampling data for further analysis
#'into a tidy dataframe
#' @param formula_in the input formula, has to be created with formula()
#' @param data the used dataframe for the survival analysis
#' @keywords preprocessing
#' @export
#' @examples
#' preprocess_exp_data


compare_fits_multiple <- function(formula_in, data = dat){
  model = fit.models(formula= formula_in, data = data,
                   distr=c("exponential","gompertz","weibull",
                           "weibullPH","loglogistic","lognormal"))
  list_model <- model$models %>% lapply(summary) %>% lapply(as.data.frame)
  list_model <- dplyr::bind_rows(list_model)
  list_model$model <- rep(c("exponential","gompertz","weibull",
                          "weibullPH","loglogistic","lognormal"), each = nrow(list_model)/6)
  ggplot2::ggplot(list_model, aes(time, est, color = as.factor(model))) +
    geom_ribbon(size = 1, aes(ymin = lcl, ymax = ucl,
                              color = NA, fill = as.factor(model)), alpha = 0.3)+ geom_line() +
    labs(color = "Model", x = "time", y = "survival", fill = "Model")+ theme_minimal() +
    scale_colour_viridis_d()+ scale_fill_viridis_d()+ geom_hline(yintercept = 0.5, linetype ="dashed")
  }







