#' A Test function
#'
#' This function allows you to test your package.
#' @param test Did you install the package by purpose? Defaults to TRUE.
#' @keywords test
#' @export
#' @examples
#' install_test()

install_test <- function(test=TRUE){
  if(test==TRUE){
    print("Success, you installed the package")
  }
  else {
    print("ok")
  }
}


