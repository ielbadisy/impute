# wrapper for Amelia::missmap(irisNA)

#' Title
#'
#' @param dataNA 
#'
#' @return
#' @export
#'
#' @examples
plot_miss <- function(dataNA){
  p <- naniar::vis_miss(dataNA)
  p
}


#library(naniar)
#vis_miss(airquality)
