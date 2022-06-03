# wrapper for Amelia::missmap(irisNA)

plot_miss <- function(dataNA){
  p <- naniar::vis_miss(dataNA)
  p
}


#library(naniar)
#vis_miss(airquality)
