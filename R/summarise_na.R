
#' Title
#'
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
#' #----------- summarise_na() : summarise NA by cases and variables (wrapper for naniar::miss_var_summary & naniar::miss_case_summary)

summarise_na <- function(data) {
  
  stopifnot(is.data.frame(data))
  
  s <- list(
    
    by_var = naniar::miss_var_summary(data),
    by_case = naniar::miss_case_summary(data)
    
  )
  
  return(s)
}


#summarise_na(iris)

