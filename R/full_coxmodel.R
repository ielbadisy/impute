#' Title
#'
#' @param fullcox 
#' @param myformula 
#' @param scenario 
#' @param method 
#'
#' @return
#' @export
#'
#' @examples
full_coxmodel <- function(fullcox, myformula, scenario = "baseline", method = "fullcox"){
  
  
  #need to check for arguments!!
  # Full data analysis
  coefs <- as.data.frame(summary(coxph(myformula, data = fullcox))$coef)
  # return a data.frame of coefficients (est), upper and lower 95% limits
  out <- data.frame(est = coefs$coef,
                    lo95 = (coefs$coef + qnorm(0.025) * coefs$`se(coef)`),
                    hi95 = (coefs$coef + qnorm(0.975) * coefs$`se(coef)`),
                    row.names = row.names(coefs))
  
  out$method <- rep(method, nrow(out))
  out$scenario <-rep(scenario, nrow(out))
  
  #out$cover <- kLogHR >= out$lo95 & kLogHR <= out$hi95
  out
}



# Example 

#d <- full_coxmodel(dat, myformula, scenario = "baseline", method = "fullcox")