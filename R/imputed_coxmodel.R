
#' Title
#'
#' @param impcox 
#' @param myformula 
#' @param method 
#' @param scenario 
#' @param true_val 
#'
#' @return
#' @export
#'
#' @examples
imputed_coxmodel <- function(impcox, myformula, method = NULL, scenario = NULL, true_val = logBeta){
  # Full data analysis
  mod <- summary(survival::coxph(myformula, data = impcox))
  coefs <- as.data.frame(mod$coef)
  # return a data.frame of coefficients (est), upper and lower 95% limits
  out <- data.frame(est = coefs$coef,
                    lo95 = (coefs$coef + qnorm(0.025) * coefs$`se(coef)`),
                    hi95 = (coefs$coef + qnorm(0.975) * coefs$`se(coef)`),
                    row.names = row.names(coefs))
  out$CI_length <- out$hi95 - out$lo95 
  out$method <- rep(method, nrow(out))
  out$scenario <-rep(scenario, nrow(out))
  
  out$CI_coverage <- true_val >= out$lo95 & true_val <= out$hi95
  
  return(out)
  
}

# example
# #logBeta <- d$est
#imputed_coxmodel(dat, myformula, method = "y", scenario = "x")
