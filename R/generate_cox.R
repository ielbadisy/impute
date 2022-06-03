#-------------------generate_cox()  

##' Generate survival data with $beta$, $lambda_t$ baseline of censoring and $labmda_c$ hazard of censoring
##'
##' 
##'
##' @title Generate survival data from a Cox model
##' @param n Sample size
##' @param beta true beta
##' @param lambdaT baseline hazard
##' @param lambdaC hazard of censoring 
##' @param x covariate
##' @return data.frame with time (survival time), event(0 = censored), predictors (x_i)
##'         T (survival time), event (0: censored),
##'         
##' @references N.J. Horton and Ken K.Keinman, Using R and RStudio for Data Management and Statistical Analysis and Graphics, second edition, 2015

library(survival)

generate_cox <- function(n = 1000, beta = 0.6, lambdaT = 0.002, lambdaC = 0.004) {
  pacman::p_load(mice, survival)
  # covariates
  rho <- 0.3
  x1  <- rnorm(n, mean = 0, sd = 1)
  x2  <- (rho * x1) + sqrt(1 - rho*rho) * rnorm(n, mean = 0, sd = 1)
  x3 = 0.6 * (x1 + x2 - x1 * x2) + rnorm(n) # !!!!! derived variable from x1 and x3
  x4 = rbinom(n, 1, 0.5) # binary variable
  x5 = sample(c(1, 2, 3), n, replace= TRUE, prob = c(1/3, 1/3, 1/3)) # ordinal variale ????!!!!
  
  # true event time
  T = rweibull(n, shape=1, scale=lambdaT*exp(beta*x1 + beta*x2 + beta*x3 + beta*x4 + beta*x5))
  C = rweibull(n, shape=1, scale=lambdaC)
  
  # censoring time
  time = pmin(T,C) #observed time is min of censored and true
  censored = (time==C)
  
  # set to 1 if event is censored
  event = 1 - censored
  
  data <- data.frame(time, event, x1, x2, x3, x4, x5)
  
  # Observed marginal cumulative hazard for imputation model : Nelson-Altschuler- Aalen estimator
  
  data$cumhaz <- mice::nelsonaalen(data, time, event) # need a helper function  + problem with NA for some patients
  
  # final generated dataset
  data$x4 <- as.factor(data$x4)
  data$x5 <- as.factor(data$x5)
  return(data)
}


##-----test
#myformula <- as.formula(Surv(time, event) ~ x1 + x2 + x3 + x4 + x5)
#dat <- generate_cox(1000, 0.6, 0.0002, 0.0004)
#summary(survival::coxph(myformula, data = dat))
