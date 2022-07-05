#-------------------gencox() 

##' Generate survival data with $beta$, $lambda_t$ baseline of censoring and $labmda_c$ hazard of censoring
##' @title Generate survival data from a Cox model
##' 
##' @param N Sample size
##' @param beta true beta vector 
##' @param lambda baseline hazard (scale parameter in h0)
##' @param rateC rate parameter of the exponential distribution of Censoring
##' @param rho scale parameter
##' 
##' @return data.frame with time (survival time), status(1 = censored), predictors (x_i)
##'         time(survival time), cumhaz (cumulative hazad),
##' @export 
##' @references N.J. Horton and Ken K.Keinman, Using R and RStudio for Data Management and Statistical Analysis and Graphics, second edition, 2015

gencox <- function(N = 1000, beta = rep(0.5,5), lambda = 0.002, rateC = 0.004,  rho = 0.003, seed = 123) {
  
  pacman::p_load(mice, survival)
  set.seed(seed)
  # covariates
  x1 = rnorm(N)
  x2 = rnorm(N)
  x3 = 0.5 * (x1 + x2) + rnorm(N) # derived variable from x1 and x3
  x4 = rbinom(N, 1, 0.4) # binary variable
  x5 = sample(c(1, 2, 3), N, replace= TRUE, prob = c(1/2, 1/4, 1/4)) # ordinal variale ????!!!!
  
  # latent event times   Weibull
  v <- runif(n = N)
  
  #  estimated survival times 
  Tlat <- (- log(v) / (lambda * exp(beta[1]*x1 + beta[2]*x2 + beta[3]*x3 + beta[4]*x4 + beta[5]*x5)))^(1/rho)
  
  # censoring times 
  C <- rexp(N, rate = rateC) 
  
  # follow-up times and event indicators
  #observed time is min of censored and true
  time <- pmin(Tlat, C) 
  status <- as.numeric(Tlat <= C)
  
  # data set
  data <- data.frame(time, status, x1, x2, x3, x4, x5)
  
  # Observed marginal cumulative hazard for imputation model : Nelson-Altschuler- Aalen estimator
  data$cumhaz <- mice::nelsonaalen(data, time, status) # need a helper function  + problem with NA for some patients
  
  # final generated dataset
  #data$x4 <- as.factor(data$x4)
  #data$x5 <- as.factor(data$x5)
  return(data)
}


##-----test

#set.seed(123)
#R = 1000
#myformula <- as.formula(Surv(time, status) ~ x1 + x2 + x3 + x4 + x5)
#betaHat <- rep(0.5, 5)
#beta <- rep(NA, R)
#for (k in 1:R){
  #dat <- gencox(N = 1000, beta = betaHat, rho = 1, lambda = 0.01, rateC = 0.003)
  #fit <- survival::coxph(myformula, data = dat)
  #beta[k] <- fit$coef
#}
#mean(beta)