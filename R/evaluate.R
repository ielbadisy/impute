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
#*************************************************************************

#logBeta <- d$est

imputed_coxmodel <- function(impcox, myformula, method = "y", scenario = "x", true_val = logBeta){
  # Full data analysis
  mod <- summary(survival::coxph(myformula, data = impcox))
  c_index <- mod$concordance
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
  
  
  
  
  
  # estimation
  list(out, c_index)
  
}

#imputed_coxmodel(dat, myformula, method = "y", scenario = "x")

#*************************************************************************

# file:///home/elbadisy/Desktop/Phd/Project2-Simulation-MDI/6.mindmaps/target-parameters.html

# good utils here : https://github.com/torockel/missMethods/blob/master/R/utils-evaluation.R
# 
eval_est <- function(estimate, true_val, metric = "bias") {
  # https://github.com/torockel/missMethods/blob/master/R/utils-evaluation.R
  
  stopifnot(metric %in% c(
    "RMSE", "bias", "MCe", "bias_rel", "cor", "MAE", "MAE_rel", "MSE",
    "NRMSE_col_mean", "NRMSE_col_mean_sq", "NRMSE_col_sd",
    "NRMSE_tot_mean", "NRMSE_tot_mean_sq", "NRMSE_tot_sd", "nr_NA"))
  
  switch(metric,
         bias = mean(estimate - true_val),
         MCe = sd(bias)/sqrt(length(bias)), # se of bias (MC error)
         sd_est = sd(estimate),
         bias_rel =  mean((estimate - true_val) / abs(true_val)),
         precision = var(true_val)/var(estimate),
         cor = stats::cor(estimate, true_val),
         MAE = mean(abs(estimate - true_val)),
         MAE_rel = mean(abs(estimate - true_val) / abs(true_val)),
         nr_NA = sum(is.na(estimate))
         )
  }


# example
#eval_est(c(5, 5, 0.6), c(7, 0.6, 0.3), "MCe") 


#***********************************

eval_pred <- function(data, R = 10, method= "AUC") {
  
  library(SurvMetrics)
  library(caret)
  library(randomForestSRC)
  library(survival)  
  library(pec)
  library(ggplot2)
  set.seed(123)

  metrics_cox = 0
  mydata = data
  
  #Initialization
  
         for (i in 1:R) {
          
          # add statements for different models + metrics +++
           # https://stackoverflow.com/questions/31486174/r-an-if-else-statement-inside-a-for-loop
           
           index_data = createFolds(1:nrow(mydata), 2)
           train_data = mydata[index_data[[1]],]
           test_data = mydata[index_data[[2]],]
           
           # sort unique time points for prediction
           dtimes <- sort(unique(with(dat,time[event==1])))
           
           #fit the models
           fitcox = coxph(myformula, data = train_data, x = TRUE)
           mat_cox = predictSurvProb(fitcox, test_data, dtimes)
           
           #calculate the C index
           med_index = median(1:length(dtimes))
           surv_obj = Surv(test_data$time, test_data$event)
           
           #C index for Cox
           
           metrics_cox[i] = Cindex(surv_obj, predicted = mat_cox[, med_index])
          
           #ggplot(data_CI, aes(x = model, y = Cindex, fill = model)) + geom_boxplot()
           
         }
         
  data_CI = data.frame('Cindex' = c(metrics_cox), 'model' = c(rep('Cox', length(metrics_cox))))
  return(data_CI)

}

#eval_pred(dat, R = 15, "AUC")
