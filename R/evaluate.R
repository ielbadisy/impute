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

#imputed_coxmodel(dat, myformula, method = "y", scenario = "x")

#*************************************************************************

# file:///home/elbadisy/Desktop/Phd/Project2-Simulation-MDI/6.mindmaps/target-parameters.html

# good utils here : https://github.com/torockel/missMethods/blob/master/R/utils-evaluation.R
# 
eval_est <- function(estimate, true_val, metric = "bias") {
  # https://github.com/torockel/missMethods/blob/master/R/utils-evaluation.R
  
  stopifnot(metric %in% c(
    "bias", "MCe", "RMSE", "sd_est", "bias_rel", "precision", "cor", "MAE", "MAE_rel", "nr_NA"))
  
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

# https://cran.r-project.org/web/packages/SurvMetrics/vignettes/SurvMetrics-vignette.html

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
           index_data = createFolds(1:nrow(mydata), 2)
           train_data = mydata[index_data[[1]],]
           test_data = mydata[index_data[[2]],]
           
           
           ## Cindex
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
      
           # *****************
           # if 
           ## IBS
        
           fitcox = coxph(myformula, data = train_data, x = TRUE)
           mat_cox = predictSurvProb(fitcox, test_data, dtimes)
             
           #calculate the IBS
           med_index = median(1:length(dtimes))
           surv_obj = Surv(test_data$time, test_data$event)
           
             
           #IBS for Cox
          metrics_cox = IBS(surv_obj, mat_cox, dtimes)

           
           
           
           #ggplot(data_CI, aes(x = model, y = Cindex, fill = model)) + geom_boxplot()
           
         }
         
  data_CI = data.frame('Cindex' = c(metrics_cox), 'model' = c(rep('Cox', length(metrics_cox))))
  return(data_CI)

}

eval_pred(dat, R = 15, "AUC")

#***************************************************Cindex
library(impute)
library(survival)
library(SurvMetrics)
set.seed
mydata <- generate_cox()
N <- nrow(dat)
myformula <- as.formula(Surv(time, event) ~ x1 + x2 + x3 + x4 + x5)

#index.train = sample(1:N,2/3*N)
#data.train = dat[index.train,]
#data.test = dat[-index.train,]

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


pred <- predict(fitcox, newdata=test_dat)

#C index for Cox

metrics_cox = Cindex(surv_obj, predicted = mat_cox[, med_index])

#********



#*************






# https://cran.r-project.org/web/packages/SurvMetrics/SurvMetrics.pdf
##  IBS 
# ipred::sbrier() : https://www.rdocumentation.org/packages/ipred/versions/0.9-9/topics/sbrier




#******************************OK 

# https://cran.r-project.org/web/packages/survAUC/survAUC.pdf

## predError : Distance-based estimators of survival predictive accuracy

data(cancer,package="survival")
TR <- ovarian[1:16,]
TE <- ovarian[17:26,]
train.fit <- survival::coxph(survival::Surv(futime, fustat) ~ age, x=TRUE, y=TRUE,
                             method="breslow", data=TR)
lp <- predict(train.fit)
lpnew <- predict(train.fit, newdata=TE)
Surv.rsp <- survival::Surv(TR$futime, TR$fustat)
Surv.rsp.new <- survival::Surv(TE$futime, TE$fustat)
times <- 1:500

predErr(Surv.rsp, Surv.rsp.new, lp, lpnew, times,
        type = "robust", int.type = "weighted")$ierror

### Schmid, M., T. Hielscher, T. Augustin, and O. Gefeller (2011). A robust alter- native to the Schemper-Henderson estimator of prediction error. Biometrics 67, 524–535


#*******************************OK 
## AUC.cd : 
### 

library(survAUC)
data(cancer,package="survival")
TR <- ovarian[1:16,]
TE <- ovarian[17:26,]
train.fit <- survival::coxph(survival::Surv(futime, fustat) ~ age,
                             x=TRUE, y=TRUE, method="breslow", data=TR)
lp <- predict(train.fit)
lpnew <- predict(train.fit, newdata=TE)
Surv.rsp <- survival::Surv(TR$futime, TR$fustat)
Surv.rsp.new <- survival::Surv(TE$futime, TE$fustat)
times <- seq(10, 1000, 10)
AUC.cd(Surv.rsp, Surv.rsp.new, lp, lpnew, times)$iauc

