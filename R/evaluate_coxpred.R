#' Title
#'
#' @param seed radnom number generation 
#' @param ... 
#' @param dat 
#' @param myformula 
#' @param metric AUC, IBS or predError
#'
#' @return
#' @export
#'
#' @examples
evaluate_coxpred <- function(dat, myformula, metric, seed = 1234, ...) {
  #pacman::p_load(survival, SurvMetrics, survival, pec, ipred)
  # https://cran.r-project.org/web/packages/survAUC/survAUC.pdf
  set.seed(seed)
  N = nrow(dat)

  #Initialization
  
    index.train = sample(1:N,2/3*N)
    data.train = dat[index.train,]
    data.test = dat[-index.train,]
    dis_time = sort(data.train$time[data.train$event == 1])  #the default time points
    
    
    ## **** IBS 
    # Gerds, T. A. and M. Schumacher (2006). Consistent estimation of the expected Brier score in general survival models with right-censored event times.
    # Biometrical Journal 48, 1029–1040.
    
    fitcox <- survival::coxph(myformula, data = data.train, x = TRUE)
    predcox <- predict(fitcox)
    predcoxnew <- predict(fitcox, newdata=data.test)
    surv_obj <- survival::Surv(data.train$time, data.train$event)
    surv_obj_new <- survival::Surv(data.test$time, data.test$event)
    
    IBS = survAUC::predErr(surv_obj, surv_obj_new, predcox, predcoxnew, dis_time, type = "brier", int.type = "weighted")$ierror
    
    
    
    ##*** survAUC::AUC.cd()
    ### Chambless, L. E. and G. Diao (2006). Estimation of time-dependent area under the ROC curve for long-term risk prediction. Statistics in Medicine 25, 3474–3486
    
    fitcox = survival::coxph(myformula, data = data.train, x = TRUE)
    predcox <- predict(fitcox)
    predcoxnew <- predict(fitcox, newdata=data.test)
    surv_obj <- survival::Surv(data.train$time, data.train$event)
    surv_obj_new <- survival::Surv(data.test$time, data.test$event)
    
    AUC = survAUC::AUC.cd(surv_obj, surv_obj_new, predcox, predcoxnew, dis_time)$iauc
    #AUC = as.numeric(unlist(AUC))
    
    #****survAUC::preErr (absolute deviation between predicted and observed survival)
    # Schmid, M., T. Hielscher, T. Augustin, and O. Gefeller (2011).
    # A robust alter- native to the Schemper-Henderson estimator of prediction error. Biometrics 67, 524–535.
    
    fitcox <- survival::coxph(myformula, data = data.train, x = TRUE)
    predcox <- predict(fitcox)
    predcoxnew <- predict(fitcox, newdata=data.test)
    surv_obj <- survival::Surv(data.train$time, data.train$event)
    surv_obj_new <- survival::Surv(data.test$time, data.test$event)
    
    predError = survAUC::predErr(surv_obj, surv_obj_new, predcox, predcoxnew, dis_time, type = "robust", int.type = "unweighted")$ierror
    
    #**************** results
  #pred_metrics = data.frame(metrics = c('IBS', 'predError', 'AUC'), 
                              #values = c(IBS, predError,  AUC)
                              #)
  
    
   #colnames(pred_metrics) <- c("metrics", "values")
   #pred_metrics
   res <- switch(metric,
                 IBS = IBS,
                 AUC  = AUC,
                 predError = predError,
                 stop("metric ", metric, " is not implemented")
                 )
   res
}

#dat <- imputer::generate_cox(1000)
#myformula <- as.formula(Surv(time, event) ~ x1 + x2 + x3 + x4 + x5)
#evaluate_coxpred(dat, myformula, metric = "IBS")
