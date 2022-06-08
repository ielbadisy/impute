#' Title
#'
#' @param data generate with the substantive cox model either complete or imputed 
#' @param R number of replcations 
#' @param seed radnom number generation 
#' @param method 
#' @param scenario 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
evaluate_coxpred <- function(data, R = 5, seed = 1234, method = "fullcox", scenario = "MAR-10", ...) {
  
  #pacman::p_load(survival, SurvMetrics, survival, pec, ipred)
  
  set.seed(seed)
  N = nrow(data)
  dat = data
  
  #Initialization
  
  for (i in 1:R){
    index.train = sample(1:N,2/3*N)
    data.train = dat[index.train,]
    data.test = dat[-index.train,]
    dis_time = sort(data.train$time[data.train$event == 1])  #the default time points
    
    
    ## **** IBS 
    fitcox = survival::coxph(Surv(time, event)~., data = data.train, x = TRUE)
    mat_cox = pec::predictSurvProb(fitcox, data.test, dis_time)
    surv_obj = survival::Surv(data.test$time, data.test$event)
    
    IBS[i] = SurvMetrics::IBS(surv_obj, mat_cox, dis_time)
    IBS = as.numeric(unlist(IBS))
    
    
    #**** Cindex
    med_index = stats::median(1:length(dis_time))
    surv_obj = survival::Surv(data.test$time, data.test$event)
    
    Cindex[i] = SurvMetrics::Cindex(surv_obj, predicted = mat_cox[, med_index])
    Cindex = as.numeric(unlist(Cindex))
    
    #**********************************
    ## survAUC::AUC.cd()
    ### Chambless, L. E. and G. Diao (2006). Estimation of time-dependent area under the ROC curve for long-term risk prediction. Statistics in Medicine 25, 3474–3486
    
    fitcox = survival::coxph(Surv(time, event)~., data = data.train, x = TRUE)
    predcox <- predict(fitcox)
    predcoxnew <- predict(fitcox, newdata=data.test)
    surv_obj <- survival::Surv(data.train$time, data.train$event)
    surv_obj_new <- survival::Surv(data.test$time, data.test$event)
    
    AUC[i] = survAUC::AUC.cd(surv_obj, surv_obj_new, predcox, predcoxnew, dis_time)$iauc
    AUC = as.numeric(unlist(AUC))
    
    #****
    fitcox <- survival::coxph(Surv(time, event)~., data = data.train, x = TRUE)
    predcox <- predict(fitcox)
    predcoxnew <- predict(fitcox, newdata=data.test)
    surv_obj <- survival::Surv(data.train$time, data.train$event)
    surv_obj_new <- survival::Surv(data.test$time, data.test$event)
    
    predError[i] = predErr(surv_obj, surv_obj_new, predcox, predcoxnew, dis_time, type = "robust", int.type = "weighted")$ierror
    
    #*******************list results
    pred_metrics = data.frame(metrics = c('IBS', 'Cindex', 'AUC', 'predError'),
                              values = c(IBS, Cindex, AUC, predError),
                              method = rep(method, 4),
                              scenario = rep(scenario, 4),
                              iter = 1:R)
  }
  
  colnames(pred_metrics) <- c("metrics", "values", "method", "scenario", "iter")
  return(pred_metrics)
}