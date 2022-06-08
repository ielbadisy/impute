#' Title
#'
#' @param data generated with the substantive cox model either complete or imputed 
#' @param myformula 
#' @param method 
#' @param scenario 
#' @param ... 

#'
#' @return
#' @export
#'
#' @examples
evaluate_coxpred <- function(data, myformula, method = "fullcox", scenario = "MAR-10", ...) {
  
  #pacman::p_load(survival, SurvMetrics, survival, pec, ipred)
  
  data = generate_cox(1000)
  N = nrow(data)
  dat = data
  
  #Initialization
  
  index.train = sample(1:N,2/3*N)
  data.train = dat[index.train,]
  data.test = dat[-index.train,]
  dis_time = sort(data.train$time[data.train$event == 1])  #the default time points
  
  
  fitcox = survival::coxph(myformula, data = data.train, x = TRUE)
  predcox <- predict(fitcox)
  predcoxnew <- predict(fitcox, newdata=data.test)
  surv_obj <- survival::Surv(data.train$time, data.train$event)
  surv_obj_new <- survival::Surv(data.test$time, data.test$event)
  
  #**** concordance inex for cox models : Gonen and Heller’s Concordance Index for Cox proportional hazards models
  #  The results obtained from GHCI are valid as long as lpnew is the predictor of a correctly specified
  # Cox proportional hazards model. In this case, the estimator remains valid even if the censoring
  # times depend on the values of the predictor.
  # ref : Gonen, M. and G. Heller (2005). Concordance probability and discriminatory power in proportional hazards regression. Biometrika 92, 965–970.
  Cprob = survAUC::GHCI(predcoxnew)
  
  #**** survAUC::AUC.cd()
  ### Chambless, L. E. and G. Diao (2006). Estimation of time-dependent area under the ROC curve for long-term risk prediction. Statistics in Medicine 25, 3474–3486
  AUC = survAUC::AUC.cd(surv_obj, surv_obj_new, predcox, predcoxnew, dis_time)$iauc
  AUC = as.numeric(unlist(AUC))
  
  #****predERROR : absolute deviation between predicted and observed survival : Schmid, M., T. Hielscher, T. Augustin, and O. Gefeller (2011). A robust alter- native to the Schemper-Henderson estimator of prediction error. Biometrics 67, 524–535.
  # https://cran.r-project.org/web/packages/survAUC/survAUC.pdf
  predError = survAUC::predErr(surv_obj, surv_obj_new, predcox, predcoxnew, dis_time, type = "robust", int.type = "weighted")$ierror
  
  
  #****predERROR : Integrated brier score
  IBS = survAUC::predErr(surv_obj, surv_obj_new, predcox, predcoxnew, dis_time, type = "brier", int.type = "weighted")$ierror
  
  #*******************list results
  pred_metrics = data.frame(metrics = c('Cprob', 'AUC', 'predError', 'IBS'),
                            values = c(Cprob, AUC, predError, IBS),
                            method = rep(method, 4),
                            scenario = rep(scenario, 4))
  
  
  colnames(pred_metrics) <- c("metrics", "values", "method", "scenario")
  return(pred_metrics)
  
}

#dat <- generate_cox(1000)
#myformula <- as.formula(Surv(time, event) ~ x1 + x2 + x3 + x4 + x5)
#evaluate_coxpred(dat, myformula, method = "fullcox", scenario = "MAR-10")

