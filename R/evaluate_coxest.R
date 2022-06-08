#' Title
#'
#' @param estimate 
#' @param truelogHR 
#' @param metric 
#'
#' @return
#' @export
#'
#' @examples

evaluate_coxest <- function(estimate, truelogHR, metric = "bias") {
  # https://github.com/torockel/missMethods/blob/master/R/utils-evaluation.R
  
  stopifnot(metric %in% c(
    "bias", "MCe", "RMSE", "sd_est", "bias_rel", "precision", "cor", "MAE", "MAE_rel", "nr_NA"))
  
  switch(metric,
         bias = mean(estimate - truelogHR),
         MCe = sd(bias)/sqrt(length(bias)), # se of bias (MC error)
         sd_est = sd(estimate),
         bias_rel =  mean((estimate - truelogHR) / abs(truelogHR)),
         precision = var(truelogHR)/var(estimate),
         cor = stats::cor(estimate, truelogHR),
         MAE = mean(abs(estimate - truelogHR)),
         MAE_rel = mean(abs(estimate - truelogHR) / abs(truelogHR)),
         nr_NA = sum(is.na(estimate)) # to control aberrant behaviour of NA
  )
}


# example
#eval_est(c(5, 5, 0.6), c(7, 0.6, 0.3), "MCe") 
