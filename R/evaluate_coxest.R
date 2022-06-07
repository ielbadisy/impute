#' Title
#'
#' @param estimate 
#' @param true_val 
#' @param metric 
#'
#' @return
#' @export
#'
#' @examples

evaluate_coxest <- function(estimate, true_val, metric = "bias") {
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
         nr_NA = sum(is.na(estimate)) # to control aberrant behaviour of NA
  )
}


# example
#eval_est(c(5, 5, 0.6), c(7, 0.6, 0.3), "MCe") 
