# main function of the package  

#' @title impute data set with a specific method 
#' @export
#' 
#' @param x a data frame containing missing values (pas plus que 50%)
#'
#' @return z complete data frame with missing values imputed regarding the choosen method
#'
#' @details computes the square of x, in a slightly more efficient manner than
#'   \code{x ^ 2}
#' @examples
#' x <- variable(dim = c(3, 3))
#' y <- square(x)
#' 
#' 

#**************************************************
#* https://github.com/markvanderloo/simputation/blob/master/pkg/R/multivariate.R
#* https://github.com/stekhoven/missForest/blob/master/R/prodNA.R
#* 
#* see the mindpam here : file:///home/elbadisy/Desktop/Phd/Project2-Simulation-MDI/6.mindmaps/target-parameters.html
#**************************************************

impute <- function(data, method = "naive") {
  
  dat <- data
  var <- names(data)
  
  m = c("naive", "hotdeck", "knn", "cart", "glmnet", "missranger", "missforest", "spmm", "famd", "mpmm", "micerf", "supermice", "amelia")
  
  stopifnot(is.data.frame(data))
  
  stopifnot(method %in% m)
  #------------------------------------------------
  if (method == "naive"){
    # naive = "mean/mode"
    # code taken from ramhiser example : https://gist.github.com/ramhiser/4729c049aa0c6bd3c862
    
    impute_naive <- function(x, draw=FALSE) {
      x <- as.data.frame(x)
      
      cols_imputed <- lapply(x, function(col) {
        which_na <- which(is.na(col))
        num_na <- length(which_na)
        
        if (is.numeric(col)) {
          if (!draw) {
            col <- replace(col, is.na(col), median(col, na.rm=TRUE))
          } else {
            col_range <- range(col, na.rm=TRUE)
            vals_imputed <- runif(n=num_na, min=col_range[1], max=col_range[2])
            col <- replace(col, which_na, vals_imputed)
          }
        } else {
          col_levels <- levels(col)
          if (!draw) {
            col_mode <- col_levels[which.max(table(col))]
            col <- replace(col, is.na(col), col_mode)
          } else {
            col_proportions <- prop.table(table(col))
            vals_imputed <- sample(length(col_levels), num_na, prob=col_proportions, replace=TRUE)
            col <- replace(col, which_na, col_levels[vals_imputed])
          }
        }
        
        col
      })
      
      do.call(cbind.data.frame, cols_imputed)
    }
    impx <- impute_naive(dat)
    
    return(impx)
  }
  #-------------------------------------------------
  if (method == "hotdeck") {
    
  impx <- simputation::impute_shd(dat, .~1, backend="VIM")
  return(impx)
  }
  #-------------------------------------------------
  if (method == "knn") {
   
  impx <- VIM::kNN(dat) 
  return(impx[var])
  }
  
  #-------------------------------------------------
  if (method == "cart") {
    
  impx <- simputation::impute_cart(dat, .~.)
  return(impx)
  }
  
  
  #-------------------------------------------------
  if (method == "missforest") {
   
  impx <- missForest::missForest(dat, xtrue = dat, verbose = FALSE)$ximp
  return(impx) 
  }
  
  #-------------------------------------------------
  if (method == "missranger") {
    
    impx <- missRanger::missRanger(dat, pmm.k = 5, num.trees = 100, verbose = 0)
    return(impx) 
  }
  
  #-------------------------------------------------
  if (method == "spmm") {
    
    impx <- mice::complete(mice::mice(dat, m = 1, method = "pmm"))
    return(impx) 
  }
  
  #-------------------------------------------------
  if (method == "mpmm") {
    
    impx <- mice::complete(mice::mice(dat, m = 10, method = "pmm", print = FALSE))
    return(impx) 
  }
  
  #-------------------------------------------------
  if (method == "famd") {
    
    impx <- missMDA::imputeFAMD(dat, ncp = 3)$completeObs
    return(impx) 
  }
  
  #-------------------------------------------------
  if (method == "micerf") {
    
    impx <- mice::complete(mice::mice(dat, meth = "rf", ntree = 5, print = FALSE))
    return(impx) 
  }
  
  #-------------------------------------------------

  
  # add Amelia & mi : https://github.com/Tirgit/missCompare/blob/master/R/impute_data.R

}
