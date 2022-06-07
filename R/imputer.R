
#' impute variables with missing data values using a specific algorithm
#'
#' @description 
#' @usage imputer(dataNA, "method")
#' @param data A data frame or matrix containing missing values
#' @param method Imputation algorithms wrapped in the imputer function : "naive", "hotdeck", "knn", "cart", "glmnet", "missranger", "missforest", "spmm", "famd", "mpmm", "micerf", "supermice".
#'
#' @return A single complete data set imputed with the chosen method.
#' @details 
#' code{imputer} is a wrapper function that gives a direct use of the specified imputation method. All the available methods support mixed data type (continuous and categroical). 
#' Two types of methods exist :
#' - univariate imputation methods : "naive", "hotdeck", "knn", "cart", "glmnet", "missforest", "spmm", "famd", 
#'
#' \tabular{111}{
#' \code{"naive"}  \tab Mean for continuous and Mode for categorical variables
#' \code{"hotdeck"}  \tab Sequential hot deck imputation
#' \code{"knn"}  \tab K-nearest neighbour imputation
#' \code{"cart"}  \tab rpart algorithm
#' \code{"glmnet"}  \tab ridge/elasticnet/lasso regression
#' \code{"missforest"}  \tab Nonparametric imputation using Random Forest algorithm
#' \code{"spmm"}  \tab Single Predictive Mean Matching
#' \code{"famd"}  \tab Factorial Analysis for Mixed data imputation
#' \code{"mpmm"}  \tab Multiple Predictive Mean Matching
#' \code{"micerf"}  \tab Multiple Imputation by random forests
#' \code{"missRanger"}  \tab Fast Imputation  by Chained Random Forests
#' \code{"supermice"}  \tab SuplerLearner ensemble method based combined with {mice} approach


#' @references 
#' Add references here !!!
#' }
#' @export
#' @examples
imputer <- function(data, method = "naive") {
  
  dat <- data
  var <- names(data)
  
  m = c("naive", "hotdeck", "knn", "cart", "glmnet", "missforest", "spmm", "famd", "missranger", "mpmm", "micerf", "supermice")
  
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
