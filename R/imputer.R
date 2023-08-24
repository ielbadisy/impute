
#' Impute mixed-type data with missing values using a specific algorithm
#'
#' @description add description  
#' @usage imputer(data_with_NA, "imputation_method")
#' @param data A data frame or matrix containing missing values
#' @param method Imputation algorithms wrapped in the imputer function : "naive", "hotdeck", "knn", "cart", "missranger", "missforest", "spmm", "famd", "mpmm", "micerf", "micecart", 
#' @return A single complete data set imputed with the chosen method.
#' \code{imputer} is a wrapper function that gives a direct use of the specified imputation method. All the available methods support mixed data type (continuous and categroical). 

#' \code{"naive"} imputation by mean for continuous and mode for categorical variables
#' \code{"hotdeck"}  single sequential hot deck imputation
#' \code{"knn"}  single imputation by k-nearest neighbor
#' \code{"cart"} single imputation by classification and regression trees
#' \code{"missforest"}  single nonparametric imputation using random forest algorithm
#' \code{"spmm"}  single predictive mean matching
#' \code{"famd"}  single imputation by factorial analysis for mixed-type
#' \code{"missranger"} single imputation  by chained random forests
#' \code{"misscforest"} single imputation  by ensemble conditional trees
#' \code{"mice"}  multiple imputation by chained equations 
#' \code{"mpmm"} multiple predictive mean matching
#' \code{"micerf"}  multiple Imputation by random forests
#' \code{"micecart"}  multiple  imputation by classification and regression # 
#' \code{"mixgb"}  multiple imputation by xgboost
#' 
#' @import simputation
#' @import missForest
#' @import missRanger
#' @import mice
#' @import missMDA
#' @import mixgb
#' @import missCforest
#' @export  
#' @importFrom stats median runif
imputer <- function(data, method = "naive") {
  
  dat <- data
  var <- names(data)
  
  m = c("naive", "hotdeck", "knn", "cart", "missforest", "spmm", "famd", "missranger", "misscforest", "mpmm", "mice", "micerf", "micecart", "mixgb", "complete")
  
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
  # single PMM
  if (method == "spmm") {
    
    impx <- mice::complete(mice::mice(dat, m = 1, method = "pmm", print = FALSE))
    return(impx) 
  }
  #-------------------------------------------------
  if (method == "famd") {
    
    impx <- missMDA::imputeFAMD(dat, ncp = 3)$completeObs
    return(impx) 
  }
  #-------------------------------------------------
  if (method == "misscforest") {
    suppressWarnings({
    impx <- missCforest::missCforest(dat, ntree = 10L)
    })
    return(impx) 
  }
  #-------------------------------------------------
  # Multiple PMM
  if (method == "mpmm") {
    impx <- mice::mice(dat, m = 10, method = "pmm", print = FALSE)
    #impx <- mice::complete(impx, action = "all")
    return(impx) 
  }
  #-------------------------------------------------
  if (method == "mice") {
    impx <- mice::mice(dat, m = 10, print = FALSE)
    #impx <- mice::complete(impx, action = "all") 
    return(impx) 
  }
  #-------------------------------------------------
  if (method == "micecart") {
    impx <- mice::mice(dat, m = 10, meth = "cart", minbucket = 4, print = FALSE)
    #impx <- mice::complete(impx, action = "all")
    return(impx) 
  }
  #-------------------------------------------------
  if (method == "micerf") {
    impx <- mice::mice(dat, m = 10, meth = "rf", ntree = 5, print = FALSE)
    #impx <- mice::complete(impx, action = "all")
    return(impx) 
  }
  #-------------------------------------------------
  if (method == "mixgb") {
    impx <- mixgb::mixgb(dat, m = 10, maxit = 5)
    return(impx) 
  }
  #-------------------------------------------------
  if (method == "complete") {
    impx <- dat[stats::complete.cases(dat), ]
    return(impx) 
  }
  ## Add more methods here ! 
}
