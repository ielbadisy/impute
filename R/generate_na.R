#***************************************************************************
generate_na <- function(x, p = 0.2, seed = 123) {
  
  stopifnot(p >= 0, p <= 1, is.atomic(x) || is.data.frame(x))
  
  set.seed(seed)  
  
  generate_na_vec <- function(z, p) {
    
  n <- length(z) 
  
  z[sample(n, floor(p * n))] <- NA
  z
  } 
  
  # vector or matrix
  if (is.atomic(x)) return(generate_na_vec(x, p))
  
  # data frame
  
  v <- if (is.null(names(p))) names(x) else intersect(names(p), names(x))
  x[, v] <- Map(generate_na_vec, x[, v, drop = FALSE], p)
  x
  
  }

#***************************************************************************

# see here : https://cran.r-project.org/web/packages/naniar/readme/README.html
#*******************************************************
# ref : https://hal.archives-ouvertes.fr/hal-02879337/file/R-miss-tastic_arXiv_v3.pdf 
# vignette : https://cran.r-project.org/web/packages/missMethods/vignettes/Generating-missing-values.html
# github : https://github.com/cran/CoImp/blob/master/R/MCAR.R
# explaining the ampute function : https://hal.archives-ouvertes.fr/hal-02879337/file/R-miss-tastic_arXiv_v3.pdf
# generate MAR : https://stackoverflow.com/questions/67104293/how-to-generate-missing-at-random-mar-data-for-specific-gender-using-r-from
# generate MCAR : 
# https://stackoverflow.com/questions/50528719/simulate-data-and-randomly-add-missing-values-to-dataframe
# Good place to start : https://cran.r-project.org/web/packages/missMethods/vignettes/Generating-missing-values.html ++++

#**************************************

# see later for the nice description here : 
# it a slightly modified version of the mice::ampute. The underlying mechanisms remaind the same but the proportions of NA are fixed
#*****************************************************************


# CODE NEED  TO BE ClEANED UP & DOCUMENTATION NEED TO BE CHANGED !!!

#****************************************************************

#' generation of missing values on complete or incomplete data according to different missingness mechanisms and patterns
#' 
#' @param data [data.frame, matrix] (mixed) data table (n x p)
#' @param mechanism [string] either one of "MCAR", "MAR", "MNAR"; default is "MCAR"
#' @param self.mask [string] either NULL or one of "sym", "upper", "lower"; default is NULL
#' @param perc.missing [positive double] proportion of missing values, between 0 and 1; default is 0.5
#' @param idx.incomplete [array] indices of variables to generate missing values for; if NULL then missing values in all variables are possible; default is NULL
#' @param idx.covariates [matrix] binary matrix such that entries in row i that are equal to 1 indicate covariates that incluence missingness of variable i (sum(idx.incomplete) x p); if NULL then all covariates contribute; default is NULL
#' @param weights.covariates [matrix] matrix of same size as idx.covariates with weights in row i for contribution of each covariate to missingness model of variable i; if NULL then a (regularized) logistic model is fitted; default is NULL
#' @param by.patterns [boolean] generate missing values according to (pre-specified) patterns; default is FALSE
#' @param patterns [matrix] binary matrix with 1=observed, 0=missing (n_pattern x p); default is NULL
#' @param freq.patterns [array] array of size n_pattern containing desired proportion of each pattern; if NULL then mice::ampute.default.freq will be called ; default is NULL
#' @param weights.patterns [matrix] weights used to calculate weighted sum scores (n_pattern x p); if NULL then mice::ampute.default.weights will be called; default is NULL
#' @param use.all [boolean] use all observations, including incomplete observations, for amputation when amputing by patterns (only relevant if initial data is incomplete and by.pattern=T); default is FALSE
#' @param logit.model [string] either one of "RIGHT","LEFT","MID","TAIL"; default is "RIGHT"
#' @param seed [natural integer] seed for random numbers generator; default is NULL
#' 
#' @return A list with the following elements
#' \item{data.init}{original data.frame}
#' \item{data.incomp}{data.frame with the newly generated missing values, observed values correspond to the values from the initial data.frame}
#' \item{idx_newNA}{a boolean data.frame indicating the indices of the newly generated missing values}
#'
#' @export
#' @import mice mltools gdata LiblineaR glmnet dplyr


#taken of here : https://raw.githubusercontent.com/R-miss-tastic/website/master/static/how-to/generate/amputation.R
# Credit go to : https://www.imkemayer.com/




