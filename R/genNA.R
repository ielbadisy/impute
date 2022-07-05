#*****************************************
# MAR : set the variable values to NA (prop) conditional on another variable values

## if 
generate_MCAR <- function(x, p = 0.2, seed = 123) {
  
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



# MNAR : set the variable value to NA (prop) conditional on its own values

generate_MNAR <- function(x, p = 0.2, seed = 123) {
  
  stopifnot(p >= 0, p <= 1, is.atomic(x) || is.data.frame(x))
  
  set.seed(seed)  
  
  generate_na_vec <- function(z, p) {
    
    n <- length(z) 
    
    #z[sample(n, floor(p * n))] <- NA
    #z
   if (!is.atomic(z)){
     
   }
   sort.z = sort(z, decreasing = TRUE)
   nmar = sort.z[floor(p * n)]
   y.nmar = ifelse(z > nmar, NA, z) # does not show up when values are high
   y.nmar
  } 
  # vector or matrix
  if (is.atomic(x)) return(generate_na_vec(x, p))
  # data frame
  v <- if (is.null(names(p))) names(x) else intersect(names(p), names(x))
  x[, v] <- Map(generate_na_vec, x[, v, drop = FALSE], p)
  x
}

irisNA <- generate_MNAR(iris)
# MCAR : sample randomly from the complete dataset and set the sampled values to NA

#****************
createNAs <- function (x, pctNA = 0.1) {
n <- nrow(x)
p <- ncol(x)
NAloc <- rep(FALSE, n * p)
NAloc[sample.int(n * p, floor(n * p * pctNA))] <- TRUE
x[matrix(NAloc, nrow = n, ncol = p)] <- NA
return(x)
}
#**************
library(dplyr)
df %>%
  mutate(
    x2 = if_else(x1 == 1 & runif(n()) < .1, NA_real_, x2)
  )


