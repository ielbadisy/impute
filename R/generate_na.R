
#' Title
#'
#' @param x 
#' @param p 
#' @param seed 
#'
#' @return
#' @export
#'
#' @examples
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



