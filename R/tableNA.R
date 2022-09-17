#' Title
#'
#' @param data
#' @param order
#' @param cumsum
#'
#' @return value
#' @export
#'
#' @examples
tableNA <- function(data, order = TRUE, cumsum = FALSE) {
  
  col_NA <- colSums(is.na(data))
  pct_col_NA <- colMeans(is.na(data)) * 100
  
  out <- data.frame(Variabes = names(col_NA),
                    number_NA = as.integer(col_NA),
                    percetnage_NA = as.numeric(pct_col_NA))
  
  if (cumsum) {
    out$n_NA_cumsum <- cumsum(out$number_NA)
  }
  
  if (order) {
    return(out[order(-out$number_NA), ])
  }
  
  return(out)
  
}
